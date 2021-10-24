module Brace.ArgParser.Parser (Matches, parse, tests) where

import Test.HUnit

import Brace.ArgParser.Description

import qualified Data.Map as Map

import Data.List

data Parser
    = Parser
      { description :: Description
      , input :: [String]
      , matches :: Matches
      , positionals :: [(ArgProps, TakesValueProps)]
      }

data ParseError
    = InvalidValue String
    | InvalidFlag String
    | ExpectedValue
    | NoNameFlag
    | ExcessArguments

data Matches = Matches
    { positional_results :: (Map.Map String [String])
    , flags_present :: [Char]
    , option_results :: (Map.Map Char [String])
    }

first_str :: Parser -> Maybe String
first_str (Parser { input = x:_ }) = Just x
first_str (Parser { input = [] }) = Nothing

first_ch :: Parser -> Maybe Char
first_ch (Parser { input = (x:_):_ }) = Just x
first_ch (Parser { input = _ }) = Nothing

uncons_input :: Parser -> Maybe (String, [String])
uncons_input = uncons . input

current_positional :: Parser -> Maybe (ArgProps, TakesValueProps)
current_positional (Parser { positionals = p:_ }) = Just p
current_positional (Parser { positionals = [] }) = Nothing

record_positional_match :: String -> [String] -> Parser -> Parser
record_positional_match n vs parser@Parser { matches = m } =
    let results = Map.findWithDefault [] n (positional_results m)
        m' = m { positional_results = Map.insert n (results ++ vs) (positional_results m) }
    in parser { matches = m' }

record_flag_presence :: Char -> Parser -> Parser
record_flag_presence c parser@Parser { matches = m } =
    let m' = m { flags_present = c : flags_present m }
    in parser { matches = m' }

record_option_match :: Char -> [String] -> Parser -> Parser
record_option_match n vs parser@Parser { matches = m } =
    let results = Map.findWithDefault [] n (option_results m)
        m' = m { option_results = Map.insert n (results ++ vs) (option_results m) }
    in parser { matches = m' }

drop_first_str :: Parser -> Parser
drop_first_str p@(Parser { input = [] }) = p
drop_first_str p@(Parser { input = _:more_strs }) = p { input = more_strs }

drop_first_ch :: Parser -> Parser
drop_first_ch p@(Parser { input = (_:more_chs):more_strs }) = p { input = more_chs:more_strs }
drop_first_ch p@(Parser { input = []:_ }) = p
drop_first_ch p@(Parser { input = [] }) = p

drop_first_str_if_empty :: Parser -> Parser
drop_first_str_if_empty parser
    | first_str parser == Just "" = drop_first_str parser
    | otherwise = parser

consume_until_char_or_end :: Char -> Parser -> (String, Parser)
consume_until_char_or_end char parser =
    case uncons_input parser of
        Nothing -> ("", parser)

        Just (f, more_strs) ->
            let (consumed, left) = break (==char) f
            in (consumed, parser { input = left : more_strs })

parse :: Description -> [String] -> Either ParseError Matches
parse desc args =
    let parser =
            Parser
            { description = desc
            , input = args
            , matches = (Matches Map.empty [] Map.empty)
            , positionals = get_positionals desc
            }

        parser' = parse' parser

        parse' p
            | null $ input p = Right p
            | otherwise = parse_arg p >>= parse'

    in matches <$> parser'

parse_arg :: Parser -> Either ParseError Parser
parse_arg parser =
    case drop_first_str_if_empty parser of
        parser'@(Parser { input = [] }) -> Right parser'

        parser'
            | first_ch parser' == Just '-' -> parse_flag $ drop_first_ch parser'
            | otherwise -> parse_positional parser'

parse_flag :: Parser -> Either ParseError Parser
parse_flag parser
    | first_ch parser == Just '-' = parse_long_flag $ drop_first_ch parser
    | otherwise = parse_short_flag $ drop_first_ch parser

parse_long_flag :: Parser -> Either ParseError Parser
parse_long_flag parser =
    let (flag_name, parser') = consume_until_char_or_end '=' parser
    in
    if null flag_name
        then Left NoNameFlag
        else
            case get_flag (Right flag_name) (description parser') of
                Nothing -> Left (InvalidFlag $ "--" ++ flag_name)
                Just (FlagProps (ShortFlagName flag_ch) _, _, Just (TakesValueProps _ n_values)) ->
                    parse_values n_values parser' >>= \ (values, parser'') ->
                    let parser''' = record_option_match flag_ch values parser''
                    in Right $ parser'''

                Just (FlagProps (ShortFlagName flag_ch) _, _, Nothing) ->
                    Right $ record_flag_presence flag_ch parser'

parse_short_flag :: Parser -> Either ParseError Parser
parse_short_flag parser
    | first_ch parser == Nothing = Right parser
    | otherwise =
        maybe (Left NoNameFlag) Right (first_ch parser) >>= \ flag_name ->
        case get_flag (Left flag_name) (description parser) of
            Nothing -> Left (InvalidFlag $ ['-', flag_name])

            Just (FlagProps (ShortFlagName flag_ch) _, _, Just (TakesValueProps _ n_values)) ->
                parse_values n_values (drop_first_ch parser) >>= \ (values, parser') ->
                Right $ record_option_match flag_ch values parser'

            Just (FlagProps (ShortFlagName ch) _, _, Nothing) ->
                let parser' = record_flag_presence ch parser
                in parse_short_flag (drop_first_ch parser')

parse_positional :: Parser -> Either ParseError Parser
parse_positional parser =
    case current_positional parser of
        Just (_, (TakesValueProps (ValueName v_name) n_values)) ->
            parse_values n_values parser >>= \ (values, parser') ->
            Right $ record_positional_match v_name values parser'

        Nothing -> Left ExcessArguments

parse_values :: NValues -> Parser -> Either ParseError ([String], Parser)
parse_values (Number n) parser = parse_n_values [] n parser
parse_values ZeroOrMore parser = parse_zero_or_more [] parser
parse_values OneOrMore parser =
    case parse_zero_or_more [] parser of
        Right ([], _) -> Left ExpectedValue
        o -> o

parse_zero_or_more :: [String] -> Parser -> Either ParseError ([String], Parser)
parse_zero_or_more acc parser =
    case parse_value_once parser of
        Nothing -> Right (acc, parser)
        Just (v, parser') -> parse_zero_or_more (acc ++ [v]) parser'

parse_n_values :: [String] -> Int -> Parser -> Either ParseError ([String], Parser)
parse_n_values acc 0 parser = Right (acc, parser)
parse_n_values acc n parser =
    case parse_value_once parser of
        Nothing -> Left ExpectedValue
        Just (v, parser') -> parse_n_values (acc ++ [v]) (n - 1) parser'

parse_value_once :: Parser -> Maybe (String, Parser)
parse_value_once p =
    case first_str p of
        Nothing -> Nothing
        Just ('-':_) -> Nothing
        Just s -> Just (s, drop_first_str p)

tests :: Test
tests = TestList []
