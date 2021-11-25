{-# LANGUAGE TemplateHaskell #-}

module UHF.ArgParser.Parser
    ( Matches(..)
    , ParseError(..)
    , parse
    , str_error

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import UHF.ArgParser.Description

import qualified Data.Map as Map

import Data.List

data Parser
    = Parser
      { description :: Description
      , input :: [String]
      , matches :: Matches
      , positionals :: [(ArgProps, TakesValueProps)]
      }
    deriving (Show, Eq)

-- TODO: better error messages for this
data ParseError
    = InvalidValue String
    | InvalidFlag String
    | ExpectedValue
    | NoNameFlag
    | ExcessArguments
    | UnexpectedValue
    deriving (Show, Eq)

data Matches =
    Matches
    { positional_results :: (Map.Map String [String])
    , flags_present :: [Char]
    , option_results :: (Map.Map Char [String])
    }
    deriving (Show, Eq)

empty_matches :: Matches
empty_matches = Matches Map.empty [] Map.empty

make_parser :: Description -> [String] -> Parser
make_parser d i =
    Parser
    { description = d
    , input = i
    , matches = empty_matches
    , positionals = get_positionals d
    }

str_error :: ParseError -> String
str_error (InvalidValue val) = "invalid value: '" ++ val ++ "'"
str_error (InvalidFlag fl) = "invalid flag: '" ++ fl ++ "'"
str_error (ExpectedValue) = "expected a value"
str_error (NoNameFlag) = "flag with no name"
str_error (ExcessArguments) = "too many arguments"
str_error (UnexpectedValue) = "unexpected value"

uncons_input :: Parser -> Maybe (String, [String])
uncons_input = uncons . input

uncons_positionals :: Parser -> Maybe ((ArgProps, TakesValueProps), [(ArgProps, TakesValueProps)])
uncons_positionals = uncons . positionals

first_str :: Parser -> Maybe String
first_str (Parser { input = x:_ }) = Just x
first_str (Parser { input = [] }) = Nothing

first_ch :: Parser -> Maybe Char
first_ch (Parser { input = (x:_):_ }) = Just x
first_ch (Parser { input = _ }) = Nothing

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

                left' = case left of
                    (_:more) -> more
                    _ -> left

            in (consumed, parser { input = left' : more_strs })

parse :: Description -> [String] -> Either ParseError Matches
parse desc args =
    let parser = make_parser desc args
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
    | otherwise = parse_short_flag parser

parse_long_flag :: Parser -> Either ParseError Parser
parse_long_flag parser =
    let (flag_name, parser') = consume_until_char_or_end '=' parser
    in if null flag_name
        then Left NoNameFlag
        else
            case get_flag (Right flag_name) (description parser') of
                Nothing -> Left (InvalidFlag $ "--" ++ flag_name)

                Just (FlagProps (ShortFlagName flag_ch) _, _, Just (TakesValueProps _ n_values)) ->
                    parse_values n_values (drop_first_str_if_empty parser') >>= \ (values, parser'') ->
                    let parser''' = record_option_match flag_ch values parser''
                    in Right parser'''

                Just (FlagProps (ShortFlagName flag_ch) _, _, Nothing) ->
                    if first_str parser' /= Just ""
                        then Left UnexpectedValue
                        else Right $ drop_first_str $ record_flag_presence flag_ch parser'

parse_short_flag :: Parser -> Either ParseError Parser
parse_short_flag parser =
    case first_ch parser of
        Nothing -> Right $ drop_first_str parser
        Just flag_name ->
            case get_flag (Left flag_name) (description parser) of
                Nothing -> Left (InvalidFlag $ ['-', flag_name])

                Just (FlagProps (ShortFlagName flag_ch) _, _, Just (TakesValueProps _ n_values)) ->
                    parse_values n_values (drop_first_str_if_empty $ drop_first_ch parser) >>= \ (values, parser') ->
                    Right $ record_option_match flag_ch values parser'

                Just (FlagProps (ShortFlagName ch) _, _, Nothing) ->
                    let parser' = record_flag_presence ch parser
                    in parse_short_flag (drop_first_ch parser')

parse_positional :: Parser -> Either ParseError Parser
parse_positional parser =
    case uncons_positionals parser of
        Just ((_, (TakesValueProps (ValueName v_name) n_values)), more_positionals) ->
            parse_values n_values parser >>= \ (values, parser') ->
            Right $ record_positional_match v_name values parser' { positionals = more_positionals }

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

-- tests {{{1
parser_with_input :: [String] -> Parser
parser_with_input inp = Parser (Description []) inp empty_matches []

make_parse_test :: [Arg] -> [String] -> [String] -> Matches -> [(ArgProps, TakesValueProps)] -> (Parser -> Either ParseError Parser) -> IO ()
make_parse_test desc inp expected_inp expected_matches expected_positionals parse_fn =
    case parse_fn (make_parser (Description desc) inp) of
        Right p ->
            (expected_inp @=? input p) >>
            (expected_matches @=? matches p) >>
            (expected_positionals @=? positionals p)

        Left e -> assertFailure $ "expected parser to return success, but it returned failure: '" ++ show e ++ "'"

expect_fail_test :: [Arg] -> [String] -> ParseError -> (Parser -> Either ParseError Parser) -> IO ()
expect_fail_test desc inp e parse_fn =
    case parse_fn (make_parser (Description desc) inp) of
        Left e'
            | e == e' -> return ()

        Left e' -> assertFailure $ "parser did not return correct error: expected '" ++ show e ++ "' but got '" ++ show e' ++ "'"

        Right p -> assertFailure $ "expected parser to fail, but it succedeed: '" ++ show p ++ "'"

case_first_str_with_input :: Assertion
case_first_str_with_input = Just "abc" @=? first_str (parser_with_input ["abc", "def"])
case_first_str_empty = Nothing @=? first_str (parser_with_input [])
case_first_str_empty :: Assertion

case_first_ch_with_input = Just 'a' @=? first_ch (parser_with_input ["abc", "def"])
case_first_ch_with_input :: Assertion
case_first_ch_empty_first = Nothing @=? first_ch (parser_with_input ["", "abc"])
case_first_ch_empty_first :: Assertion
case_first_ch_empty = Nothing @=? first_ch (parser_with_input [])
case_first_ch_empty :: Assertion

case_uncons_input = Just ("abc", ["def", "ghi"]) @=? uncons_input (parser_with_input ["abc", "def", "ghi"])
case_uncons_input :: Assertion

case_uncons_positionals :: Assertion
case_uncons_positionals =
    let ap = ArgProps (HelpMessage "")
        tvp = TakesValueProps (ValueName "v") ZeroOrMore
    in Just ((ap, tvp), []) @=? uncons_positionals (Parser (Description []) [] empty_matches [(ap, tvp)])

case_record_positional_match = Matches (Map.fromList [("key", ["val1", "val2"])]) "" Map.empty @=? matches (record_positional_match "key" ["val1", "val2"] (parser_with_input []))
case_record_positional_match :: Assertion

case_record_flat_presence = Matches Map.empty "c" Map.empty @=? matches (record_flag_presence 'c' (parser_with_input []))
case_record_flat_presence :: Assertion

case_record_option_match = Matches Map.empty "" (Map.fromList [('c', ["val1", "val2"])]) @=? matches (record_option_match 'c' ["val1", "val2"] (parser_with_input []))
case_record_option_match :: Assertion

case_drop_first_str = ["b"] @=? input (drop_first_str $ parser_with_input ["a", "b"])
case_drop_first_str :: Assertion
case_drop_first_str_empty = [] @=? input (drop_first_str $ parser_with_input [])
case_drop_first_str_empty :: Assertion

case_drop_first_ch = ["bc", "def"] @=? input (drop_first_ch $ parser_with_input ["abc", "def"])
case_drop_first_ch :: Assertion
case_drop_first_ch_2 = ["bc"] @=? input (drop_first_ch $ parser_with_input ["abc"])
case_drop_first_ch_2 :: Assertion
case_drop_first_ch_first_empty = [""] @=? input (drop_first_ch $ parser_with_input [""])
case_drop_first_ch_first_empty :: Assertion
case_drop_first_ch_empty = [] @=? input (drop_first_ch $ parser_with_input [])
case_drop_first_ch_empty :: Assertion

case_drop_first_str_if_empty_no = ["abc", "def"] @=? input (drop_first_str_if_empty $ parser_with_input ["abc", "def"])
case_drop_first_str_if_empty_no :: Assertion
case_drop_first_str_if_empty_empty = ["abc", "def"] @=? input (drop_first_str_if_empty $ parser_with_input ["", "abc", "def"])
case_drop_first_str_if_empty_empty :: Assertion

case_consume_until_char_or_end_char :: Assertion
case_consume_until_char_or_end_char =
    let (res, p) = consume_until_char_or_end '=' (parser_with_input ["abc=def", "ghi"])
    in ("abc" @=? res) >> (["def", "ghi"] @=? input p)

case_consume_until_char_or_end_end :: Assertion
case_consume_until_char_or_end_end =
    let (res, p) = consume_until_char_or_end '=' (parser_with_input ["abc", "def", "ghi"])
    in ("abc" @=? res) >> (["", "def", "ghi"] @=? input p)

case_consume_until_char_or_end_empty :: Assertion
case_consume_until_char_or_end_empty =
    let (res, p) = consume_until_char_or_end '=' (parser_with_input [])
    in ("" @=? res) >> ([] @=? input p)

case_parse :: Assertion
case_parse =
    let parsed =
            parse
                (Description [flag 'f' Nothing "", flag 'g' (Just "gflag") "", option 'o' Nothing "" "option_value" (Number 2), option 'p' (Just "poption") "" "poption_value" (Number 2), positional "" "positional_val" (Number 1)])
                ["-f", "--gflag", "-o", "a", "b", "--poption", "c", "d", "pos"]
    in Right (Matches (Map.fromList [("positional_val", ["pos"])]) "gf" (Map.fromList [('o', ["a", "b"]), ('p', ["c", "d"])])) @=? parsed

case_parse_flag_short = make_parse_test [flag 's' Nothing ""] ["s"] [] (Matches Map.empty "s" Map.empty) [] parse_flag
case_parse_flag_short :: Assertion
case_parse_flag_long = make_parse_test [flag 's' (Just "long") ""] ["-long"] [] (Matches Map.empty "s" Map.empty) [] parse_flag
case_parse_flag_long :: Assertion

case_parse_long_flag_no_name = expect_fail_test [flag 's' (Just "long") ""] [""] NoNameFlag parse_long_flag
case_parse_long_flag_no_name :: Assertion
case_parse_long_flag_invalid = expect_fail_test [flag 's' (Just "long") ""] ["abc"] (InvalidFlag "--abc") parse_long_flag
case_parse_long_flag_invalid :: Assertion
case_parse_long_flag_unexpected_value = expect_fail_test [flag 's' (Just "long") ""] ["long=abc"] UnexpectedValue parse_long_flag
case_parse_long_flag_unexpected_value :: Assertion

case_parse_long_flag = make_parse_test [flag 's' (Just "long") ""] ["long"] [] (Matches Map.empty "s" Map.empty) [] parse_long_flag
case_parse_long_flag :: Assertion
case_parse_long_flag_long_option_with_space = make_parse_test [option 's' (Just "long") "" "long_value" (Number 1)] ["long", "abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_long_flag
case_parse_long_flag_long_option_with_space :: Assertion
case_parse_long_flag_long_option_with_equal = make_parse_test [option 's' (Just "long") "" "long_value" (Number 1)] ["long=abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_long_flag
case_parse_long_flag_long_option_with_equal :: Assertion

case_parse_short_flag_invalid = expect_fail_test [flag 's' Nothing ""] ["a"] (InvalidFlag "-a") parse_short_flag
case_parse_short_flag_invalid :: Assertion

case_parse_short_flag_short_flag = make_parse_test [flag 's' Nothing ""] ["s"] [] (Matches Map.empty "s" Map.empty) [] parse_short_flag
case_parse_short_flag_short_flag :: Assertion
case_parse_short_flag_multiple_short_flags = make_parse_test [flag 's' Nothing "", flag 'a' Nothing ""] ["sa"] [] (Matches Map.empty "as" Map.empty) [] parse_short_flag
case_parse_short_flag_multiple_short_flags :: Assertion
case_parse_short_flag_short_option_with_space = make_parse_test [option 's' Nothing "" "value_name" (Number 1)] ["s", "abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
case_parse_short_flag_short_option_with_space :: Assertion
case_parse_short_flag_short_option_with_no_space = make_parse_test [option 's' Nothing "" "value_name" (Number 1)] ["sabc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
case_parse_short_flag_short_option_with_no_space :: Assertion
case_parse_short_flag_flags_and_options_with_space = make_parse_test [flag 'a' Nothing "", option 's' Nothing "" "value_name" (Number 1)] ["as", "abc"] [] (Matches Map.empty "a" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
case_parse_short_flag_flags_and_options_with_space :: Assertion
case_parse_short_flag_flags_and_options_with_no_space = make_parse_test [flag 'a' Nothing "", option 's' Nothing "" "value_name" (Number 1)] ["asabc"] [] (Matches Map.empty "a" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
case_parse_short_flag_flags_and_options_with_no_space :: Assertion

case_parse_positional = make_parse_test [positional "" "positional_val" (Number 1)] ["val"] [] (Matches (Map.fromList [("positional_val", ["val"])]) "" Map.empty) [] parse_positional
case_parse_positional :: Assertion
case_parse_positional_extra = expect_fail_test [] ["val"] ExcessArguments parse_positional
case_parse_positional_extra :: Assertion

case_parse_values_1_empty = Left ExpectedValue @=? parse_values (Number 1) (parser_with_input [])
case_parse_values_1_empty :: Assertion
case_parse_values_1_1_value = Right (["val"], parser_with_input []) @=? parse_values (Number 1) (parser_with_input ["val"])
case_parse_values_1_1_value :: Assertion
case_parse_values_1_2_values = Right (["val1"], parser_with_input ["val2"]) @=? parse_values (Number 1) (parser_with_input ["val1", "val2"])
case_parse_values_1_2_values :: Assertion

case_parse_values_2_empty = Left ExpectedValue @=? parse_values (Number 2) (parser_with_input [])
case_parse_values_2_empty :: Assertion
case_parse_values_2_1_value = Left ExpectedValue @=? parse_values (Number 2) (parser_with_input ["val"])
case_parse_values_2_1_value :: Assertion
case_parse_values_2_2_values = Right (["val1", "val2"], parser_with_input []) @=? parse_values (Number 2) (parser_with_input ["val1", "val2"])
case_parse_values_2_2_values :: Assertion

case_parse_values_zero_more_empty = Right ([], parser_with_input []) @=? parse_values ZeroOrMore (parser_with_input [])
case_parse_values_zero_more_empty :: Assertion
case_parse_values_zero_more_1_value = Right (["val"], parser_with_input []) @=? parse_values OneOrMore (parser_with_input ["val"])
case_parse_values_zero_more_1_value :: Assertion
case_parse_values_zero_more_2_values = Right (["val1", "val2"], parser_with_input []) @=? parse_values OneOrMore (parser_with_input ["val1", "val2"])
case_parse_values_zero_more_2_values :: Assertion

case_parse_values_one_more_empty = Left ExpectedValue @=? parse_values OneOrMore (parser_with_input [])
case_parse_values_one_more_empty :: Assertion
case_parse_values_one_more_1_value = Right (["val"], parser_with_input []) @=? parse_values OneOrMore (parser_with_input ["val"])
case_parse_values_one_more_1_value :: Assertion
case_parse_values_one_more_2_values = Right (["val1", "val2"], parser_with_input []) @=? parse_values OneOrMore (parser_with_input ["val1", "val2"])
case_parse_values_one_more_2_values :: Assertion

case_parse_zero_more_empty = Right ([], parser_with_input []) @=? parse_zero_or_more [] (parser_with_input [])
case_parse_zero_more_empty :: Assertion
case_parse_zero_more_1 = Right (["val"], parser_with_input []) @=? parse_zero_or_more [] (parser_with_input ["val"])
case_parse_zero_more_1 :: Assertion
case_parse_zero_more_2 = Right (["val1", "val2"], parser_with_input []) @=? parse_zero_or_more [] (parser_with_input ["val1", "val2"])
case_parse_zero_more_2 :: Assertion

case_parse_n_values_1_empty = Left ExpectedValue @=? parse_n_values [] 1 (parser_with_input [])
case_parse_n_values_1_empty :: Assertion
case_parse_n_values_1_1 :: Assertion
case_parse_n_values_1_1 = Right (["val"], parser_with_input []) @=? parse_n_values [] 1 (parser_with_input ["val"])
case_parse_n_values_1_2 :: Assertion
case_parse_n_values_1_2 = Right (["val1"], parser_with_input ["val2"]) @=? parse_n_values [] 1 (parser_with_input ["val1", "val2"])

case_parse_n_values_2_empty :: Assertion
case_parse_n_values_2_empty = Left ExpectedValue @=? parse_n_values [] 2 (parser_with_input [])
case_parse_n_values_2_1 :: Assertion
case_parse_n_values_2_1 = Left ExpectedValue @=? parse_n_values [] 2 (parser_with_input ["val"])
case_parse_n_values_2_2 :: Assertion
case_parse_n_values_2_2 = Right (["val1", "val2"], parser_with_input []) @=? parse_n_values [] 2 (parser_with_input ["val1", "val2"])

case_parse_value_once_value :: Assertion
case_parse_value_once_value = Just ("val", parser_with_input []) @=? parse_value_once (parser_with_input ["val"])
case_parse_value_once_2_values :: Assertion
case_parse_value_once_2_values = Just ("val1", parser_with_input ["val2"]) @=? parse_value_once (parser_with_input ["val1", "val2"])
case_parse_values_once_empty :: Assertion
case_parse_values_once_empty = Nothing @=? parse_value_once (parser_with_input [])
case_parse_values_once_long_flag :: Assertion
case_parse_values_once_long_flag = Nothing @=? parse_value_once (parser_with_input ["--flag"])
case_parse_values_once_short_flag :: Assertion
case_parse_values_once_short_flag = Nothing @=? parse_value_once (parser_with_input ["-f"])

tests :: TestTree
tests = $(testGroupGenerator)
