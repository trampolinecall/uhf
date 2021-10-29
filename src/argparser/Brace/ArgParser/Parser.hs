module Brace.ArgParser.Parser
    ( Matches(..)
    , ParseError(..)
    , parse
    , str_error
    , tests
    ) where

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

tests :: Test
tests =
    let parser_with_input inp = Parser (Description []) inp empty_matches []
    in test
        [ "first_str" ~:
            [ Just "abc" ~=? first_str (parser_with_input ["abc", "def"])
            , Nothing ~=? first_str (parser_with_input [])
            ]

        , "first_ch" ~:
            [ Just 'a' ~=? first_ch (parser_with_input ["abc", "def"])
            , Nothing ~=? first_ch (parser_with_input ["", "abc"])
            , Nothing ~=? first_ch (parser_with_input [])
            ]

        , "uncons_input" ~:
            Just ("abc", ["def", "ghi"]) ~=? uncons_input (parser_with_input ["abc", "def", "ghi"])

        , "uncons_positionals" ~:
            let ap = ArgProps (HelpMessage "")
                tvp = TakesValueProps (ValueName "v") ZeroOrMore
            in Just ((ap, tvp), []) ~=? uncons_positionals (Parser (Description []) [] empty_matches [(ap, tvp)])

        , "record_positional_match" ~:
            Matches (Map.fromList [("key", ["val1", "val2"])]) "" Map.empty ~=? matches (record_positional_match "key" ["val1", "val2"] (parser_with_input []))

        , "record_flag_presence" ~:
            Matches Map.empty "c" Map.empty ~=? matches (record_flag_presence 'c' (parser_with_input []))

        , "record_option_match" ~:
            Matches Map.empty "" (Map.fromList [('c', ["val1", "val2"])]) ~=? matches (record_option_match 'c' ["val1", "val2"] (parser_with_input []))

        , "drop_first_str" ~:
            [ ["b"] ~=? input (drop_first_str $ parser_with_input ["a", "b"])
            , [] ~=? input (drop_first_str $ parser_with_input [])
            ]

        , "drop_first_ch" ~:
            [ ["bc", "def"] ~=? input (drop_first_ch $ parser_with_input ["abc", "def"])
            , ["bc"] ~=? input (drop_first_ch $ parser_with_input ["abc"])
            , [""] ~=? input (drop_first_ch $ parser_with_input [""])
            , [] ~=? input (drop_first_ch $ parser_with_input [])
            ]

        , "drop_first_str_if_empty" ~:
            [ ["abc", "def"] ~=? input (drop_first_str_if_empty $ parser_with_input ["abc", "def"])
            , ["abc", "def"] ~=? input (drop_first_str_if_empty $ parser_with_input ["", "abc", "def"])
            ]

        , "consume_until_char_or_end" ~:
            [ let (res, p) = consume_until_char_or_end '=' (parser_with_input ["abc=def", "ghi"])
              in ("abc" @=? res) >> (["def", "ghi"] @=? input p)

            , let (res, p) = consume_until_char_or_end '=' (parser_with_input ["abc", "def", "ghi"])
              in ("abc" @=? res) >> (["", "def", "ghi"] @=? input p)

            , let (res, p) = consume_until_char_or_end '=' (parser_with_input [])
              in ("" @=? res) >> ([] @=? input p)
            ]

        , "parsing" ~:
            let make_parse_test name desc inp expected_inp expected_matches expected_positionals parse_fn =
                    name ~:
                        case parse_fn (make_parser (Description desc) inp) of
                            Right p ->
                                (expected_inp @=? input p) >>
                                (expected_matches @=? matches p) >>
                                (expected_positionals @=? positionals p)

                            Left e -> assertFailure $ "expected parser to return success, but it returned failure: '" ++ show e ++ "'"

                expect_fail_test name desc inp e parse_fn =
                    name ~:
                        case parse_fn (make_parser (Description desc) inp) of
                            Left e'
                                | e == e' -> return ()

                            Left e' -> assertFailure $ "parser did not return correct error: expected '" ++ show e ++ "' but got '" ++ show e' ++ "'"

                            Right p -> assertFailure $ "expected parser to fail, but it succedeed: '" ++ show p ++ "'"

            in
                [ "parse" ~:
                    let parsed =
                            parse
                                (Description [flag 'f' Nothing "", flag 'g' (Just "gflag") "", option 'o' Nothing "" "option_value" (Number 2), option 'p' (Just "poption") "" "poption_value" (Number 2), positional "" "positional_val" (Number 1)])
                                ["-f", "--gflag", "-o", "a", "b", "--poption", "c", "d", "pos"]
                    in Right (Matches (Map.fromList [("positional_val", ["pos"])]) "gf" (Map.fromList [('o', ["a", "b"]), ('p', ["c", "d"])])) @=? parsed

                , "parse_flag" ~:
                    [ make_parse_test "short flags" [flag 's' Nothing ""] ["s"] [] (Matches Map.empty "s" Map.empty) [] parse_flag
                    , make_parse_test "long flags" [flag 's' (Just "long") ""] ["-long"] [] (Matches Map.empty "s" Map.empty) [] parse_flag
                    ]

                , "parse_long_flag" ~:
                    [ expect_fail_test "no name" [flag 's' (Just "long") ""] [""] NoNameFlag parse_long_flag
                    , expect_fail_test "invalid" [flag 's' (Just "long") ""] ["abc"] (InvalidFlag "--abc") parse_long_flag
                    , expect_fail_test "unexpected value" [flag 's' (Just "long") ""] ["long=abc"] UnexpectedValue parse_long_flag

                    , make_parse_test "long flag" [flag 's' (Just "long") ""] ["long"] [] (Matches Map.empty "s" Map.empty) [] parse_long_flag
                    , make_parse_test "long option with space" [option 's' (Just "long") "" "long_value" (Number 1)] ["long", "abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_long_flag
                    , make_parse_test "long option with equal" [option 's' (Just "long") "" "long_value" (Number 1)] ["long=abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_long_flag
                    ]

                , "parse_short_flag" ~:
                    [ expect_fail_test "invalid" [flag 's' Nothing ""] ["a"] (InvalidFlag "-a") parse_short_flag

                    , make_parse_test "short flag" [flag 's' Nothing ""] ["s"] [] (Matches Map.empty "s" Map.empty) [] parse_short_flag
                    , make_parse_test "multiple short flags" [flag 's' Nothing "", flag 'a' Nothing ""] ["sa"] [] (Matches Map.empty "as" Map.empty) [] parse_short_flag

                    , make_parse_test "short option with space" [option 's' Nothing "" "value_name" (Number 1)] ["s", "abc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
                    , make_parse_test "short option with no space" [option 's' Nothing "" "value_name" (Number 1)] ["sabc"] [] (Matches Map.empty "" (Map.fromList [('s', ["abc"])])) [] parse_short_flag

                    , make_parse_test "short flags and options with space" [flag 'a' Nothing "", option 's' Nothing "" "value_name" (Number 1)] ["as", "abc"] [] (Matches Map.empty "a" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
                    , make_parse_test "short flags and options with no space" [flag 'a' Nothing "", option 's' Nothing "" "value_name" (Number 1)] ["asabc"] [] (Matches Map.empty "a" (Map.fromList [('s', ["abc"])])) [] parse_short_flag
                    ]

                , "parse_positional" ~:
                    [ make_parse_test "positional" [positional "" "positional_val" (Number 1)] ["val"] [] (Matches (Map.fromList [("positional_val", ["val"])]) "" Map.empty) [] parse_positional
                    , expect_fail_test "extra positional" [] ["val"] ExcessArguments parse_positional
                    ]

                , "parse_values" ~:
                    [ Left ExpectedValue ~=? parse_values (Number 1) (parser_with_input [])
                    , Right (["val"], parser_with_input []) ~=? parse_values (Number 1) (parser_with_input ["val"])
                    , Right (["val1"], parser_with_input ["val2"]) ~=? parse_values (Number 1) (parser_with_input ["val1", "val2"])

                    , Left ExpectedValue ~=? parse_values (Number 2) (parser_with_input [])
                    , Left ExpectedValue ~=? parse_values (Number 2) (parser_with_input ["val"])
                    , Right (["val1", "val2"], parser_with_input []) ~=? parse_values (Number 2) (parser_with_input ["val1", "val2"])

                    , Right ([], parser_with_input []) ~=? parse_values ZeroOrMore (parser_with_input [])
                    , Right (["val"], parser_with_input []) ~=? parse_values OneOrMore (parser_with_input ["val"])
                    , Right (["val1", "val2"], parser_with_input []) ~=? parse_values OneOrMore (parser_with_input ["val1", "val2"])

                    , Left ExpectedValue ~=? parse_values OneOrMore (parser_with_input [])
                    , Right (["val"], parser_with_input []) ~=? parse_values OneOrMore (parser_with_input ["val"])
                    , Right (["val1", "val2"], parser_with_input []) ~=? parse_values OneOrMore (parser_with_input ["val1", "val2"])
                    ]

                , "parse_zero_or_more" ~:
                    [ Right ([], parser_with_input []) ~=? parse_zero_or_more [] (parser_with_input [])
                    , Right (["val"], parser_with_input []) ~=? parse_zero_or_more [] (parser_with_input ["val"])
                    , Right (["val1", "val2"], parser_with_input []) ~=? parse_zero_or_more [] (parser_with_input ["val1", "val2"])
                    ]

                , "parse_n_values" ~:
                   [ Left ExpectedValue ~=? parse_n_values [] 1 (parser_with_input [])
                   , Right (["val"], parser_with_input []) ~=? parse_n_values [] 1 (parser_with_input ["val"])
                   , Right (["val1"], parser_with_input ["val2"]) ~=? parse_n_values [] 1 (parser_with_input ["val1", "val2"])

                   , Left ExpectedValue ~=? parse_n_values [] 2 (parser_with_input [])
                   , Left ExpectedValue ~=? parse_n_values [] 2 (parser_with_input ["val"])
                   , Right (["val1", "val2"], parser_with_input []) ~=? parse_n_values [] 2 (parser_with_input ["val1", "val2"])
                   ]

                , "parse_value_once" ~:
                   [ Just ("val", parser_with_input []) ~=? parse_value_once (parser_with_input ["val"])
                   , Just ("val1", parser_with_input ["val2"]) ~=? parse_value_once (parser_with_input ["val1", "val2"])
                   , Nothing ~=? parse_value_once (parser_with_input [])
                   , Nothing ~=? parse_value_once (parser_with_input ["--flag"])
                   , Nothing ~=? parse_value_once (parser_with_input ["-f"])
                   ]
                ]
        ]
