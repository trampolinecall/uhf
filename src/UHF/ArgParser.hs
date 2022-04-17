{-# LANGUAGE TemplateHaskell #-}

module UHF.ArgParser
    ( module UHF.ArgParser.Description

    , get_matches_io
    , get_matches

    , ArgMatches
    , get_flag_result
    , get_positional_result
    , get_positional_result_ne
    , get_positional_result_1
    , get_option_result
    , get_option_result_ne
    , get_option_result_1

    , report_errors

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import System.IO (hPutStr, hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getProgName, getArgs)

import UHF.ArgParser.Description
import UHF.ArgParser.Help hiding (tests)
import UHF.ArgParser.Parser hiding (tests)

import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty

-- ArgMatches {{{1
data ArgMatches = ArgMatches (Map.Map String [String]) [Char] (Map.Map Char [String])
data ArgError = Needs1Argument String | NeedsArguments String deriving (Show, Eq)

str_arg_error :: ArgError -> String
str_arg_error (Needs1Argument n) = "argument '" ++ n ++ "' requires 1 value"
str_arg_error (NeedsArguments n) = "argument '" ++ n ++ "' requires values"

get_flag_result :: Char -> ArgMatches -> Bool
get_flag_result c (ArgMatches _ f _) = c `elem` f

get_positional_result :: String -> ArgMatches -> [String]
get_positional_result n (ArgMatches p _ _) = Map.findWithDefault [] n p

get_positional_result_ne :: String -> ArgMatches -> Either ArgError (NonEmpty.NonEmpty String)
get_positional_result_ne n = require_non_empty n . get_positional_result n

get_positional_result_1 :: String -> ArgMatches -> Either ArgError String
get_positional_result_1 n = require_1 n . get_positional_result n

get_option_result :: Char -> ArgMatches -> [String]
get_option_result n (ArgMatches _ _ o) = Map.findWithDefault [] n o

get_option_result_ne :: Char -> ArgMatches -> Either ArgError (NonEmpty.NonEmpty String)
get_option_result_ne n = require_non_empty ['-', n] . get_option_result n

get_option_result_1 :: Char -> ArgMatches -> Either ArgError String
get_option_result_1 n = require_1 ['-', n] . get_option_result n

require_1 :: String -> [String] -> Either ArgError String
require_1 n [] = Left $ Needs1Argument n
require_1 _ [x] = Right x
require_1 n _ = Left $ Needs1Argument n

require_non_empty :: String -> [String] -> Either ArgError (NonEmpty.NonEmpty String)
require_non_empty n [] = Left $ NeedsArguments n
require_non_empty _ (x:xs) = Right $ x NonEmpty.:| xs

-- get_matches {{{1
get_matches_io :: Description -> IO ArgMatches
get_matches_io description =
    getProgName >>= \ prog_name ->
    getArgs >>= \ args ->
    get_matches description prog_name args

get_matches :: Description -> String -> [String] -> IO ArgMatches
get_matches desc prog_name args =
    case get_matches' desc prog_name args of
        Right (Right (p, f, o)) -> return $ ArgMatches p f o

        Right (Left help_message) ->
            putStr help_message >> exitSuccess

        Left err -> hPutStr stderr err >> exitFailure

get_matches' :: Description -> String -> [String] -> Either String (Either String (Map.Map String [String], [Char], Map.Map Char [String]))
get_matches' desc@(Description desc_args) prog_name args =
    let (desc', added_help_flag) =
            case get_flag (Left 'h') desc of
                Just _ -> (desc, False)
                Nothing -> (Description (desc_args ++ [flag 'h' (Just "help") "show this help message"]), True)

    in case parse desc' args of
        Right (Matches positionals flags options) ->
            if 'h' `elem` flags && added_help_flag
                then Right (Left $ args_help_message desc' prog_name)
                else Right (Right (positionals, flags, options))

        Left err -> Left $ "error: " ++ str_error err ++ "\n" ++ args_help_message desc' prog_name
-- report_errors {{{1
report_errors :: Either ArgError a -> IO a
report_errors e =
    case e of
        Right a -> return a

        Left ae ->
            hPutStrLn stderr ("error: " ++ str_arg_error ae) >>
            exitFailure
-- tests {{{1
test_get_matches' :: [TestTree]
test_get_matches' =
    let desc = (Description [flag 'c' Nothing "help"])
        prog_name = "prog"

        help_message = "usage: prog [flags]\n\
                       \\n\
                       \flags:\n\
                       \    -c                  help\n\
                       \    -h, --help          show this help message\n"
    in
        [ testCase "get_matches'_flag" $ Right (Right (Map.empty, "c", Map.empty)) @=? get_matches' desc prog_name ["-c"]
        , testCase "get_matches'_invalid_flag" $ Left ("error: invalid flag: '-j'\n" ++ help_message) @=? get_matches' desc prog_name ["-j"]

        , testCase "get_matches'_help" $ Right (Left help_message) @=? get_matches' desc prog_name ["-h"]
        , testCase "get_matches'_long_help" $ Right (Left help_message) @=? get_matches' desc prog_name ["--help"]

        , testCase "get_matches'_other_h_flag" $ Right (Right (Map.empty, "h", Map.empty)) @=? get_matches' (Description [flag 'h' Nothing "other thing"]) prog_name ["-h"]
        ]

case_get_positional_result_results :: Assertion
case_get_positional_result_results = ["abc", "def"] @=? get_positional_result "pos" (ArgMatches (Map.fromList [("pos", ["abc", "def"])]) [] Map.empty)
case_get_positional_result_empty :: Assertion
case_get_positional_result_empty = [] @=? get_positional_result "pos" (ArgMatches (Map.fromList [("pos", [])]) [] Map.empty)
case_get_positional_result_no_result :: Assertion
case_get_positional_result_no_result = [] @=? get_positional_result "pos" (ArgMatches Map.empty [] Map.empty)

case_get_flag_result_yes :: Assertion
case_get_flag_result_yes = True @=? get_flag_result 'c' (ArgMatches Map.empty "c" Map.empty)
case_get_flag_result_no :: Assertion
case_get_flag_result_no = False @=? get_flag_result 'c' (ArgMatches Map.empty "" Map.empty)

case_get_option_result_multiple :: Assertion
case_get_option_result_multiple = ["abc", "def"] @=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc", "def"])]))
case_get_option_result_empty :: Assertion
case_get_option_result_empty = [] @=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList [('o', [])]))
case_get_option_result_no_result :: Assertion
case_get_option_result_no_result = [] @=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList []))

case_get_positional_result_ne :: Assertion
case_get_positional_result_ne = Right ("abc" NonEmpty.:| ["def"]) @=? get_positional_result_ne "pos" (ArgMatches (Map.fromList [("pos", ["abc", "def"])]) [] Map.empty)
case_get_positional_result_ne_empty :: Assertion
case_get_positional_result_ne_empty = Left (NeedsArguments "pos") @=? get_positional_result_ne "pos" (ArgMatches Map.empty [] Map.empty)

case_get_positional_result_1 :: Assertion
case_get_positional_result_1 = Right "abc" @=? get_positional_result_1 "pos" (ArgMatches (Map.fromList [("pos", ["abc"])]) [] Map.empty)
case_get_positional_result_1_none :: Assertion
case_get_positional_result_1_none = Left (Needs1Argument "pos") @=? get_positional_result_1 "pos" (ArgMatches Map.empty [] Map.empty)

case_get_option_result_ne :: Assertion
case_get_option_result_ne = Right ("abc" NonEmpty.:| ["def"]) @=? get_option_result_ne 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc", "def"])]))
case_get_option_result_ne_empty :: Assertion
case_get_option_result_ne_empty = Left (NeedsArguments "-o") @=? get_option_result_ne 'o' (ArgMatches Map.empty [] Map.empty)

case_get_option_result_1 :: Assertion
case_get_option_result_1 = Right "abc" @=? get_option_result_1 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc"])]))
case_get_option_result_1_empty :: Assertion
case_get_option_result_1_empty = Left (Needs1Argument "-o") @=? get_option_result_1 'o' (ArgMatches Map.empty [] Map.empty)

case_require_1_none :: Assertion
case_require_1_none = Left (Needs1Argument "name") @=? require_1 "name" []
case_require_1_1 :: Assertion
case_require_1_1 = Right "a" @=? require_1 "name" ["a"]
case_require_1_2 :: Assertion
case_require_1_2 = Left (Needs1Argument "name") @=? require_1 "name" ["a", "b"]

case_require_non_empty_none :: Assertion
case_require_non_empty_none = Left (NeedsArguments "name") @=? require_non_empty "name" []
case_require_non_empty_1 :: Assertion
case_require_non_empty_1 = Right ("a" NonEmpty.:| [])  @=? require_non_empty "name" ["a"]
case_require_non_empty_2 :: Assertion
case_require_non_empty_2 = Right ("a" NonEmpty.:| ["b"]) @=? require_non_empty "name" ["a", "b"]

tests :: TestTree
tests = $(testGroupGenerator)
