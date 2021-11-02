module Brace.ArgParser
    ( get_matches

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

import Test.HUnit

import System.IO (hPutStr, hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

import Brace.ArgParser.Description
import Brace.ArgParser.Help hiding (tests)
import Brace.ArgParser.Parser hiding (tests)

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
tests :: Test
tests = test
    [ "get_matches'" ~:
        let desc = (Description [flag 'c' Nothing "help"])
            prog_name = "prog"

            help_message = "usage: prog [flags]\n\
                           \\n\
                           \flags:\n\
                           \    -c                  help\n\
                           \    -h, --help          show this help message\n"
        in
            [ Right (Right (Map.empty, "c", Map.empty)) ~=? get_matches' desc prog_name ["-c"]
            , Left ("error: invalid flag: '-j'\n" ++ help_message) ~=? get_matches' desc prog_name ["-j"]

            , Right (Left help_message) ~=? get_matches' desc prog_name ["-h"]
            , Right (Left help_message) ~=? get_matches' desc prog_name ["--help"]

            , Right (Right (Map.empty, "h", Map.empty)) ~=? get_matches' (Description [flag 'h' Nothing "other thing"]) prog_name ["-h"]
            ]

    , "get_positional_result" ~:
        [ ["abc", "def"] ~=? get_positional_result "pos" (ArgMatches (Map.fromList [("pos", ["abc", "def"])]) [] Map.empty)
        , [] ~=? get_positional_result "pos" (ArgMatches (Map.fromList [("pos", [])]) [] Map.empty)
        , [] ~=? get_positional_result "pos" (ArgMatches Map.empty [] Map.empty)
        ]

    , "get_flag_result" ~:
        [ True ~=? get_flag_result 'c' (ArgMatches Map.empty "c" Map.empty)
        , False ~=? get_flag_result 'c' (ArgMatches Map.empty "" Map.empty)
        ]

    , "get_option_result" ~:
        [ ["abc", "def"] ~=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc", "def"])]))
        , [] ~=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList [('o', [])]))
        , [] ~=? get_option_result 'o' (ArgMatches Map.empty [] (Map.fromList []))
        ]

    , "get_positional_result_ne" ~:
        [ Right ("abc" NonEmpty.:| ["def"]) ~=? get_positional_result_ne "pos" (ArgMatches (Map.fromList [("pos", ["abc", "def"])]) [] Map.empty)
        , Left (NeedsArguments "pos") ~=? get_positional_result_ne "pos" (ArgMatches Map.empty [] Map.empty)
        ]

    , "get_positional_result_1" ~:
        [ Right "abc" ~=? get_positional_result_1 "pos" (ArgMatches (Map.fromList [("pos", ["abc"])]) [] Map.empty)
        , Left (Needs1Argument "pos") ~=? get_positional_result_1 "pos" (ArgMatches Map.empty [] Map.empty)
        ]

    , "get_option_result_ne" ~:
        [ Right ("abc" NonEmpty.:| ["def"]) ~=? get_option_result_ne 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc", "def"])]))
        , Left (NeedsArguments "-o") ~=? get_option_result_ne 'o' (ArgMatches Map.empty [] Map.empty)
        ]

    , "get_option_result_1" ~:
        [ Right "abc" ~=? get_option_result_1 'o' (ArgMatches Map.empty [] (Map.fromList [('o', ["abc"])]))
        , Left (Needs1Argument "-o") ~=? get_option_result_1 'o' (ArgMatches Map.empty [] Map.empty)
        ]

    , "require_1" ~:
        [ Left (Needs1Argument "name") ~=? require_1 "name" []
        , Right "a" ~=? require_1 "name" ["a"]
        , Left (Needs1Argument "name") ~=? require_1 "name" ["a", "b"]
        ]

    , "require_non_empty" ~:
        [ Left (NeedsArguments "name") ~=? require_non_empty "name" []
        , Right ("a" NonEmpty.:| [])  ~=? require_non_empty "name" ["a"]
        , Right ("a" NonEmpty.:| ["b"]) ~=? require_non_empty "name" ["a", "b"]
        ]
    ]
