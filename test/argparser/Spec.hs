module Main where

import Test.HUnit

import qualified Brace.ArgParser as ArgParser (tests)
import qualified Brace.ArgParser.Parser as Parser (tests)
import qualified Brace.ArgParser.Help as Help (tests)

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "ArgParser" ~: ArgParser.tests
            , "ArgParser.Parser" ~: Parser.tests
            , "ArgParser.Help" ~: Help.tests
            ]
