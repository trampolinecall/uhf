module Main where

import Test.HUnit

import qualified Brace.ArgParser as ArgParser (tests)
import qualified Brace.ArgParser.Parser as Parser (tests)

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "ArgParser" ~: ArgParser.tests
            , "ArgParser.Parser" ~: Parser.tests
            ]
