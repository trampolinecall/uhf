module Main where

import Test.HUnit

import qualified UHF.ArgParser as ArgParser (tests)
import qualified UHF.ArgParser.Parser as Parser (tests)
import qualified UHF.ArgParser.Help as Help (tests)

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "ArgParser" ~: ArgParser.tests
            , "ArgParser.Parser" ~: Parser.tests
            , "ArgParser.Help" ~: Help.tests
            ]
