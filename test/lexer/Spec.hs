module Main where

import Test.HUnit

import qualified UHF.Lexer as Lexer

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "Lexer" ~: Lexer.tests
            ]
