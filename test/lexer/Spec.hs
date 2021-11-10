module Main where

import Test.HUnit

import qualified UHF.Lexer as Lexer
import qualified UHF.Lexer.DFA as DFA

main :: IO ()
main =
    runTestTTAndExit $ test
        [ "Lexer" ~: Lexer.tests
        , "DFA" ~: DFA.tests
        ]
