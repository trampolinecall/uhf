module Main where

import Test.Tasty

import qualified UHF.Lexer (tests)
import qualified UHF.Lexer.DFA (tests)

main :: IO ()
main = defaultMain $ testGroup "lexer" [UHF.Lexer.tests, UHF.Lexer.DFA.tests]
