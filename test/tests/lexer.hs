module Main where

import Test.Tasty

import qualified UHF.Lexer.MainLexer (tests)
import qualified UHF.Lexer.PostProcess (tests)
import qualified UHF.Lexer.DFA (tests)

main :: IO ()
main = defaultMain $ testGroup "lexer" [UHF.Lexer.MainLexer.tests, UHF.Lexer.PostProcess.tests, UHF.Lexer.DFA.tests]
