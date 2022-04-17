module Main where

import Test.Tasty

import qualified UHF.ArgParser (tests)
import qualified UHF.ArgParser.Help (tests)
import qualified UHF.ArgParser.Parser (tests)

import qualified UHF.Diagnostic.FormattedString (tests)
import qualified UHF.Diagnostic.Sections.Underlines (tests)
import qualified UHF.Diagnostic.Sections.Utils (tests)

import qualified UHF.IO.Location (tests)
import qualified UHF.IO.Path (tests)

import qualified UHF.Lexer.MainLexer (tests)
import qualified UHF.Lexer.PostProcess (tests)
import qualified UHF.Lexer.DFA (tests)

main :: IO ()
main = defaultMain $
    testGroup "tests"
        [ UHF.ArgParser.tests
        , UHF.ArgParser.Help.tests
        , UHF.ArgParser.Parser.tests
        , UHF.Lexer.MainLexer.tests
        , UHF.Lexer.PostProcess.tests
        , UHF.Lexer.DFA.tests
        , UHF.IO.Location.tests
        , UHF.IO.Path.tests
        , UHF.Diagnostic.FormattedString.tests
        , UHF.Diagnostic.Sections.Underlines.tests
        , UHF.Diagnostic.Sections.Utils.tests
        ]
