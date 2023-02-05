module Main where

import UHF.Util.Prelude

import qualified Arena (tests)

import qualified UHF.Diagnostic.Sections.Underlines (tests)
import qualified UHF.Diagnostic.Sections.Utils (tests)

import qualified UHF.IO.Location (tests)
import qualified UHF.IO.Path (tests)

import qualified UHF.Lexer.MainLexer (tests)
import qualified UHF.Lexer.IdentifierGrouper (tests)

import qualified UHF.Parser (tests)
import qualified UHF.Parser.PEG (tests)

main :: IO ()
main = defaultMain $
    testGroup "tests"
        [ UHF.Diagnostic.Sections.Underlines.tests
        , UHF.Diagnostic.Sections.Utils.tests
        , UHF.Lexer.MainLexer.tests
        , UHF.Lexer.IdentifierGrouper.tests
        , UHF.IO.Location.tests
        , UHF.IO.Path.tests
        , UHF.Parser.tests
        , UHF.Parser.PEG.tests
        , Arena.tests
        ]
