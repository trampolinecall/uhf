module Main where

import UHF.Util.Prelude

import qualified Arena (tests)

import qualified UHF.Diagnostic.Report.Messages (tests)
import qualified UHF.Diagnostic.Report.Utils (tests)

import qualified UHF.IO.Location (tests)

import qualified UHF.Phases.Front.Lexer.MainLexer (tests)
import qualified UHF.Phases.Front.Lexer.IdentifierGrouper (tests)

import qualified UHF.Phases.Front.Parser (tests)
import qualified UHF.Phases.Front.Parser.PEG (tests)

main :: IO ()
main = defaultMain $
    testGroup "tests"
        [ UHF.Diagnostic.Report.Messages.tests
        , UHF.Diagnostic.Report.Utils.tests
        , UHF.Phases.Front.Lexer.MainLexer.tests
        , UHF.Phases.Front.Lexer.IdentifierGrouper.tests
        , UHF.IO.Location.tests
        , UHF.Phases.Front.Parser.tests
        , UHF.Phases.Front.Parser.PEG.tests
        , Arena.tests
        ]
