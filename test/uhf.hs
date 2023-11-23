module Main where

import UHF.Util.Prelude

import qualified Arena (tests)
import qualified UHF.Diagnostic.Report.Messages (tests)
import qualified UHF.Diagnostic.Report.Utils (tests)
import qualified UHF.IO.Location (tests)
import qualified UHF.Phases.Lexer.IdentifierGrouper (tests)
import qualified UHF.Phases.Lexer.MainLexer (tests)
import qualified UHF.Phases.Parser (tests)
import qualified UHF.Phases.Parser.PEG (tests)
import qualified UHF.PP.Precedence (tests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "tests"
            [ Arena.tests
            , UHF.Diagnostic.Report.Messages.tests
            , UHF.Diagnostic.Report.Utils.tests
            , UHF.IO.Location.tests
            , UHF.Phases.Lexer.IdentifierGrouper.tests
            , UHF.Phases.Lexer.MainLexer.tests
            , UHF.Phases.Parser.PEG.tests
            , UHF.Phases.Parser.tests
            , UHF.PP.Precedence.tests
            ]
