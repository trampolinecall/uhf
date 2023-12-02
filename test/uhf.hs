module Main where

import UHF.Prelude

import qualified UHF.Util.Arena (tests)
import qualified UHF.Diagnostic.Report.Messages (tests)
import qualified UHF.Diagnostic.Report.Utils (tests)
import qualified UHF.Source.Location (tests)
import qualified UHF.PP.Precedence (tests)
import qualified UHF.Phases.Lexer (tests)
import qualified UHF.Phases.Parser (tests)
import qualified UHF.Phases.Parser.PEG (tests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "tests"
            [ Arena.tests
            , UHF.Diagnostic.Report.Messages.tests
            , UHF.Diagnostic.Report.Utils.tests
            , UHF.Source.Location.tests
            , UHF.Phases.Lexer.tests
            , UHF.Phases.Parser.PEG.tests
            , UHF.Phases.Parser.tests
            , UHF.PP.Precedence.tests
            ]
