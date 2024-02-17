module Main where

import UHF.Prelude

import qualified UHF.Diagnostic.Styles.Default.RenderMessagesSection (tests)
import qualified UHF.Diagnostic.Styles.Default.Utils (tests)
import qualified UHF.PP.Precedence (tests)
import qualified UHF.Parts.Lexer (tests)
import qualified UHF.Parts.Parser (tests)
import qualified UHF.Parts.Parser.PEG (tests)
import qualified UHF.Source.Location (tests)
import qualified UHF.Util.Arena (tests)

main :: IO ()
main =
    defaultMain $
        testGroup
            "tests"
            [ UHF.Diagnostic.Styles.Default.RenderMessagesSection.tests
            , UHF.Diagnostic.Styles.Default.Utils.tests
            , UHF.PP.Precedence.tests
            , UHF.Parts.Lexer.tests
            , UHF.Parts.Parser.PEG.tests
            , UHF.Parts.Parser.tests
            , UHF.Source.Location.tests
            , UHF.Util.Arena.tests
            ]
