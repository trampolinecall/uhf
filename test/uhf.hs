module Main where

import UHF.Prelude

import qualified CompileTests

import qualified UHF.Diagnostic.Report.Messages (tests)
import qualified UHF.Diagnostic.Report.Utils (tests)
import qualified UHF.PP.Precedence (tests)
import qualified UHF.Parts.Lexer (tests)
import qualified UHF.Parts.Parser (tests)
import qualified UHF.Parts.Parser.PEG (tests)
import qualified UHF.Source.Location (tests)
import qualified UHF.Util.Arena (tests)

main :: IO ()
main = do
    compile_tests <- CompileTests.get_compile_tests
    defaultMain $
        testGroup "tests"
        [ unit_tests
        , compile_tests
        ]

unit_tests :: TestTree
unit_tests =
    testGroup
        "unit tests"
        [ UHF.Diagnostic.Report.Messages.tests
        , UHF.Diagnostic.Report.Utils.tests
        , UHF.PP.Precedence.tests
        , UHF.Parts.Lexer.tests
        , UHF.Parts.Parser.PEG.tests
        , UHF.Parts.Parser.tests
        , UHF.Source.Location.tests
        , UHF.Util.Arena.tests
        ]
