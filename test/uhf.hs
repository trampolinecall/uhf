module Main where

import UHF.Prelude

import qualified CompileTests

import qualified UHF.Diagnostic.Styles.Default.RenderMessagesSection (tests)
import qualified UHF.Diagnostic.Styles.Default.Utils (tests)
import qualified UHF.PP.Precedence (tests)
import qualified UHF.Parts.Lexer (tests)
import qualified UHF.Parts.SolveTypes.Error.InferVarNamer (tests)
import qualified UHF.Source.Location (tests)
import qualified UHF.Util.Arena (tests)

main :: IO ()
main = do
    compile_tests <- CompileTests.get_compile_tests
    defaultMain $
        testGroup
            "tests"
            [ unit_tests
            , compile_tests
            ]

unit_tests :: TestTree
unit_tests =
    testGroup
        "tests"
        [ UHF.Diagnostic.Styles.Default.RenderMessagesSection.tests
        , UHF.Diagnostic.Styles.Default.Utils.tests
        , UHF.PP.Precedence.tests
        , UHF.Parts.Lexer.tests
        , UHF.Parts.SolveTypes.Error.InferVarNamer.tests
        , UHF.Source.Location.tests
        , UHF.Util.Arena.tests
        ]
