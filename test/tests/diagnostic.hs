module Main where

import Test.Tasty

import qualified UHF.Diagnostic.FormattedString (tests)
import qualified UHF.Diagnostic.Sections.Underlines (tests)
import qualified UHF.Diagnostic.Sections.Utils (tests)

main :: IO ()
main = defaultMain $ testGroup "diagnostic" [UHF.Diagnostic.FormattedString.tests, UHF.Diagnostic.Sections.Underlines.tests, UHF.Diagnostic.Sections.Utils.tests]
