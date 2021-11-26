module Main where

import Test.Tasty

import qualified UHF.Diagnostic.FormattedString (tests)

main :: IO ()
main = defaultMain $ testGroup "diagnostic" [UHF.Diagnostic.FormattedString.tests]
