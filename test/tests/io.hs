module Main where

import Test.Tasty

import qualified UHF.IO.Location (tests)
import qualified UHF.IO.Path (tests)

main :: IO ()
main = defaultMain $ testGroup "io" [UHF.IO.Location.tests, UHF.IO.Path.tests]
