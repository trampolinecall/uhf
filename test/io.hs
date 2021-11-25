module Main where

import Test.Tasty

import qualified UHF.IO.Path (tests)

main :: IO ()
main = defaultMain $ testGroup "io" [UHF.IO.Path.tests]
