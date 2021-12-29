module Main where

import Test.Tasty

import qualified UHF.ArgParser (tests)
import qualified UHF.ArgParser.Help (tests)
import qualified UHF.ArgParser.Parser (tests)

main :: IO ()
main = defaultMain $ testGroup "argparser" [UHF.ArgParser.tests, UHF.ArgParser.Help.tests, UHF.ArgParser.Parser.tests]
