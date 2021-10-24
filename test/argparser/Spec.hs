module Main where

import Test.HUnit

import qualified Brace.ArgParser.Parser as Parser (tests)

main :: IO ()
main = runTestTTAndExit Parser.tests
