module Main where

import Test.HUnit
import Brace.ArgParser (tests)

main :: IO ()
main = runTestTT tests >> return ()
