module Main where

import Brace.Testing

import Brace.ArgParser (tests)

main :: IO ()
main = run_test_suite $ TestSuite tests
