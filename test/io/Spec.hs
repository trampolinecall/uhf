module Main where

import Test.HUnit

import qualified Brace.IO.Path as Path (tests)

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "IO.Path" ~: Path.tests
            ]
