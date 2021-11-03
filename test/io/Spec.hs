module Main where

import Test.HUnit

import qualified UHF.IO.Path as Path (tests)

main :: IO ()
main =
    runTestTTAndExit $ test
            [ "IO.Path" ~: Path.tests
            ]
