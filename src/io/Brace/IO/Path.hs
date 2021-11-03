module Brace.IO.Path where

import Test.HUnit

replace_extension :: String -> String -> String
replace_extension ext p =
    let dropped = dropWhile (/= '.') $ reverse p
    in if null dropped
        then p ++ "." ++ ext
        else reverse dropped ++ ext

tests :: Test
tests = test
    [ "thing.def" ~=? replace_extension "def" "thing.abc"
    , "thing.def" ~=? replace_extension "def" "thing"
    ]
