module UHF.IO.Path where

import Test.Tasty.HUnit

replace_extension :: String -> String -> String
replace_extension ext p =
    let dropped = dropWhile (/= '.') $ reverse p
    in if null dropped
        then p ++ "." ++ ext
        else reverse dropped ++ ext

case_replace_extension :: Assertion
case_replace_extension = "thing.def" @=? replace_extension "def" "thing.abc"

case_replace_eextension_no_extension :: Assertion
case_replace_eextension_no_extension = "thing.def" @=? replace_extension "def" "thing"
