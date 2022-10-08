module UHF.IO.Path where

import UHF.Util.Prelude

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

replace_extension :: FilePath -> FilePath -> FilePath
replace_extension ext p =
    let dropped = dropWhile (/= '.') $ reverse p
    in if null dropped
        then p ++ "." ++ ext
        else reverse dropped ++ ext

case_replace_extension :: Assertion
case_replace_extension = "thing.def" @=? replace_extension "def" "thing.abc"

case_replace_extension_no_extension :: Assertion
case_replace_extension_no_extension = "thing.def" @=? replace_extension "def" "thing"

tests :: TestTree
tests = $(testGroupGenerator)
