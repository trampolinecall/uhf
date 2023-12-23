module CompileTests (get_compile_tests) where

import UHF.Prelude

get_compile_tests :: IO TestTree
get_compile_tests = pure $ testGroup "compile tests" [] -- TODO

