{-# LANGUAGE TemplateHaskell #-}

module CompileTests (get_compile_tests) where

import UHF.Prelude

import qualified Language.Haskell.TH as TH
import qualified System.FilePath.Glob as Glob
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory

get_compile_tests :: IO TestTree
get_compile_tests = do
    filepaths <- compile_test_filepaths
    pure $ testGroup "compile tests" $ map make_test_case filepaths -- TODO

compile_test_filepaths :: IO [FilePath]
compile_test_filepaths = Glob.globDir1 compile_test_filepath_pattern compile_test_search_directory
    where
        compile_test_filepath_pattern = Glob.compile "**/*.uhf"

        compile_test_search_directory =
            $(do
                dir <- TH.runIO Directory.getCurrentDirectory
                filename <- TH.loc_filename <$> TH.location
                TH.litE $ TH.stringL $ dir FilePath.</> filename
             )

make_test_case :: FilePath -> TestTree
make_test_case test_path = testCase test_path _
