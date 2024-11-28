{-# LANGUAGE TemplateHaskell #-}

module CompileTests (get_compile_tests) where

import UHF.Prelude

import qualified Data.Array as Array
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Language.Haskell.TH as TH
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified System.FilePath.Glob as Glob
import qualified Text.Regex.PCRE as Regex

import qualified UHF.Compiler as Compiler
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Driver as Driver
import qualified UHF.Source.Location as Location
import UHF.Source.Span (Span)
import qualified UHF.Source.Span as Span

get_compile_tests :: IO TestTree
get_compile_tests = do
    filepaths <- compile_test_filepaths
    pure $ testGroup "compile tests" $ map make_test_case filepaths -- TODO

compile_test_filepaths :: IO [FilePath]
compile_test_filepaths = Glob.globDir1 compile_test_filepath_pattern compile_test_search_directory
    where
        compile_test_filepath_pattern = Glob.compile "**/*.uhf"

        compile_test_search_directory =
            $( do
                dir <- TH.runIO Directory.getCurrentDirectory
                filename <- TH.loc_filename <$> TH.location
                TH.litE $ TH.stringL $ FilePath.takeDirectory $ dir FilePath.</> filename
             )

make_test_case :: FilePath -> TestTree
make_test_case test_path =
    testCase test_path $ do
        test_file_contents <- Text.IO.readFile test_path
        let (expected_errors, expected_warnings) = parse_expectations test_file_contents

        compile_result <- Driver.compile_returning_diagnostics $ Driver.CompileOptions test_path Nothing [Driver.Check]
        let (compile_errors, compile_warnings) = case compile_result of
                Right () -> mempty
                Left (Compiler.Diagnostics es ws) -> (toList es, toList ws)

        let (_, unexpected_errors, unmatched_error_expects) =
                match_up
                    (\(Diagnostic.Error sp msg _ _) expect -> expect_matches sp msg expect)
                    compile_errors
                    expected_errors

        let (_, unexpected_warnings, unmatched_warning_expects) =
                match_up
                    (\(Diagnostic.Warning sp msg _ _) expect -> expect_matches sp msg expect)
                    compile_warnings
                    expected_warnings

        mapM_ (\(nr, msg) -> assertFailure $ "no match for expected error '" ++ Text.unpack msg ++ "' on line " ++ show nr) unmatched_error_expects
        mapM_ (\(nr, msg) -> assertFailure $ "no match for expected warning '" ++ Text.unpack msg ++ "' on line " ++ show nr) unmatched_warning_expects
        mapM_
            ( \(Diagnostic.Error m_sp msg _ _) -> assertFailure $ "unexpected error with message '" ++ Text.unpack msg ++ "'" ++ maybe "" (\sp -> " at " ++ Text.unpack (format sp)) m_sp
            )
            unexpected_errors
        mapM_
            ( \(Diagnostic.Warning m_sp msg _ _) -> assertFailure $ "unexpected warning with message '" ++ Text.unpack msg ++ "'" ++ maybe "" (\sp -> " at " ++ Text.unpack (format sp)) m_sp
            )
            unexpected_warnings
    where
        parse_expectations test_file_contents =
            let test_file_contents_str = Text.unpack test_file_contents

                expected_errors = convert_expects (test_file_contents_str Regex.=~ ("EXPECT compile error \"([^\"]+)\"" :: [Char]) :: [Regex.MatchText [Char]])
                expected_warnings = convert_expects (test_file_contents_str Regex.=~ ("EXPECT compile warning \"([^\"]+)\"" :: [Char]) :: [Regex.MatchText [Char]])
                noloc_expected_errors = convert_noloc_expects (test_file_contents_str Regex.=~ ("EXPECT anywhere compile error \"([^\"]+)\"" :: [Char]) :: [Regex.MatchText [Char]])
                noloc_expected_warnings = convert_noloc_expects (test_file_contents_str Regex.=~ ("EXPECT anywhere compile warning \"([^\"]+)\"" :: [Char]) :: [Regex.MatchText [Char]])
            in (expected_errors ++ noloc_expected_errors, expected_warnings ++ noloc_expected_warnings)
            where
                offset_to_line_number offset = Text.count "\n" (Text.take offset test_file_contents) + 1
                convert_expects =
                    map
                        ( \expectation ->
                            let (_, (offset, _)) = expectation Array.! 0
                                line_nr = offset_to_line_number offset
                                (message, _) = expectation Array.! 1
                            in (Just line_nr, Text.pack message)
                        )

                convert_noloc_expects =
                    map
                        ( \expectation ->
                            let (message, _) = expectation Array.! 1
                            in (Nothing, Text.pack message)
                        )

        expect_matches :: Maybe Span -> Text -> (Maybe Int, Text) -> Bool
        expect_matches sp msg (ex_ln, ex_msg) =
            let sp_ln = Location.lc_row . Location.lc . Span.start <$> sp
            in ex_ln == sp_ln && ex_msg == msg

match_up :: (a -> b -> Bool) -> [a] -> [b] -> ([(a, b)], [a], [b])
match_up check_match = go [] [] []
    where
        go matches leftover_a leftover_b [] bs = (matches, leftover_a, leftover_b ++ bs)
        go matches leftover_a leftover_b (a : as) bs =
            case List.findIndex (check_match a) bs of
                Just b_ind ->
                    let (matching_b, bs') = case List.splitAt b_ind bs of
                            (bs_before, matching_b : bs_after) -> (matching_b, bs_before ++ bs_after)
                            (_, []) -> unreachable
                    in go (matches ++ [(a, matching_b)]) leftover_a leftover_b as bs'
                Nothing ->
                    -- a has no match
                    go matches (leftover_a ++ [a]) leftover_b as bs
