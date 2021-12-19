{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic.Line where

import Test.Tasty.HUnit

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.File as File

import qualified System.Console.ANSI as ANSI
import qualified Data.Text as Text

type Line = (Text.Text, Char, FormattedString.FormattedString)

file_line :: File.File -> Line
file_line f = ("", '>', FormattedString.make_formatted_string [(Colors.file_path, Text.pack $ File.path f)])

elipsis_line :: Line
elipsis_line = ("...", '|', FormattedString.make_formatted_string [([], "...")])

compare_line :: Text.Text -> Char -> [(Char, [ANSI.SGR])] -> String -> String -> Line -> Bool
compare_line prefix sep bindings text sgrs (line_prefix, line_sep, line_after) =
    prefix == line_prefix &&
    sep == line_sep &&
    FormattedString.compare_formatted_string bindings text sgrs line_after

compare_many_lines :: [(Char, [ANSI.SGR])] -> [(Text.Text, Char, String, String)] -> [Line] -> Bool
compare_many_lines bindings line_expectations =
    let c ((pre, sep, text, sgrs), l) = compare_line pre sep bindings text sgrs l
    in all c . zip line_expectations

compare_many_lines' :: [(Char, [ANSI.SGR])] -> [(Text.Text, Char, String, String)] -> [Line] -> Assertion
compare_many_lines' bindings line_expectations lns =
    let expectations_str = concatMap
            (\ (prefix, ch, text, formats) ->
                Text.unpack prefix ++ " " ++ [ch] ++ " " ++ show text ++ "\n" ++ replicate (Text.length prefix + 3) ' ' ++ show formats ++ "\n")
            line_expectations

        lns_str = concatMap
            (\ (prefix, ch, fmstr) ->
                let (fmstr_text, fmstr_formats) = FormattedString.formatted_string_contents_and_formats bindings fmstr
                in Text.unpack prefix ++ " " ++ [ch] ++ " " ++ show fmstr_text ++ "\n" ++ replicate (Text.length prefix + 3) ' ' ++ show fmstr_formats ++ "\n"
            )
            lns

    in assertBool ("expected\n" ++ expectations_str ++ "got\n" ++ lns_str) $ length lns == length line_expectations && compare_many_lines bindings line_expectations lns
