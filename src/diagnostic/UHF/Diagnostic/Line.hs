{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic.Line where

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
