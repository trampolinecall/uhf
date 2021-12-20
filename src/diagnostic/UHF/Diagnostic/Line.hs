{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic.Line
    ( Line
    , prefix
    , separ
    , contents

    , file_line
    , elipsis_line
    , numbered_line
    , other_line

    , compare_line
    , compare_many_lines
    , compare_many_lines'

    ) where

import Test.Tasty.HUnit

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.File as File

import qualified System.Console.ANSI as ANSI
import qualified Data.Text as Text

data Line = Line { prefix :: Text.Text, separ :: Char, contents :: FormattedString.FormattedString } deriving (Show, Eq)

file_line :: File.File -> Line
file_line f = Line "" '>' (FormattedString.make_formatted_string [(Colors.file_path, Text.pack $ File.path f)])

elipsis_line :: Line
elipsis_line = Line "..." '|' (FormattedString.make_formatted_string [([], "...")])

numbered_line :: Int -> FormattedString.FormattedString -> Line
numbered_line num = Line (Text.pack $ show num) '|'

other_line :: FormattedString.FormattedString -> Line
other_line = Line "" '|'

compare_line :: Text.Text -> Char -> [(Char, [ANSI.SGR])] -> String -> String -> Line -> Bool
compare_line pre sep bindings text sgrs (Line line_pre line_sep line_after) =
    pre == line_pre &&
    sep == line_sep &&
    FormattedString.compare_formatted_string bindings text sgrs line_after

compare_many_lines :: [(Char, [ANSI.SGR])] -> [(Text.Text, Char, String, String)] -> [Line] -> Bool
compare_many_lines bindings line_expectations =
    let c ((pre, sep, text, sgrs), l) = compare_line pre sep bindings text sgrs l
    in all c . zip line_expectations

compare_many_lines' :: [(Char, [ANSI.SGR])] -> [(Text.Text, Char, String, String)] -> [Line] -> Assertion
compare_many_lines' bindings line_expectations lns =
    let expectations_str = concatMap
            (\ (pre, ch, text, formats) ->
                Text.unpack pre ++ " " ++ [ch] ++ " " ++ show text ++ "\n" ++ replicate (Text.length pre + 3) ' ' ++ show formats ++ "\n")
            line_expectations

        lns_str = concatMap
            (\ (Line pre ch fmstr) ->
                let (fmstr_text, fmstr_formats) = FormattedString.formatted_string_contents_and_formats bindings fmstr
                in Text.unpack pre ++ " " ++ [ch] ++ " " ++ show fmstr_text ++ "\n" ++ replicate (Text.length pre + 3) ' ' ++ show fmstr_formats ++ "\n"
            )
            lns

    in assertBool ("expected\n" ++ expectations_str ++ "got\n" ++ lns_str) $ length lns == length line_expectations && compare_many_lines bindings line_expectations lns
