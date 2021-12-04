{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic.Line where

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.File as File

import qualified Data.Text as Text

type Line = (Text.Text, Char, FormattedString.FormattedString)

file_line :: File.File -> Line
file_line f = ("", '>', FormattedString.make_formatted_string [(Colors.file_path, Text.pack $ File.path f)])

elipsis_line :: Line
elipsis_line = ("...", '|', FormattedString.make_formatted_string [([], "...")])
