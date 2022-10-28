module UHF.Diagnostic.Line
    ( Line
    , prefix
    , separ
    , contents

    , file_line
    , elipsis_line
    , numbered_line
    , other_line

    , compare_lines
    ) where

import UHF.Util.Prelude


import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.File as File

import qualified Data.Text as Text

data Line = Line { prefix :: Text, separ :: Char, contents :: FormattedString.FormattedString } deriving (Show, Eq)

file_line :: File.File -> Line
file_line f = Line "" '>' (FormattedString.color_text Colors.file_path $ Text.pack $ File.path f)

elipsis_line :: Line
elipsis_line = Line "..." '|' "..."

numbered_line :: Int -> FormattedString.FormattedString -> Line
numbered_line num = Line (Text.pack $ show num) '|'

other_line :: FormattedString.FormattedString -> Line
other_line = Line "" '|'

compare_lines :: [(Text, Char, Text)] -> [Line] -> Assertion
compare_lines expect lns =
    let expectations_str = Text.concat $ map
            (\ (pre, ch, text) -> pre <> " " <> Text.pack [ch] <> " " <> show text <> "\n")
            expect

        lns_str = Text.concat $ map
            (\ (Line pre ch fmstr) ->
                let fmstr_text = FormattedString.flatten_no_sgr fmstr
                in pre <> " " <> Text.pack [ch] <> " " <> fmstr_text <> "\n"
            )
            lns

        lns_flattened = map (\ (Line pre ch fmstr) -> (pre, ch, FormattedString.flatten_no_sgr fmstr)) lns

        error_msg = "expected\n" ++ Text.unpack expectations_str ++ "got\n" ++ Text.unpack lns_str

    in assertBool ("number of lines mismatch\n" ++ error_msg) (length lns == length expect) >>
    assertBool ("mismatch\n" ++ error_msg) (expect == lns_flattened)
