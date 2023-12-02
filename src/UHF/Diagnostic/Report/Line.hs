module UHF.Diagnostic.Report.Line
    ( Line
    , prefix
    , separ
    , contents

    , file_line
    , elipsis_line
    , numbered_line
    , heading_line
    , other_line

    , compare_lines
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text

import UHF.IO.File (File)
import qualified UHF.Diagnostic.Report.Style as Style
import qualified UHF.IO.File as File
import qualified UHF.IO.FormattedString as FormattedString

data Line = Line { prefix :: Text, separ :: Char, contents :: FormattedString.FormattedString } deriving (Show, Eq)

file_line :: Style.Style -> File -> Line
file_line style f = Line "" (Style.file_line_char style) (FormattedString.color_text (Style.file_path_color style) $ Text.pack $ File.path f)

elipsis_line :: Style.Style -> Line
elipsis_line style = Line "..." (Style.other_line_char style) "..."

numbered_line :: Style.Style -> Int -> FormattedString.FormattedString -> Line
numbered_line style num = Line (Text.pack $ show num) (Style.other_line_char style)

heading_line :: Style.Style -> FormattedString.FormattedString -> Line
heading_line style = Line "" (Style.header_line_char style)

other_line :: Style.Style -> FormattedString.FormattedString -> Line
other_line style = Line "" (Style.other_line_char style)

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
