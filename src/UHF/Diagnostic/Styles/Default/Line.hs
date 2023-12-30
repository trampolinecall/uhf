module UHF.Diagnostic.Styles.Default.Line
    ( Line
    , print
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

import UHF.Prelude

import qualified Data.Text as Text
import qualified System.IO as IO

import UHF.Source.File (File)
import qualified UHF.Diagnostic.Styles.Default.Options as Options
import qualified UHF.Source.File as File
import qualified UHF.Source.FormattedString as FormattedString

data Line = Line { prefix :: Text, separ :: Char, contents :: FormattedString.FormattedString } deriving (Show, Eq)

print :: FormattedString.ColorsNeeded -> Int -> IO.Handle -> Line -> IO ()
print c_needed indent handle (Line pre sep contents) =
    hPutStr handle (replicate (indent - Text.length pre) ' ') >>
    hPutStr handle pre >>
    hPutStr handle [' ', sep, ' '] >>
    FormattedString.render handle c_needed contents >>
    hPutText handle "\n"

file_line :: Options.Options -> File -> Line
file_line style f = Line "" (Options.file_line_char style) (FormattedString.color_text (Options.file_path_color style) $ Text.pack $ File.path f)

elipsis_line :: Options.Options -> Line
elipsis_line style = Line "..." (Options.other_line_char style) "..."

numbered_line :: Options.Options -> Int -> FormattedString.FormattedString -> Line
numbered_line style num = Line (Text.pack $ show num) (Options.other_line_char style)

heading_line :: Options.Options -> FormattedString.FormattedString -> Line
heading_line style = Line "" (Options.header_line_char style)

other_line :: Options.Options -> FormattedString.FormattedString -> Line
other_line style = Line "" (Options.other_line_char style)

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
