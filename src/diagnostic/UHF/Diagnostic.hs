{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic
    ( Diagnostic(..)
    , Section

    , ToSection(..)
    , to_section
    , report
    ) where

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Code as Code
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.IO as IO

data Diagnostic = Diagnostic Code.Code (Maybe Location.Span) [Section]

data Section = Section [(Text.Text, Char, FormattedString.FormattedString)]
class ToSection s where
    to_section' :: s -> [(Text.Text, Char, FormattedString.FormattedString)]

to_section :: ToSection s => s -> Section
to_section = Section . to_section'

report :: IO.Handle -> Diagnostic -> IO ()
report handle (Diagnostic code m_sp sections) =
    let header = FormattedString.make_formatted_string $
            case Code.code_type code of
                Code.Error -> [(Colors.error, "error")]
                Code.Warning -> [(Colors.warning, "warning")]
                Code.DebugMessage -> [(Colors.debug_message, "debug message")]
                Code.InternalError -> [(Colors.error, "internal error")]
            ++
            case m_sp of
                Just sp -> [([], " at "), (Colors.file_path, Text.pack $ Location.fmt_span sp)]
                Nothing -> []

        footer =
            case Code.code_code_desc code of
                Just (c, d) -> Just $ FormattedString.make_formatted_string [([], "==> ["), (Colors.diag_code, c), ([], "] "), (Colors.diag_desc, d)]
                Nothing -> Nothing

        section_lines = concatMap (\ (Section l) -> l) sections
        indent = maximum $ map (\ (p, _, _) -> Text.length p) section_lines

        p_fmtstr = FormattedString.render_formatted_string handle FormattedString.AutoDetect

        p_line (prefix, sep, str) =
            IO.hPutStr handle (replicate (indent - Text.length prefix) ' ') >>
            Text.IO.hPutStr handle prefix >>
            IO.hPutStr handle [' ', sep, ' '] >>
            p_fmtstr str >>
            IO.hPutStr handle "\n"
    in
    p_fmtstr header >>
    IO.hPutStr handle "\n" >>
    sequence_ (map p_line section_lines) >>
    maybe (return ()) (\ f -> IO.hPutStr handle (replicate indent ' ') >> p_fmtstr f) footer
