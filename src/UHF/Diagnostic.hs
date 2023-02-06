{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic
    ( Error(..)
    , Warning(..)
    , Section
    , section_contents

    , IsError(..)
    , IsWarning(..)
    , ToSection(..)
    , to_section
    , report
    , report_diagnostics
    ) where

import UHF.Util.Prelude

import qualified UHF.FormattedString as FormattedString
import qualified UHF.Diagnostic.Codes.Code as Code
import qualified UHF.Diagnostic.Colors as Colors
import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified System.IO as IO

data Error = Error Code.Error (Maybe Location.Span) [Section]
data Warning = Warning Code.Warning (Maybe Location.Span) [Section]

class IsError e where
    to_error :: e -> Error
class IsWarning w where
    to_warning :: w -> Warning

class IsDiagnostic d where
    to_diagnostic :: d -> (FormattedString.FormattedString, Maybe (Text, Text), Maybe Location.Span, [Section])

instance IsDiagnostic Error where
    to_diagnostic (Error c sp sections) = (FormattedString.color_text Colors.error "error", Code.error_code_desc c, sp, sections)
instance IsDiagnostic Warning where
    to_diagnostic (Warning c sp sections) = (FormattedString.color_text Colors.warning "warning", Code.warning_code_desc c, sp, sections)

newtype Section = Section { section_contents :: [Line.Line] }
class ToSection s where
    to_section' :: s -> [Line.Line]
instance ToSection [Line.Line] where
    to_section' = identity

to_section :: ToSection s => s -> Section
to_section = Section . to_section'

report :: IsDiagnostic d => Handle -> d -> IO ()
report handle d =
    let (type_str, code_and_desc, m_sp, sections) = to_diagnostic d
        header =
            type_str
            {-
            (case Code.code_type code of
                Code.Error ->
                Code.Warning -> FormattedString.color_text Colors.warning "warning"
                Code.DebugMessage -> FormattedString.color_text Colors.debug_message "debug message"
                Code.InternalError -> FormattedString.color_text Colors.error "internal error") -}
            <>
            (case m_sp of
                Just sp -> " at " <> format sp
                Nothing -> "")

        footer =
            case code_and_desc of
                Just (c, d) -> Just $ "==> [" <> FormattedString.color_text Colors.diag_code c <> "] " <> FormattedString.color_text Colors.diag_desc d
                Nothing -> Nothing

        section_lines = concatMap (\ (Section l) -> l) sections
        indent =
            if null section_lines
                then 4
                else maximum $ map (Text.length . Line.prefix) section_lines

        p_line l =
            let pre = Line.prefix l
                sep = Line.separ l
                contents = Line.contents l
            in
            hPutStr handle (replicate (indent - Text.length pre) ' ') >>
            hPutStr handle pre >>
            hPutStr handle [' ', sep, ' '] >>
            hPutStr handle contents >>
            hPutText handle "\n"
    in
    hPutStr handle header >>
    hPutText handle "\n" >>
    mapM_ p_line section_lines >>
    maybe (pure ()) (\ f -> hPutStr handle (replicate indent ' ') >> hPutStr handle f >> IO.hPutStr handle "\n") footer

report_diagnostics :: IsDiagnostic d => Handle -> [d] -> IO ()
report_diagnostics handle = mapM_ (report handle)
