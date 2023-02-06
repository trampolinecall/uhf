{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic
    ( Error(..)
    , Warning(..)
    , DebugMessage(..)
    , InternalError(..)

    , IsError(..)
    , IsWarning(..)

    , DiagnosticContents(..)

    , report
    ) where

import UHF.Util.Prelude

import qualified UHF.FormattedString as FormattedString
import qualified UHF.Diagnostic.Codes.Code as Code
import qualified UHF.Diagnostic.Colors as Colors
import qualified UHF.Diagnostic.Section as Section
import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified System.IO as IO

data Error = Error Code.Error DiagnosticContents
data Warning = Warning Code.Warning DiagnosticContents
data DebugMessage = DebugMessage DiagnosticContents
data InternalError = InternalError DiagnosticContents

data DiagnosticContents = DiagnosticContents (Maybe Location.Span) [Section.Section]

class IsError e where
    to_error :: e -> Error
class IsWarning w where
    to_warning :: w -> Warning

class IsDiagnostic d where
    to_diagnostic :: d -> (FormattedString.FormattedString, Maybe (Text, Text), DiagnosticContents)

instance IsDiagnostic Error where
    to_diagnostic (Error c contents) = (FormattedString.color_text Colors.error "error", Code.error_code_desc c, contents)
instance IsDiagnostic Warning where
    to_diagnostic (Warning c contents) = (FormattedString.color_text Colors.warning "warning", Code.warning_code_desc c, contents)
instance IsDiagnostic DebugMessage where
    to_diagnostic (DebugMessage contents) = (FormattedString.color_text Colors.debug_message "debug message", Nothing, contents)
instance IsDiagnostic InternalError where
    to_diagnostic (InternalError contents) = (FormattedString.color_text Colors.error "internal error", Nothing, contents)

report :: IsDiagnostic d => Handle -> d -> IO ()
report handle d =
    let (type_str, code_and_desc, (DiagnosticContents m_sp sections)) = to_diagnostic d
        header =
            type_str
            <>
            (case m_sp of
                Just sp -> " at " <> format sp
                Nothing -> "")

        footer =
            case code_and_desc of
                Just (c, d) -> Just $ "==> [" <> FormattedString.color_text Colors.diag_code c <> "] " <> FormattedString.color_text Colors.diag_desc d
                Nothing -> Nothing

        section_lines = concatMap (\ (Section.Section l) -> l) sections
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
