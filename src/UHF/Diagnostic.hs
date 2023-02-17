{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

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
import qualified UHF.Diagnostic.Sections.Messages as Messages
import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified System.IO as IO

data Error = Error Code.Error DiagnosticContents
data Warning = Warning Code.Warning DiagnosticContents
newtype DebugMessage = DebugMessage DiagnosticContents
newtype InternalError = InternalError DiagnosticContents

data DiagnosticContents = DiagnosticContents (Maybe Location.Span) Text Messages.MessagesSection [Section.SomeSection]

class IsError e where
    to_error :: e -> Error
class IsWarning w where
    to_warning :: w -> Warning

class IsDiagnostic d where
    to_diagnostic :: d -> (FormattedString.FormattedString, Maybe (Text, Text), Messages.Type, DiagnosticContents)

instance IsDiagnostic Error where
    to_diagnostic (Error c contents) = (FormattedString.color_text Colors.error "error", Code.error_code_desc c, Messages.Error, contents)
instance IsDiagnostic Warning where
    to_diagnostic (Warning c contents) = (FormattedString.color_text Colors.warning "warning", Code.warning_code_desc c, Messages.Warning, contents)
instance IsDiagnostic DebugMessage where
    to_diagnostic (DebugMessage contents) = (FormattedString.color_text Colors.debug_message "debug message", Nothing, Messages.Note, contents)
instance IsDiagnostic InternalError where
    to_diagnostic (InternalError contents) = (FormattedString.color_text Colors.error "internal error", Nothing, Messages.Error, contents)

report :: IsDiagnostic d => Handle -> d -> IO ()
report handle d =
    let (type_str, code_and_desc, diag_message_type, DiagnosticContents m_sp main_message main_section sections) = to_diagnostic d
        header =
            (case m_sp of
                Just sp -> format (Location.sp_s sp) <> ": "
                Nothing -> "")
                <> type_str <> ": "
                <> convert_str main_message

        footer =
            case code_and_desc of
                Just (c, d) -> Just $ "==> [" <> FormattedString.color_text Colors.diag_code c <> "] " <> FormattedString.color_text Colors.diag_desc d
                Nothing -> Nothing

        section_lines =
            let main_section' = case m_sp of
                    Just main_sp -> Section.SomeSection $ (main_sp, diag_message_type, Nothing) : main_section
                    Nothing -> Section.SomeSection main_section
            in concatMap Section.render (main_section' : sections)

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
