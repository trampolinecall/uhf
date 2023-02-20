module UHF.Diagnostic.Report (report) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.Report.Colors as Colors
import qualified UHF.Diagnostic.Report.Line as Line
import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report.Messages as Messages
import qualified UHF.Diagnostic.Codes.Code as Code

import qualified UHF.Diagnostic.Settings as Settings

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.IO.Span as Span
import UHF.IO.Span (Span)

import qualified Data.Text as Text
import qualified System.IO as IO

class ToDiagnostic d where
    to_diagnostic :: d -> (FormattedString.FormattedString, Maybe (Text, Text), Diagnostic.MessageType, Maybe Span, Text, Diagnostic.MessagesSection, [Diagnostic.OtherSection])

instance ToDiagnostic Diagnostic.Error where
    to_diagnostic (Diagnostic.Error c sp main_message messages sections) = (FormattedString.color_text Colors.error "error", Code.error_code_desc c, Diagnostic.MsgError, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.Warning where
    to_diagnostic (Diagnostic.Warning c sp main_message messages sections) = (FormattedString.color_text Colors.warning "warning", Code.warning_code_desc c, Diagnostic.MsgWarning, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.DebugMessage where
    to_diagnostic (Diagnostic.DebugMessage sp main_message messages sections) = (FormattedString.color_text Colors.debug_message "debug message", Nothing, Diagnostic.MsgNote, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.InternalError where
    to_diagnostic (Diagnostic.InternalError sp main_message messages sections) = (FormattedString.color_text Colors.error "internal error", Nothing, Diagnostic.MsgError, sp, main_message, messages, sections)

report :: ToDiagnostic d => Handle -> FormattedString.ColorsNeeded -> Settings.Settings -> d -> IO ()
report handle c_needed Settings.Settings d = -- TODO: use diagnostic settings
    let (type_str, code_and_desc, main_message_type, m_sp, main_message, main_section, sections) = to_diagnostic d
        header =
            (case m_sp of
                Just sp -> convert_str (FormattedString.color_text Colors.file_path_color (format $ Span.start sp)) <> ": "
                Nothing -> "")
                <> type_str <> ": "
                <> convert_str main_message

        footer =
            case code_and_desc of
                Just (c, d) -> Just $ "==> [" <> FormattedString.color_text Colors.diag_code c <> "] " <> FormattedString.color_text Colors.diag_desc d
                Nothing -> Nothing

        section_lines =
            let main_section' = case m_sp of
                    Just main_sp -> Diagnostic.Section'Messages $ (Just main_sp, main_message_type, Nothing) : main_section
                    Nothing -> Diagnostic.Section'Messages main_section
            in concatMap render_section (main_section' : sections)

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
            FormattedString.render handle c_needed contents >>
            hPutText handle "\n"
    in FormattedString.render handle c_needed header >> hPutText handle "\n" >>
    mapM_ p_line section_lines >>
    case footer of
        Just footer -> hPutStr handle (replicate indent ' ') >> FormattedString.render handle c_needed footer >> IO.hPutStr handle "\n"
        Nothing -> pure ()

render_section :: Diagnostic.OtherSection -> [Line.Line]
render_section (Diagnostic.Section'Messages m) = Messages.render m
