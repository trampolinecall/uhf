module UHF.Diagnostic.Report (report) where

import UHF.Prelude

import qualified Data.Text as Text

import UHF.Source.Span (Span)
import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report.Line as Line
import qualified UHF.Diagnostic.Report.Messages as Messages
import qualified UHF.Diagnostic.Report.Style as Style
import qualified UHF.Diagnostic.Settings as Settings
import qualified UHF.Source.FormattedString as FormattedString
import qualified UHF.Source.Span as Span

class ToDiagnostic d where
    to_diagnostic :: Style.Style -> d -> (FormattedString.FormattedString, Diagnostic.MessageType, Maybe Span, Text, Diagnostic.MessagesSection, [Diagnostic.OtherSection])

instance ToDiagnostic Diagnostic.Error where
    to_diagnostic style (Diagnostic.Error sp main_message messages sections) = (FormattedString.color_text (Style.error_color style) "error", Diagnostic.MsgError, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.Warning where
    to_diagnostic style (Diagnostic.Warning sp main_message messages sections) = (FormattedString.color_text (Style.warning_color style) "warning", Diagnostic.MsgWarning, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.DebugMessage where
    to_diagnostic style (Diagnostic.DebugMessage sp main_message messages sections) = (FormattedString.color_text (Style.debug_message_color style) "debug message", Diagnostic.MsgNote, sp, main_message, messages, sections)
instance ToDiagnostic Diagnostic.InternalError where
    to_diagnostic style (Diagnostic.InternalError sp main_message messages sections) = (FormattedString.color_text (Style.error_color style) "internal error", Diagnostic.MsgError, sp, main_message, messages, sections)

report :: ToDiagnostic d => Handle -> FormattedString.ColorsNeeded -> Settings.Settings -> d -> IO ()
report handle c_needed (Settings.Settings report_style) d =
    let style = case report_style of
            Settings.ASCII -> Style.default_style
            Settings.Unicode -> Style.unicode_style

        (type_str, main_message_type, m_sp, main_message, main_section, sections) = to_diagnostic style d
        header =
            (case m_sp of
                Just sp -> convert_str (FormattedString.color_text (Style.file_path_color style) (format $ Span.start sp)) <> ": "
                Nothing -> "")
                <> type_str <> ": "
                <> convert_str main_message

        section_lines =
            let main_section' = case m_sp of
                    Just main_sp -> Diagnostic.Section'Messages $ (Just main_sp, main_message_type, Nothing) : main_section
                    Nothing -> Diagnostic.Section'Messages main_section
            in concatMap (render_section style) (main_section' : sections)

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
    mapM_ p_line section_lines

render_section :: Style.Style -> Diagnostic.OtherSection -> [Line.Line]
render_section style (Diagnostic.Section'Messages m) = Messages.render style m
