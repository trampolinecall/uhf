module UHF.Diagnostic.Styles.Default (DefaultStyle (..), CharacterSet (..)) where

import UHF.Prelude

import qualified Data.Text as Text

import UHF.Source.Span (Span)
import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report as Report
import qualified UHF.Diagnostic.Styles.Default.Line as Line
import qualified UHF.Diagnostic.Styles.Default.RenderMessagesSection as RenderMessagesSection
import qualified UHF.Diagnostic.Styles.Default.Options as Options
import qualified UHF.Source.FormattedString as FormattedString
import qualified UHF.Source.Span as Span

data CharacterSet = ASCII | Unicode
data DefaultStyle = DefaultStyle CharacterSet

instance Report.DiagnosticStyle DefaultStyle where
    report_error          (DefaultStyle character_set) colors_needed handle e = report handle colors_needed character_set (Diagnostic.to_error e)
    report_warning        (DefaultStyle character_set) colors_needed handle w = report handle colors_needed character_set (Diagnostic.to_warning w)
    report_debug_message  (DefaultStyle character_set) colors_needed handle d = report handle colors_needed character_set d
    report_internal_error (DefaultStyle character_set) colors_needed handle i = report handle colors_needed character_set i

class ToDiagnostic d where
    to_diagnostic :: Options.Options -> d -> (FormattedString.FormattedString, Maybe Span, Text, Diagnostic.MessageType, Diagnostic.MessagesSection, [Diagnostic.Section])

instance ToDiagnostic Diagnostic.Error where
    to_diagnostic options (Diagnostic.Error sp main_message messages sections) = (FormattedString.color_text (Options.error_color options) "error", sp, main_message, Diagnostic.MsgError, messages, sections)
instance ToDiagnostic Diagnostic.Warning where
    to_diagnostic options (Diagnostic.Warning sp main_message messages sections) = (FormattedString.color_text (Options.warning_color options) "warning", sp, main_message, Diagnostic.MsgWarning, messages, sections)
instance ToDiagnostic Diagnostic.DebugMessage where
    to_diagnostic options (Diagnostic.DebugMessage sp main_message messages sections) = (FormattedString.color_text (Options.debug_message_color options) "debug message", sp, main_message, Diagnostic.MsgNote, messages, sections)
instance ToDiagnostic Diagnostic.InternalError where
    to_diagnostic options (Diagnostic.InternalError sp main_message messages sections) = (FormattedString.color_text (Options.error_color options) "internal error", sp, main_message, Diagnostic.MsgError, messages, sections)

report :: ToDiagnostic d => Handle -> FormattedString.ColorsNeeded -> CharacterSet -> d -> IO ()
report handle c_needed character_set d =
    let options = case character_set of
            ASCII -> Options.ascii_options
            Unicode -> Options.unicode_options

        (type_str, main_message_sp, main_message, main_message_type, main_section, other_sections) = to_diagnostic options d
        header =
            (case main_message_sp of
                Just sp -> convert_str (FormattedString.color_text (Options.file_path_color options) (format $ Span.start sp)) <> ": "
                Nothing -> "")
                <> type_str <> ": "
                <> convert_str main_message

        main_section' = case main_message_sp of
                Just main_sp -> Diagnostic.Section'Messages $ (Just main_sp, main_message_type, Nothing) : main_section
                Nothing -> Diagnostic.Section'Messages main_section
        section_lines = concatMap (render_section options) (main_section' : other_sections)

        indent =
            if null section_lines
                then 4
                else maximum $ map (Text.length . Line.prefix) section_lines

    in FormattedString.render handle c_needed header >> hPutText handle "\n" >> mapM_ (Line.print c_needed indent handle) section_lines

render_section :: Options.Options -> Diagnostic.Section -> [Line.Line]
render_section options (Diagnostic.Section'Messages m) = RenderMessagesSection.render options m
