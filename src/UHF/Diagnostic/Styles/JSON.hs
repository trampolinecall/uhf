module UHF.Diagnostic.Styles.JSON (JSON (..)) where

import UHF.Prelude

import UHF.Source.Span (Span)
import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report as Report

data JSON = JSON

instance Report.DiagnosticStyle JSON where
    report_error          JSON _ handle e =
        let (Diagnostic.Error         sp main_message messages sections) = Diagnostic.to_error e
        in report handle sp main_message messages sections
    report_warning        JSON _ handle w =
        let (Diagnostic.Warning       sp main_message messages sections) = Diagnostic.to_warning w
        in report handle sp main_message messages sections
    report_debug_message  JSON _ handle d =
        let (Diagnostic.DebugMessage  sp main_message messages sections) = d
        in report handle sp main_message messages sections
    report_internal_error JSON _ handle i =
        let (Diagnostic.InternalError sp main_message messages sections) = i
        in report handle sp main_message messages sections

report :: Handle -> Maybe Span -> Text -> Diagnostic.MessagesSection -> [Diagnostic.Section] -> IO ()
report handle sp main_message messages sections = todo

render_section :: Diagnostic.Section -> Text
render_section (Diagnostic.Section'Messages m) = todo
