module UHF.Diagnostic.Report (DiagnosticStyle (..)) where

import UHF.Prelude

import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Source.FormattedString as FormattedString

class DiagnosticStyle s where
    report_error          :: Diagnostic.ToError   e => s -> FormattedString.ColorsNeeded -> Handle -> e                        -> IO ()
    report_warning        :: Diagnostic.ToWarning w => s -> FormattedString.ColorsNeeded -> Handle -> w                        -> IO ()
    report_debug_message  ::                           s -> FormattedString.ColorsNeeded -> Handle -> Diagnostic.DebugMessage  -> IO ()
    report_internal_error ::                           s -> FormattedString.ColorsNeeded -> Handle -> Diagnostic.InternalError -> IO ()

