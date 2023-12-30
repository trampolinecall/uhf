{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE QualifiedModuleExport #-}

module UHF.Diagnostic
    ( module UHF.Diagnostic.Diagnostic
    , module UHF.Diagnostic.Settings

    , UHF.Diagnostic.report_error
    , UHF.Diagnostic.report_warning
    , UHF.Diagnostic.report_debug_message
    , UHF.Diagnostic.report_internal_error
    ) where

import UHF.Prelude

import qualified System.IO as IO

import UHF.Diagnostic.Diagnostic
import UHF.Diagnostic.Report as Report
import UHF.Diagnostic.Settings
import qualified UHF.Diagnostic.Styles.Default as Styles.Default
import qualified UHF.Source.FormattedString as FormattedString

data SomeStyle = forall s. DiagnosticStyle s => SomeStyle s
instance DiagnosticStyle SomeStyle where
    report_error          (SomeStyle s) = Report.report_error          s
    report_warning        (SomeStyle s) = Report.report_warning        s
    report_debug_message  (SomeStyle s) = Report.report_debug_message  s
    report_internal_error (SomeStyle s) = Report.report_internal_error s

report_error :: ToError e => Settings -> FormattedString.ColorsNeeded -> IO.Handle -> e -> IO ()
report_error settings = Report.report_error (choose_style settings)
report_warning :: ToWarning w => Settings -> FormattedString.ColorsNeeded -> IO.Handle -> w -> IO ()
report_warning settings = Report.report_warning (choose_style settings)
report_debug_message :: Settings -> FormattedString.ColorsNeeded -> Handle -> DebugMessage -> IO ()
report_debug_message settings = Report.report_debug_message (choose_style settings)
report_internal_error :: Settings -> FormattedString.ColorsNeeded -> Handle -> InternalError -> IO ()
report_internal_error settings = Report.report_internal_error (choose_style settings)

choose_style :: Settings -> SomeStyle
choose_style (Settings Unicode) = SomeStyle $ Styles.Default.DefaultStyle Styles.Default.Unicode
choose_style (Settings ASCII) = SomeStyle $ Styles.Default.DefaultStyle Styles.Default.ASCII
