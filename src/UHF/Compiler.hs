module UHF.Compiler
    ( WithDiagnostics
    , WithDiagnosticsT
    , Diagnostics (..)
    , ErrorReportedPromise

    , report_diagnostics
    , convert_diagnostics

    , had_errors

    , tell_error
    , tell_errors
    , tell_warning
    , tell_warnings
    ) where

import UHF.Prelude

import Data.Functor.Identity
import qualified Data.Sequence as Sequence
import qualified System.IO as IO

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Settings as Diagnostic.Settings
import qualified UHF.Source.FormattedString as FormattedString

type WithDiagnosticsT e w = WriterT (Diagnostics e w)
type WithDiagnostics e w = WithDiagnosticsT e w Identity

data Diagnostics e w = Diagnostics (Seq e) (Seq w) deriving Show
data ErrorReportedPromise = ErrorReportedPromise deriving Show

instance Monoid (Diagnostics e w) where
    mempty = Diagnostics Sequence.empty Sequence.empty
instance Semigroup (Diagnostics e w) where
    Diagnostics e1 w1 <> Diagnostics e2 w2 = Diagnostics (e1 <> e2) (w1 <> w2)

report_diagnostics :: (Diagnostic.ToError e, Diagnostic.ToWarning w) => FormattedString.ColorsNeeded -> Diagnostic.Settings.Settings -> Diagnostics e w -> IO ()
report_diagnostics c_needed diag_settings (Diagnostics errors warnings) = do
    mapM_ (Diagnostic.report_error diag_settings c_needed IO.stderr . Diagnostic.to_error) errors
    mapM_ (Diagnostic.report_warning diag_settings c_needed IO.stderr . Diagnostic.to_warning) warnings

had_errors :: Diagnostics e w -> Bool
had_errors (Diagnostics errs _) = not $ Sequence.null errs

convert_diagnostics :: (Functor m, Diagnostic.ToError e, Diagnostic.ToWarning w) => WithDiagnosticsT e w m r -> WithDiagnosticsT Diagnostic.Error Diagnostic.Warning m r
convert_diagnostics = mapWriterT (fmap (\ (r, Diagnostics e w) -> (r, Diagnostics (fmap Diagnostic.to_error e) (fmap Diagnostic.to_warning w))))

tell_error :: (Monad m, Diagnostic.ToError e) => e -> WithDiagnosticsT e w m ErrorReportedPromise
tell_error e = tell (Diagnostics (Sequence.singleton e) Sequence.empty) >> pure ErrorReportedPromise
tell_errors :: (Monad m, Diagnostic.ToError e) => [e] -> WithDiagnosticsT e w m ErrorReportedPromise
tell_errors es = tell (Diagnostics (Sequence.fromList es) Sequence.empty) >> pure ErrorReportedPromise
tell_warning :: (Monad m, Diagnostic.ToWarning w) => w -> WithDiagnosticsT e w m ()
tell_warning w = tell (Diagnostics Sequence.empty (Sequence.singleton w))
tell_warnings :: (Monad m, Diagnostic.ToWarning w) => [w] -> WithDiagnosticsT e w m ()
tell_warnings ws = tell (Diagnostics Sequence.empty (Sequence.fromList ws))
