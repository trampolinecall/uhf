module UHF.Compiler
    ( Compiler
    , run_compiler

    , error
    , errors
    , warning
    , warnings
    ) where

import UHF.Util.Prelude hiding (error)

import Control.Monad.Fix (MonadFix (mfix))

import qualified System.IO as IO

import qualified Data.Sequence as Sequence

import qualified UHF.IO.FormattedString as FormattedString

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

newtype Compiler r = Compiler { uncompiler :: WriterT Diagnostics IO r }

instance Functor Compiler where
    fmap f (Compiler c) = Compiler $ f <$> c

instance Applicative Compiler where
    pure = Compiler . pure
    (Compiler a) <*> (Compiler b) = Compiler $ a <*> b

instance Monad Compiler where
    Compiler a >>= b = Compiler $ a >>= uncompiler . b

instance MonadFix Compiler where
    mfix f = Compiler $ mfix (uncompiler . f)

data Diagnostics
    = Diagnostics
        { _errors :: Seq Diagnostic.Error
        , _warnings :: Seq Diagnostic.Warning
        }

instance Monoid Diagnostics where
    mempty = Diagnostics Sequence.empty Sequence.empty
instance Semigroup Diagnostics where
    Diagnostics e1 w1 <> Diagnostics e2 w2 = Diagnostics (e1 <> e2) (w1 <> w2)

run_compiler :: Compiler r -> FormattedString.ColorsNeeded -> DiagnosticSettings.Settings -> IO (Maybe r)
run_compiler (Compiler r) c_needed diag_settings =
    runWriterT r >>= \ (result, Diagnostics errors warnings) ->
    mapM_ (Diagnostic.report IO.stdout c_needed diag_settings) warnings >>
    mapM_ (Diagnostic.report IO.stderr c_needed diag_settings) errors >>
    if null errors
        then pure $ Just result
        else pure Nothing

error :: Diagnostic.ToError e => e -> Compiler ()
error e = Compiler $ tell (Diagnostics (Sequence.singleton $ Diagnostic.to_error e) Sequence.empty)
errors :: Diagnostic.ToError e => [e] -> Compiler ()
errors es = Compiler $ tell (Diagnostics (Sequence.fromList $ map Diagnostic.to_error es) Sequence.empty)
warning :: Diagnostic.IsWarning w => w -> Compiler ()
warning w = Compiler $ tell (Diagnostics Sequence.empty (Sequence.singleton $ Diagnostic.to_warning w))
warnings :: Diagnostic.IsWarning w => [w] -> Compiler ()
warnings ws = Compiler $ tell (Diagnostics Sequence.empty (Sequence.fromList $ map Diagnostic.to_warning ws))
