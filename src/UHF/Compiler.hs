module UHF.Compiler
    ( Compiler
    , run_compiler

    , error
    , errors
    , warning
    , warnings
    ) where

import UHF.Util.Prelude hiding (error)

import qualified UHF.Diagnostic as Diagnostic

import qualified System.IO as IO

import qualified UHF.Diagnostic.Settings as DiagnosticSettings

newtype Compiler r = Compiler { uncompiler :: Writer Diagnostics r }

instance Functor Compiler where
    fmap f (Compiler c) = Compiler $ f <$> c

instance Applicative Compiler where
    pure = Compiler . pure
    (Compiler a) <*> (Compiler b) = Compiler $ a <*> b

instance Monad Compiler where
    Compiler a >>= b = Compiler $ a >>= uncompiler . b

data Diagnostics
    = Diagnostics
        { _errors :: [Diagnostic.Error]
        , _warnings :: [Diagnostic.Warning]
        }

instance Monoid Diagnostics where
    mempty = Diagnostics [] []
instance Semigroup Diagnostics where
    Diagnostics e1 w1 <> Diagnostics e2 w2 = Diagnostics (e1 <> e2) (w1 <> w2)

run_compiler :: Compiler r -> DiagnosticSettings.Settings -> IO (Maybe r)
run_compiler (Compiler r) diag_settings =
    let (result, (Diagnostics errors warnings)) = runWriter r
    in mapM_ (Diagnostic.report IO.stdout diag_settings) warnings >>
    mapM_ (Diagnostic.report IO.stderr diag_settings) errors >>
    if null errors
        then pure $ Just result
        else pure Nothing

error :: Diagnostic.IsError e => e -> Compiler ()
error e = Compiler $ tell (Diagnostics [Diagnostic.to_error e] [])
errors :: Diagnostic.IsError e => [e] -> Compiler ()
errors es = Compiler $ tell (Diagnostics (map Diagnostic.to_error es) [])
warning :: Diagnostic.IsWarning w => w -> Compiler ()
warning w = Compiler $ tell (Diagnostics [] [Diagnostic.to_warning w])
warnings :: Diagnostic.IsWarning w => [w] -> Compiler ()
warnings ws = Compiler $ tell (Diagnostics [] (map Diagnostic.to_warning ws))
