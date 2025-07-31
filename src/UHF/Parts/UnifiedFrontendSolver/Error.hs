module UHF.Parts.UnifiedFrontendSolver.Error (Error (..)) where

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Error as TypeSolve.Error

data Error
    = NRError NameResolve.Error.Error
    | TSError TypeSolve.Error.Error

instance Diagnostic.ToError Error where
    to_error (NRError e) = Diagnostic.to_error e
    to_error (TSError e) = Diagnostic.to_error e
