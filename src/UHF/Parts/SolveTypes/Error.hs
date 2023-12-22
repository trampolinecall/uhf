module UHF.Parts.SolveTypes.Error (Error(..)) where

import UHF.Prelude

import UHF.Parts.SolveTypes.Aliases
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Parts.TypeSolver as TypeSolver

data Error
    = AmbiguousType TypeSolver.InferVarForWhat
    | SolveError (TypeSolver.SolveError (TypedWithInferVarsTypeExpr, TypeSolver.Type))

instance Diagnostic.ToError Error where
    to_error (AmbiguousType for_what) =
        let sp = TypeSolver.infer_var_for_what_sp for_what
            name = TypeSolver.infer_var_for_what_name for_what
        in Diagnostic.Error
                (Just sp)
                ("ambiguous type: could not infer the type of this " <> name)
                []
                []

    to_error (SolveError se) = Diagnostic.to_error se
