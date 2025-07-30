module UHF.Parts.UnifiedFrontendSolver.SolveTypes.Task (TypeSolveTask (..)) where

import UHF.Prelude

import UHF.Data.IR.Type.QuantVar (QuantVarKey)
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps (NameMapStackKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar as TypeWithInferVar
import UHF.Source.Located (Located)

data TypeSolveTask
    = ConstraintWhenTypeExprEvaledAsType TypeExprEvaledAsTypeKey (TypeWithInferVar.Type -> TypeSolver.Constraint)
    | Constraint TypeSolver.Constraint
    | DefinedToBeTypeOfValueRef TypeSolver.InferVarKey (IdenResolvedKey SIR.ValueRef)
    | DefinedToBeTypeOfTypeExpr TypeWithInferVar.InferVarKey TypeExprEvaledAsTypeKey
