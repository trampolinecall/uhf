module UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalTask (..), TypeExprEvalAsTypeTask (..)) where

import UHF.Prelude

import UHF.Data.IR.Type.QuantVar (QuantVarKey)
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps (NameMapStackKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledKey, TypeExprEvaledAsTypeKey)
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar

-- TODO: rename NameMapStack to NameContext everywhere
--
data IdenResolveTask result
    = ResolveRoot NameMapStackKey (Located Text) (IdenResolvedKey result)
    | ResolveGet TypeExprEvaledKey (Located Text) (IdenResolvedKey result)

data TypeExprEvalTask
    = GetFromDeclIdenResolved (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey
    | MakeTuple TypeExprEvaledKey TypeExprEvaledKey TypeExprEvaledKey
    | MakeFunction TypeExprEvaledKey TypeExprEvaledKey TypeExprEvaledKey
    | MakeForall (NonEmpty QuantVarKey) TypeExprEvaledKey TypeExprEvaledKey
    | MakeApply TypeExprEvaledKey TypeExprEvaledKey TypeExprEvaledKey
    | MakeInferVar TypeExprEvaledKey

data TypeExprEvalAsTypeTask
    = EvalAsType TypeExprEvaledKey TypeExprEvaledAsTypeKey
