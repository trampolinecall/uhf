module UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalTask (..), TypeExprEvalAsTypeTask (..)) where

import UHF.Prelude

import UHF.Data.IR.Type.QuantVar (QuantVarKey)
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps (NameMapStackKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey, TypeExprEvaledKey)
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar as TypeWithInferVar
import UHF.Source.Located (Located)

-- TODO: rename NameMapStack to NameContext everywhere
--
data IdenResolveTask result
    = ResolveRoot NameMapStackKey (Located Text)
    | ResolveGet TypeExprEvaledKey (Located Text)

data TypeExprEvalTask
    = GetFromDeclIdenResolved (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type))
    | MakeTuple TypeExprEvaledKey TypeExprEvaledKey
    | MakeFunction TypeExprEvaledKey TypeExprEvaledKey
    | MakeForall (NonEmpty QuantVarKey) TypeExprEvaledKey
    | MakeApply TypeExprEvaledKey TypeExprEvaledKey
    | MakeInferVar

newtype TypeExprEvalAsTypeTask
    = EvalAsType TypeExprEvaledKey
