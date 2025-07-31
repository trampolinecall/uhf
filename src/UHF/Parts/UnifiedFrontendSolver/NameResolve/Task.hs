module UHF.Parts.UnifiedFrontendSolver.NameResolve.Task
    ( IdenResolveTask (..)
    , TypeExprEvalTask (..)
    , TypeExprEvalAsTypeTask (..)
    , iden_resolve_task_priority
    , type_expr_eval_task_priority
    , type_expr_eval_as_type_priority
    ) where

import UHF.Prelude

import UHF.Data.IR.Type.QuantVar (QuantVarKey)
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps (NameContextKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)

data IdenResolveTask result
    = ResolveRoot NameContextKey (Located Text) (IdenResolvedKey result)
    | ResolveGet TypeExprEvaledKey (Located Text) (IdenResolvedKey result)

data TypeExprEvalTask
    = GetFromDeclIdenResolved (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey
    | MakeTuple (Located TypeExprEvaledKey) (Located TypeExprEvaledKey) TypeExprEvaledKey
    | MakeFunction (Located TypeExprEvaledKey) (Located TypeExprEvaledKey) TypeExprEvaledKey
    | MakeForall (NonEmpty QuantVarKey) (Located TypeExprEvaledKey) TypeExprEvaledKey
    | MakeApply Span (Located TypeExprEvaledKey) (Located TypeExprEvaledKey) TypeExprEvaledKey
    | MakeInferVar Span TypeExprEvaledKey

data TypeExprEvalAsTypeTask
    = EvalAsType (Located TypeExprEvaledKey) TypeExprEvaledAsTypeKey

iden_resolve_task_priority :: IdenResolveTask res -> Int
iden_resolve_task_priority (ResolveRoot _ _ _) = 0
iden_resolve_task_priority (ResolveGet _ _ _) = 1
type_expr_eval_task_priority :: TypeExprEvalTask -> Int
type_expr_eval_task_priority (GetFromDeclIdenResolved _ _) = 2
type_expr_eval_task_priority (MakeTuple _ _ _) = 1
type_expr_eval_task_priority (MakeFunction _ _ _) = 1
type_expr_eval_task_priority (MakeForall _ _ _) = 1
type_expr_eval_task_priority (MakeApply _ _ _ _) = 1
type_expr_eval_task_priority (MakeInferVar _ _) = 0
type_expr_eval_as_type_priority :: TypeExprEvalAsTypeTask -> Int
type_expr_eval_as_type_priority (EvalAsType _ _) = 0
