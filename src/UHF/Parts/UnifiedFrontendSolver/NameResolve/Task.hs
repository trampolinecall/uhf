{-# LANGUAGE DataKinds #-}
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
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps (NameContextKey)
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Data.SIR.ID as SIR.ID

data IdenResolveTask result_id
    = ResolveRoot NameContextKey (Located Text) result_id
    | ResolveGet (SIR.ID.ID "TypeExpr") (Located Text) result_id

data TypeExprEvalTask
    = GetFromDeclIdenResolved (SIR.ID.ID "DeclIden") (SIR.ID.ID "TypeExpr")
    | MakeTuple (Located (SIR.ID.ID "TypeExpr")) (Located (SIR.ID.ID "TypeExpr")) (SIR.ID.ID "TypeExpr")
    | MakeFunction (Located (SIR.ID.ID "TypeExpr")) (Located (SIR.ID.ID "TypeExpr")) (SIR.ID.ID "TypeExpr")
    | MakeForall (NonEmpty QuantVarKey) (Located (SIR.ID.ID "TypeExpr")) (SIR.ID.ID "TypeExpr")
    | MakeApply Span (Located (SIR.ID.ID "TypeExpr")) (Located (SIR.ID.ID "TypeExpr")) (SIR.ID.ID "TypeExpr")
    | MakeInferVar Span (SIR.ID.ID "TypeExpr")

data TypeExprEvalAsTypeTask
    = EvalAsType (Located (SIR.ID.ID "TypeExpr")) (SIR.ID.ID "TypeExprEvaledAsType")

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
