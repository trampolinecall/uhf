{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (EqInWhat (..), ExpectInWhat (..), TypeSolveTask (..), Constraint (..), priority) where

import UHF.Prelude

import UHF.Data.IR.TypeWithInferVar
import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef)
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms deriving Show
data ExpectInWhat
    = InTypeAnnotation
    | InCallExpr
    | InIfCondition
    | InTypeApplication
    | InADTVariantPatternField
    | InADTFieldType
    | InMainFunction
    | InVariable
    deriving Show

data TypeSolveTask
    = WhenTypeExprEvaledAsType (SIR.ID.ID "TypeExprEvaledAsType") (Type -> TypeSolveTask)
    | WhenTypeExprEvaled (SIR.ID.ID "TypeExpr") (DeclRef Type -> TypeSolveTask)
    | WhenValueRefResolved (SIR.ID.ID "ValueIden") (ValueRef -> TypeSolveTask)
    | EvalAsType Span (DeclRef Type) (Type -> TypeSolveTask)
    | GetValueRefType ValueRef (Type -> TypeSolveTask)
    | Constraint Constraint

data Constraint
    = Eq EqInWhat Span (Located Type) (Located Type)
    | Expect ExpectInWhat (Located Type) Type
    | DefinedToBe ExpectInWhat Span InferVarKey Type -- TODO: make this should have its own InWhat?
    | InferVarIsApplyResult Span InferVarKey Type Type
    deriving Show

priority :: TypeSolveTask -> Int
priority (WhenTypeExprEvaledAsType _ _) = 0
priority (WhenTypeExprEvaled _ _) = 0
priority (WhenValueRefResolved _ _) = 0
priority (EvalAsType _ _ _) = 0
priority (GetValueRefType _ _) = 0
priority (Constraint (DefinedToBe _ _ _ _)) = 1
priority (Constraint (InferVarIsApplyResult _ _ _ _)) = 1
priority (Constraint (Expect _ _ _)) = 2
priority (Constraint (Eq _ _ _ _)) = 3
