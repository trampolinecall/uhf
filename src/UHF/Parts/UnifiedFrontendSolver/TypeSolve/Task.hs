module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (EqInWhat (..), ExpectInWhat (..), TypeSolveTask (..), Constraint (..), priority) where

import UHF.Prelude

import UHF.Data.IR.TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey)
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField | InADTFieldType | InMainFunction

data TypeSolveTask
    = ConstraintWhenTypeExprEvaledAsType TypeExprEvaledAsTypeKey (Type -> Constraint)
    | Constraint Constraint
    | DefinedToBeTypeOfValueRef InferVarKey (IdenResolvedKey SIR.ValueRef) -- TODO: turn this into a ConstraintWhenTypeOfValueRefResolved
    | DefinedToBeTypeOfTypeExpr InferVarKey TypeExprEvaledAsTypeKey -- TODO: merge this with ConstraintWhenTypeExprEvaledAsType

data Constraint
    = Eq EqInWhat Span (Located Type) (Located Type)
    | Expect ExpectInWhat (Located Type) Type
    | DefinedToBe InferVarKey Type
    | InferVarIsApplyResult Span InferVarKey Type Type

priority :: TypeSolveTask -> Int
priority (ConstraintWhenTypeExprEvaledAsType _ _) = 0
priority (DefinedToBeTypeOfValueRef _ _) = 1
priority (DefinedToBeTypeOfTypeExpr _ _) = 1
priority (Constraint (DefinedToBe _ _)) = 2
priority (Constraint (InferVarIsApplyResult _ _ _ _)) = 2
priority (Constraint (Expect _ _ _)) = 3
priority (Constraint (Eq _ _ _ _)) = 4
