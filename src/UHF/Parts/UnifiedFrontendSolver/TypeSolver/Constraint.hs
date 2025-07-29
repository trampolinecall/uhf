module UHF.Parts.UnifiedFrontendSolver.TypeSolver.Constraint where

import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar

-- TODO: maybe dont have InWhat
data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField | InADTFieldType | InMainFunction

data Constraint
    = Eq EqInWhat Span (Located Type) (Located Type)
    | Expect ExpectInWhat (Located Type) Type
    | InferVarIsApplyResult Span InferVarKey Type Type
