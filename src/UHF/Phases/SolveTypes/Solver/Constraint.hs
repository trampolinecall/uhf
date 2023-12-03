module UHF.Phases.SolveTypes.Solver.Constraint where

import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Solver.InferVar

-- TODO: maybe dont have InWhat
data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField

data Constraint
    = Eq EqInWhat Span (Located TypeWithInferVars) (Located TypeWithInferVars)
    | Expect ExpectInWhat (Located TypeWithInferVars) TypeWithInferVars
    | UnkIsApplyResult Span TypeInferVarKey TypeWithInferVars TypeWithInferVars
