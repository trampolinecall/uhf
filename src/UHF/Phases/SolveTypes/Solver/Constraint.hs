module UHF.Phases.SolveTypes.Solver.Constraint where

import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Solver.Unknown

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField
data Constraint
    = Eq EqInWhat Span (Located TypeWithUnk) (Located TypeWithUnk)
    | Expect ExpectInWhat (Located TypeWithUnk) TypeWithUnk
    | UnkIsApplyResult Span TypeUnknownKey TypeWithUnk TypeWithUnk
