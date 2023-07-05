module UHF.Phases.Type.Constraint where

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Unknown

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InCasePatterns | InCaseArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField
data Constraint
    = Eq EqInWhat Span (Located TypeWithUnk) (Located TypeWithUnk)
    | Expect ExpectInWhat (Located TypeWithUnk) TypeWithUnk
    | UnkIsApplyResult Span TypeUnknownKey TypeWithUnk TypeWithUnk
