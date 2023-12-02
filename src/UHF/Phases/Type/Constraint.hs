module UHF.Phases.Type.Constraint where

import UHF.IO.Located (Located)
import UHF.IO.Span (Span)
import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Unknown

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InMatchPatterns | InMatchArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication | InADTVariantPatternField
data Constraint
    = Eq EqInWhat Span (Located TypeWithUnk) (Located TypeWithUnk)
    | Expect ExpectInWhat (Located TypeWithUnk) TypeWithUnk
    | UnkIsApplyResult Span TypeUnknownKey TypeWithUnk TypeWithUnk
