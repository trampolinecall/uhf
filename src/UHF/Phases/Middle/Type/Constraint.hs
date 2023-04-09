module UHF.Phases.Middle.Type.Constraint where

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Unknown

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InCasePatterns | InCaseArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition | InTypeApplication
data Constraint
    = Eq EqInWhat Span (Located TypeWithUnk) (Located TypeWithUnk)
    | Expect ExpectInWhat (Located TypeWithUnk) TypeWithUnk
    | UnkIsApplyResult Span TypeUnknownKey TypeWithUnk TypeWithUnk

