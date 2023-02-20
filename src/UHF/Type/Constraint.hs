module UHF.Type.Constraint where

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

import UHF.Type.Aliases

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InCasePatterns | InCaseArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition
data Constraint
    = Eq EqInWhat Span (Located TypeWithVars) (Located TypeWithVars)
    | Expect ExpectInWhat (Located TypeWithVars) TypeWithVars

