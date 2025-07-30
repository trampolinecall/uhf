module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Result (InfixGroupResult (..)) where

import UHF.Prelude
import qualified UHF.Data.SIR as SIR

data InfixGroupResult = Call InfixGroupResult InfixGroupResult | Operand Int | Operator SIR.ValueRef
