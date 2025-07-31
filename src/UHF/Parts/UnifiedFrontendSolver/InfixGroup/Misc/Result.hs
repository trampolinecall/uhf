module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey, InfixGroupedArena, InfixGroupResult (..)) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.SIR as SIR

data InfixGroupResult = Call InfixGroupResult InfixGroupResult | Operand Int | Operator SIR.ValueRef

newtype InfixGroupedKey = InfixGroupedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InfixGroupedKey where
    make_key = InfixGroupedKey
    unmake_key (InfixGroupedKey i) = i

type InfixGroupedArena = Arena.Arena (SolveResult () () InfixGroupResult) InfixGroupedKey
