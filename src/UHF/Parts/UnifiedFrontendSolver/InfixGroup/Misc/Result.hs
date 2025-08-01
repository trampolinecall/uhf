module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey, InfixGroupedArena, InfixGroupResult (..)) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (ValueRef)

data InfixGroupResult = Call InfixGroupResult InfixGroupResult | Operand Int | Operator ValueRef

newtype InfixGroupedKey = InfixGroupedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InfixGroupedKey where
    make_key = InfixGroupedKey
    unmake_key (InfixGroupedKey i) = i

type InfixGroupedArena = Arena.Arena (SolveResult () () InfixGroupResult) InfixGroupedKey
