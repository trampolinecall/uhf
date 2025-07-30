module UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedKey, InfixGroupedArena) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Result (InfixGroupResult)

newtype InfixGroupedKey = InfixGroupedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InfixGroupedKey where
    make_key = InfixGroupedKey
    unmake_key (InfixGroupedKey i) = i

type InfixGroupedArena = Arena.Arena (SolveResult () () InfixGroupResult) InfixGroupedKey
