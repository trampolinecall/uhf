module UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedKey, InfixGroupedArena) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena

newtype InfixGroupedKey = InfixGroupedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InfixGroupedKey where
    make_key = InfixGroupedKey
    unmake_key (InfixGroupedKey i) = i

-- TODO: remove res type parameter
type InfixGroupedArena res = Arena.Arena (SolveResult () () res) InfixGroupedKey
