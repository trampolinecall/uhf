module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Types (InfixGroupResultKey) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena

newtype InfixGroupResultKey = InfixGroupResultKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InfixGroupResultKey where
    make_key = InfixGroupResultKey
    unmake_key (InfixGroupResultKey i) = i
