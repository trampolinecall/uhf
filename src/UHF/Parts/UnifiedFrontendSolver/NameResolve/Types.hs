module UHF.Parts.UnifiedFrontendSolver.NameResolve.Types (NameContextKey) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena

newtype NameContextKey = NameContextKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key NameContextKey where
    make_key = NameContextKey
    unmake_key (NameContextKey i) = i
