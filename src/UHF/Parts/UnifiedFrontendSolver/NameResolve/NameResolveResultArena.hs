module UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey, IdenResolvedArena, TypeExprEvaledKey, TypeExprEvaledArena, TypeExprEvaledAsTypeKey, TypeExprEvaledAsTypeArena) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult (ResolveResult)
import qualified UHF.Util.Arena as Arena

newtype IdenResolvedKey res = IdenResolvedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key (IdenResolvedKey res) where
    make_key = IdenResolvedKey
    unmake_key (IdenResolvedKey i) = i
type IdenResolvedArena res = Arena.Arena (ResolveResult () () res) (IdenResolvedKey res)

-- TODO: split these into separate phases
newtype TypeExprEvaledKey = TypeExprEvaledKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledKey where
    make_key = TypeExprEvaledKey
    unmake_key (TypeExprEvaledKey i) = i
type TypeExprEvaledArena res = Arena.Arena (ResolveResult () () res) TypeExprEvaledKey

newtype TypeExprEvaledAsTypeKey = TypeExprEvaledAsTypeKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledAsTypeKey where
    make_key = TypeExprEvaledAsTypeKey
    unmake_key (TypeExprEvaledAsTypeKey i) = i
type TypeExprEvaledAsTypeArena res = Arena.Arena (ResolveResult () () res) TypeExprEvaledAsTypeKey
