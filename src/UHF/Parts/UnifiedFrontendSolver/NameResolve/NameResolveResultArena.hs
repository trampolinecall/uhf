module UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey, IdenResolvedArena, TypeExprEvaledKey, TypeExprEvaledArena, TypeExprEvaledAsTypeKey, TypeExprEvaledAsTypeArena) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeWithInferVar -- TODO: fix this import
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Compiler as Compiler

newtype IdenResolvedKey res = IdenResolvedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key (IdenResolvedKey res) where
    make_key = IdenResolvedKey
    unmake_key (IdenResolvedKey i) = i
type IdenResolvedArena res = Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise res) (IdenResolvedKey res)

-- TODO: split these into separate phases
newtype TypeExprEvaledKey = TypeExprEvaledKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledKey where
    make_key = TypeExprEvaledKey
    unmake_key (TypeExprEvaledKey i) = i
type TypeExprEvaledArena = Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey
-- TODO: remove res type parameter?

newtype TypeExprEvaledAsTypeKey = TypeExprEvaledAsTypeKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledAsTypeKey where
    make_key = TypeExprEvaledAsTypeKey
    unmake_key (TypeExprEvaledAsTypeKey i) = i
type TypeExprEvaledAsTypeArena = Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type) TypeExprEvaledAsTypeKey
-- TODO: remove res type parameter?
