module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( IdenResolvedKey
    , IdenResolvedArena
    , TypeExprEvaledKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledAsTypeArena
    , convert_decl_iden_resolved_key
    ) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef)

newtype IdenResolvedKey res = IdenResolvedKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key (IdenResolvedKey res) where
    make_key = IdenResolvedKey
    unmake_key (IdenResolvedKey i) = i
type IdenResolvedArena res = Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise res) (IdenResolvedKey res)

convert_decl_iden_resolved_key :: IdenResolvedKey (DeclRef TypeWithInferVar.Type) -> IdenResolvedKey (DeclRef Type.Type)
convert_decl_iden_resolved_key (IdenResolvedKey k) = IdenResolvedKey k

newtype TypeExprEvaledKey = TypeExprEvaledKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledKey where
    make_key = TypeExprEvaledKey
    unmake_key (TypeExprEvaledKey i) = i
type TypeExprEvaledArena =
    Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey

newtype TypeExprEvaledAsTypeKey = TypeExprEvaledAsTypeKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeExprEvaledAsTypeKey where
    make_key = TypeExprEvaledAsTypeKey
    unmake_key (TypeExprEvaledAsTypeKey i) = i
type TypeExprEvaledAsTypeArena =
    Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type) TypeExprEvaledAsTypeKey
