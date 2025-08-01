{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenResults
    , ValueIdenResults
    , VariantIdenResults
    , DeclIdenAlmostFinalResults
    , DeclIdenFinalResults
    , ValueIdenFinalResults
    , VariantIdenFinalResults
    , TypeExprEvaledKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledAsTypeArena
    ) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR.ID as SIR.ID
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef)
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.Type as Type

type DeclIdenResults = Map (SIR.ID.ID "DeclIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
type ValueIdenResults = Map (SIR.ID.ID "ValueIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise ValueRef)
type VariantIdenResults = Map (SIR.ID.ID "VariantIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)

type DeclIdenAlmostFinalResults = Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef TypeWithInferVar.Type))
type ValueIdenFinalResults = Map (SIR.ID.ID "ValueIden") (Maybe ValueRef)
type VariantIdenFinalResults = Map (SIR.ID.ID "VariantIden") (Maybe Type.ADT.VariantIndex)

type DeclIdenFinalResults = Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef Type.Type))

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
