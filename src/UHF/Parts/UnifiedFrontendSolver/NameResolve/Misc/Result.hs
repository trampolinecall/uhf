{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenResults
    , ValueIdenResults
    , VariantIdenResults
    , DeclIdenAlmostFinalResults
    , DeclIdenFinalResults
    , ValueIdenFinalResults
    , VariantIdenFinalResults
    , TypeExprsEvaled
    , TypeExprsEvaledAsTypes
    , TypeExprsAlmostFinalEvaled
    , TypeExprsAlmostFinalEvaledAsTypes
    , TypeExprsFinalEvaled
    , TypeExprsFinalEvaledAsTypes
    ) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR.ID as SIR.ID
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef)
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Data.IR.Type as Type

type DeclIdenResults = Map (SIR.ID.ID "DeclIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
type ValueIdenResults = Map (SIR.ID.ID "ValueIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise ValueRef)
type VariantIdenResults = Map (SIR.ID.ID "VariantIden") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)

type DeclIdenAlmostFinalResults = Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef TypeWithInferVar.Type))
type ValueIdenFinalResults = Map (SIR.ID.ID "ValueIden") (Maybe ValueRef)
type VariantIdenFinalResults = Map (SIR.ID.ID "VariantIden") (Maybe Type.ADT.VariantIndex)

type DeclIdenFinalResults = Map (SIR.ID.ID "DeclIden") (Maybe (DeclRef Type.Type))

type TypeExprsEvaled = Map (SIR.ID.ID "TypeExpr") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
type TypeExprsEvaledAsTypes = Map (SIR.ID.ID "TypeExprEvaledAsType") (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type)

type TypeExprsAlmostFinalEvaled = Map (SIR.ID.ID "TypeExpr") (Maybe (DeclRef TypeWithInferVar.Type))
type TypeExprsAlmostFinalEvaledAsTypes = Map (SIR.ID.ID "TypeExprEvaledAsType") (Maybe TypeWithInferVar.Type)

type TypeExprsFinalEvaled = Map (SIR.ID.ID "TypeExpr") (Maybe (DeclRef Type.Type))
type TypeExprsFinalEvaledAsTypes = Map (SIR.ID.ID "TypeExprEvaledAsType") (Maybe Type.Type)
