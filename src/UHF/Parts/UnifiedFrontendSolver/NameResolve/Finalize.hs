module UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize (finalize) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import UHF.Parts.UnifiedFrontendSolver.Error (Error (NRError))
import qualified UHF.Parts.UnifiedFrontendSolver.Error as SolveError
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( IdenResolvedArena
    , IdenResolvedKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledKey
    )
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef)

finalize ::
    ( IdenResolvedArena (DeclRef TypeWithInferVar.Type)
    , IdenResolvedArena ValueRef
    , IdenResolvedArena Type.ADT.VariantIndex
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    ) ->
    Compiler.WithDiagnostics
        SolveError.Error
        Void
        ( Arena.Arena (Maybe (DeclRef TypeWithInferVar.Type)) (IdenResolvedKey (DeclRef TypeWithInferVar.Type))
        , Arena.Arena (Maybe ValueRef) (IdenResolvedKey ValueRef)
        , Arena.Arena (Maybe Type.ADT.VariantIndex) (IdenResolvedKey Type.ADT.VariantIndex)
        , Arena.Arena (Maybe (DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey
        , Arena.Arena (Maybe TypeWithInferVar.Type) TypeExprEvaledAsTypeKey
        )
finalize (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena) =
    (,,,,)
        <$> Arena.transformM finalize_result decl_iden_resolved_arena
        <*> Arena.transformM finalize_result value_iden_resolved_arena
        <*> Arena.transformM finalize_result variant_iden_resolved_arena
        <*> Arena.transformM finalize_result type_expr_evaled_arena
        <*> Arena.transformM finalize_result type_expr_evaled_as_type_arena

finalize_result ::
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result -> Compiler.WithDiagnostics SolveError.Error Void (Maybe result)
finalize_result (Inconclusive (Just err)) = Compiler.tell_error (NRError err) >> pure Nothing
finalize_result (Inconclusive Nothing) = pure Nothing
finalize_result (Errored _) = pure Nothing
finalize_result (Solved r) = pure $ Just r
