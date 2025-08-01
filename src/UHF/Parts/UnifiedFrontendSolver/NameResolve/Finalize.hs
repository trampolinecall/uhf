module UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize (finalize) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import UHF.Parts.UnifiedFrontendSolver.Error (Error (NRError))
import qualified UHF.Parts.UnifiedFrontendSolver.Error as SolveError
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenAlmostFinalResults
    , DeclIdenResults
    , TypeExprsAlmostFinalEvaled
    , TypeExprsAlmostFinalEvaledAsTypes
    , TypeExprsEvaled
    , TypeExprsEvaledAsTypes
    , ValueIdenFinalResults
    , ValueIdenResults
    , VariantIdenFinalResults
    , VariantIdenResults
    )
import UHF.Parts.UnifiedFrontendSolver.SolveResult

finalize ::
    ( DeclIdenResults
    , ValueIdenResults
    , VariantIdenResults
    , TypeExprsEvaled
    , TypeExprsEvaledAsTypes
    ) ->
    Compiler.WithDiagnostics
        SolveError.Error
        Void
        ( DeclIdenAlmostFinalResults
        , ValueIdenFinalResults
        , VariantIdenFinalResults
        , TypeExprsAlmostFinalEvaled
        , TypeExprsAlmostFinalEvaledAsTypes
        )
finalize (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena) =
    (,,,,)
        <$> mapM finalize_result decl_iden_results
        <*> mapM finalize_result value_iden_results
        <*> mapM finalize_result variant_iden_results
        <*> mapM finalize_result type_expr_evaled_arena
        <*> mapM finalize_result type_expr_evaled_as_type_arena

finalize_result ::
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result -> Compiler.WithDiagnostics SolveError.Error Void (Maybe result)
finalize_result (Inconclusive (Just err)) = Compiler.tell_error (NRError err) >> pure Nothing
finalize_result (Inconclusive Nothing) = pure Nothing
finalize_result (Errored _) = pure Nothing
finalize_result (Solved r) = pure $ Just r
