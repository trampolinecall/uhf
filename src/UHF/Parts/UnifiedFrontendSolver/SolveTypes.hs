module UHF.Parts.UnifiedFrontendSolver.SolveTypes (solve) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Aliases
import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Error
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.AddTypes as AddTypes
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.RemoveInferVars as RemoveInferVars
import qualified UHF.Util.Arena as Arena

-- this does both type inference and type checking
solve :: TypeSolver.SolverState -> [TypeSolver.Constraint] -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
solve nr_solver_state backlog (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function)) = -- TODO: do not destructure ir?
    TypeSolver.run_solve_monad_with
        (
            runWriterT (AddTypes.add main_function mods adts type_synonyms quant_vars variables) >>= \ ((mods, adts, type_synonyms, variables), constraints) ->
            let get_type_synonym ts_key = pure $ Arena.get type_synonyms ts_key
            in
            mapM_ (\ constraint -> do
                TypeSolver.solve_constraint adts type_synonyms get_type_synonym quant_vars constraint >>= \case
                    Just (Left e) -> lift (Compiler.tell_error (SolveError e)) >> pure ()
                    Just (Right ()) -> pure ()
                    Nothing -> pure ()
            ) constraints >>
            TypeSolver.solve_constraint_backlog adts type_synonyms get_type_synonym quant_vars backlog >>= \ (backlog_errors, new_backlog) -> -- TODO: not sure what to do with new_backlog
            mapM (lift . Compiler.tell_error . SolveError) backlog_errors >>
            pure (mods, adts, type_synonyms, variables)
        )
        nr_solver_state >>= \ ((mods, adts, type_synonyms, variables), TypeSolver.SolverState infer_vars) ->

    RemoveInferVars.remove infer_vars mods adts type_synonyms variables >>= \ (mods, adts, type_synonyms, variables) ->

    pure (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function))
