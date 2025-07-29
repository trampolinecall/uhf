module UHF.Parts.UnifiedFrontendSolver (solve) where

import UHF.Prelude

import Data.Functor.Const (Const)
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.Aliases as Type
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps as AssignNameMaps
import qualified UHF.Compiler as Compiler
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare as Prepare

-- import qualified UHF.Compiler as Compiler
-- import qualified UHF.Data.SIR as SIR
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps as AssignNameMaps
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.EvalTypeExprs as EvalTypeExprs
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveReferStarts as ResolveReferStarts
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare as Prepare
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Resolve as Resolve
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize as Finalize
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveVPIdens as ResolveVPIdens
-- import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.AddTypes as AddTypes
-- import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Aliases
-- import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Error
-- import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.RemoveInferVars as RemoveInferVars
-- import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
-- import qualified UHF.Util.Arena as Arena

type PreSolve = ((), Const () (), (), (), (), ())
type PostSolve = (NameMaps.NameMapStackKey, Maybe (), SIR.DeclRef Type.Type, Type.Type, Maybe Type.Type, Void)

solve :: SIR.SIR PreSolve -> Compiler.WithDiagnostics NameResolve.Error.Error Void (SIR.SIR PostSolve)
solve sir = do
    (sir, name_map_stack_arena, sir_child_maps) <- AssignNameMaps.assign sir
    let sir' = Prepare.prepare sir

    todo

-- (sir, constraints) <- runWrtierT (AddTypes.add sir)

-- sir <- Resolve.resolve sir
-- -- TODO: solve constraints and mix these together

-- sir <- Finalize.finalize sir
-- sir <- RemoveInferVars.remove _

-- sir

--
-- -- this does both type inference and type checking
-- solve :: TypeSolver.SolverState -> [TypeSolver.Constraint] -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
-- solve nr_solver_state backlog (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function)) = -- TODO: do not destructure ir?
--     TypeSolver.run_solve_monad_with
--         (
--             runWriterT (AddTypes.add main_function mods adts type_synonyms quant_vars variables) >>= \ ((mods, adts, type_synonyms, variables), constraints) ->
--             let get_type_synonym ts_key = pure $ Arena.get type_synonyms ts_key
--             in
--             mapM_ (\ constraint -> do
--                 TypeSolver.solve_constraint adts type_synonyms get_type_synonym quant_vars constraint >>= \case
--                     Just (Left e) -> lift (Compiler.tell_error (SolveError e)) >> pure ()
--                     Just (Right ()) -> pure ()
--                     Nothing -> pure ()
--             ) constraints >>
--             TypeSolver.solve_constraint_backlog adts type_synonyms get_type_synonym quant_vars backlog >>= \ (backlog_errors, new_backlog) -> -- TODO: not sure what to do with new_backlog
--             mapM (lift . Compiler.tell_error . SolveError) backlog_errors >>
--             pure (mods, adts, type_synonyms, variables)
--         )
--         nr_solver_state >>= \ ((mods, adts, type_synonyms, variables), TypeSolver.SolverState infer_vars) ->
--
--     RemoveInferVars.remove infer_vars mods adts type_synonyms variables >>= \ (mods, adts, type_synonyms, variables) ->
--
--     pure (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function))
