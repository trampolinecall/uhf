{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UHF.Parts.UnifiedFrontendSolver (solve) where

import UHF.Prelude

import Data.Functor.Const (Const)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Group as InfixGroup.Group
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedKey)
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena as InfixGroupResultArena
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Prepare as InfixGroup.Prepare
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task as InfixGroup.Task
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps as NameResolve.AssignNameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize as NameResolve.Finalize
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameResolve.NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena as NameResolveResultArena
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare as NameResolve.Prepare
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Resolve as NameResolve.Resolve
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Task as NameResolve.Task
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.AddTypes as AddTypes
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.Solve as SolveTypes.Solve
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.Task as SolveTypes.Task
import qualified UHF.Parts.UnifiedFrontendSolver.Solving as Solving
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as SolveMonad
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena
import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.RemoveInferVars as RemoveInferVars
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver

-- import qualified UHF.Compiler as Compiler
-- import qualified UHF.Data.SIR as SIR
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps as AssignNameMaps
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.EvalTypeExprs as EvalTypeExprs
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveReferStarts as ResolveReferStarts
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare as Prepare
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Resolve as Resolve
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize as Finalize
-- import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveVPIdens as ResolveVPIdens
-- import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Aliases
-- import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Error
-- import qualified UHF.Parts.UnifiedFrontendSolver.SolveTypes.RemoveInferVars as RemoveInferVars
-- import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
-- import qualified UHF.Util.Arena as Arena

type PreSolve = ((), Const () (), (), (), (), (), ())
type PostSolve = (NameResolve.NameMaps.NameMapStackKey, IdenResolvedKey (), Type.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, Maybe Type.Type, InfixGroupedKey)

solve ::
    SIR.SIR PreSolve ->
    Compiler.WithDiagnostics
        NameResolve.Error.Error
        Void
        ( SIR.SIR PostSolve
        , ( Arena.Arena (Maybe (SIR.DeclRef TypeWithInferVar.Type)) (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type))
          , Arena.Arena (Maybe SIR.ValueRef) (IdenResolvedKey SIR.ValueRef)
          , Arena.Arena (Maybe Type.ADT.VariantIndex) (IdenResolvedKey Type.ADT.VariantIndex)
          , Arena.Arena (Maybe (SIR.DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey
          , Arena.Arena (Maybe TypeWithInferVar.Type) TypeExprEvaledAsTypeKey
          )
        , InfixGroupResultArena.InfixGroupedArena
        )
solve sir = do
    -- TODO: clean this up
    (sir, name_map_stack_arena, sir_child_maps) <- NameResolve.AssignNameMaps.assign sir
    let (sir', name_resolution_results, name_resolution_tasks) = NameResolve.Prepare.prepare sir
    let (sir'', infix_group_results, infix_group_tasks) = InfixGroup.Prepare.prepare sir'
    let (sir''', type_solver_state, type_solving_tasks) = AddTypes.add sir''

    (((), (name_resolution_results, infix_group_results)), TypeSolver.SolverState type_solver_state) <-
        SolveMonad.run_solve_monad_with
            ( runReaderT
                (runStateT (solve' (name_resolution_tasks, infix_group_tasks, type_solving_tasks)) (name_resolution_results, infix_group_results))
                (name_map_stack_arena, sir_child_maps, sir''')
            )
            type_solver_state

    name_resolution_results <- NameResolve.Finalize.finalize name_resolution_results
    sir'''' <- RemoveInferVars.remove infer_vars sir'''

    pure (sir'''', name_resolution_results, infix_group_results)

solve' ::
    ( ( [NameResolve.Task.IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)]
      , [NameResolve.Task.IdenResolveTask SIR.ValueRef]
      , [NameResolve.Task.IdenResolveTask Type.ADT.VariantIndex]
      , [NameResolve.Task.TypeExprEvalTask]
      , [NameResolve.Task.TypeExprEvalAsTypeTask]
      )
    , [InfixGroup.Task.InfixGroupTask]
    , [SolveTypes.Task.TypeSolveTask]
    ) ->
    StateT
        ( ( NameResolveResultArena.IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
          , NameResolveResultArena.IdenResolvedArena SIR.ValueRef
          , NameResolveResultArena.IdenResolvedArena Type.ADT.VariantIndex
          , NameResolveResultArena.TypeExprEvaledArena
          , NameResolveResultArena.TypeExprEvaledAsTypeArena
          )
        , InfixGroupResultArena.InfixGroupedArena
        )
        ( ReaderT
            (Arena.Arena NameResolve.NameMaps.NameMapStack NameResolve.NameMaps.NameMapStackKey, NameResolve.NameMaps.SIRChildMaps, SIR.SIR Solving.SolvingStage)
            (SolveMonad.SolveMonad (Compiler.WithDiagnostics NameResolve.Error.Error Void))
        )
        ()
solve'
    ( (decl_resolve_tasks, value_resolve_tasks, variant_resolve_tasks, type_expr_eval_tasks, type_expr_eval_as_type_tasks)
        , infix_group_tasks
        , type_solve_tasks
        ) = do
        -- TODO: sort by priority and ability to add new tasks
        -- (resort by priority when new tasks are added, but if no new tasks are added then the new queue is a subsequence of the original queue so we dont need to resort)
        (decl_resolve_tasks, changed1) <- go NameResolve.Resolve.resolve_decl_iden decl_resolve_tasks
        (value_resolve_tasks, changed2) <- go NameResolve.Resolve.resolve_value_iden value_resolve_tasks
        (variant_resolve_tasks, changed3) <- go NameResolve.Resolve.resolve_variant_iden variant_resolve_tasks
        (type_expr_eval_tasks, changed4) <- go NameResolve.Resolve.eval_type_expr type_expr_eval_tasks
        (type_expr_eval_as_type_tasks, changed5) <- go NameResolve.Resolve.eval_type_expr_as_type type_expr_eval_as_type_tasks

        (infix_group_tasks, changed6) <- go InfixGroup.Group.group infix_group_tasks

        (type_solve_tasks, changed7) <- go SolveTypes.Solve.solve type_solve_tasks

        when (changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7) $
            solve'
                ( (decl_resolve_tasks, value_resolve_tasks, variant_resolve_tasks, type_expr_eval_tasks, type_expr_eval_as_type_tasks)
                , infix_group_tasks
                , type_solve_tasks
                )
        where
            go :: Monad m => (task -> m (ProgressMade task)) -> [task] -> m ([task], Bool)
            go _ [] = pure ([], False)
            go solve tasks = do
                (changed, retained_tasks, new_tasks) <-
                    ( tasks
                            & mapM
                                ( \task ->
                                    solve task >>= \case
                                        Unsuccessful -> pure ([False], [task], [])
                                        Successful new_tasks -> pure ([True], [], new_tasks)
                                )
                        )
                        <&> mconcat

                let next_tasks = if not $ null new_tasks {- TODO: sortOn priority -} then retained_tasks ++ new_tasks else retained_tasks
                pure (next_tasks, or changed)

-- TODO: remove this
-- -- this does both type inference and type checking
-- solve :: TypeSolver.SolverState -> [TypeSolver.Constraint] -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
-- solve nr_solver_state backlog (SIR.SIR mods adts type_synonyms quant_vars variables (SIR.CU root_module main_function)) = -- TODO: do not destructure ir?
--     TypeSolver.run_solve_monad_with
--         (
--             let get_type_synonym ts_key = pure $ Arena.get type_synonyms ts_key
--             in
--             mapM_ (\ constraint -> do
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
