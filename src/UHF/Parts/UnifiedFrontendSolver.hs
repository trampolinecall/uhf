module UHF.Parts.UnifiedFrontendSolver (solve) where

import UHF.Prelude

import Data.Functor.Const (Const)
import Data.List (sortOn)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.Error (Error)
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Finalize as InfixGroup.Finalize
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult, InfixGroupedKey)
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result as InfixGroup.Result
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Prepare as InfixGroup.Prepare
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Solve as InfixGroup.Solve
import qualified UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task as InfixGroup.Task
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize as NameResolve.Finalize
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameResolve.NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result as NameResolve.Result
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.OtherPreparation.AssignNameMaps as NameResolve.OtherPreparation.AssignNameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare as NameResolve.Prepare
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Solve as NameResolve.Solve
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Task as NameResolve.Task
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import qualified UHF.Parts.UnifiedFrontendSolver.Solving as Solving
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Finalize as TypeSolve.Finalize
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Prepare as TypeSolve.Prepare
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Solve as TypeSolve.Solve
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task as SolveTypes.Task
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task as TypeSolve.Task
import qualified UHF.Util.Arena as Arena

type PreSolve = ((), Const () (), (), (), (), (), ())
type PostSolve =
    (NameResolve.NameMaps.NameContextKey, IdenResolvedKey (), Type.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, Maybe Type.Type, InfixGroupedKey)

solve ::
    SIR.SIR PreSolve ->
    Compiler.WithDiagnostics
        Error
        Void
        ( SIR.SIR PostSolve
        , ( Arena.Arena (Maybe (SIR.DeclRef Type.Type)) (IdenResolvedKey (SIR.DeclRef Type.Type))
          , Arena.Arena (Maybe SIR.ValueRef) (IdenResolvedKey SIR.ValueRef)
          , Arena.Arena (Maybe Type.ADT.VariantIndex) (IdenResolvedKey Type.ADT.VariantIndex)
          , Arena.Arena (Maybe (SIR.DeclRef Type.Type)) TypeExprEvaledKey
          , Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey
          )
        , Arena.Arena (Maybe InfixGroupResult) InfixGroupedKey
        )
solve sir = do
    -- TODO: clean this up
    (sir, name_map_stack_arena, sir_child_maps) <- NameResolve.OtherPreparation.AssignNameMaps.assign sir
    let (sir', name_resolution_results, name_resolution_tasks) = NameResolve.Prepare.prepare sir
    let (sir'', infix_group_results, infix_group_tasks) = InfixGroup.Prepare.prepare sir'
    let (sir''', infer_vars, type_solving_tasks) = TypeSolve.Prepare.add_types sir''

    ((), (name_resolution_results, infix_group_results, infer_vars)) <-
        runReaderT
            (runStateT (solve' ((\ (a, b, c, d, e) -> (a, b, c, map Right d, e)) name_resolution_tasks, infix_group_tasks, type_solving_tasks)) (name_resolution_results, infix_group_results, infer_vars))
            (name_map_stack_arena, sir_child_maps, sir''')

    (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena) <-
        NameResolve.Finalize.finalize name_resolution_results
    infix_group_results <- InfixGroup.Finalize.finalize infix_group_results
    (sir'''', decl_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena) <-
        TypeSolve.Finalize.remove_infer_vars infer_vars decl_iden_resolved_arena type_expr_evaled_arena type_expr_evaled_as_type_arena sir'''

    pure
        ( sir''''
        , (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
        , infix_group_results
        )

solve' ::
    ( ( [NameResolve.Task.IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)]
      , [NameResolve.Task.IdenResolveTask SIR.ValueRef]
      , [NameResolve.Task.IdenResolveTask Type.ADT.VariantIndex]
      , [Either TypeSolve.Task.TypeSolveTask NameResolve.Task.TypeExprEvalTask]
      , [NameResolve.Task.TypeExprEvalAsTypeTask]
      )
    , [InfixGroup.Task.InfixGroupTask]
    , [SolveTypes.Task.TypeSolveTask]
    ) ->
    StateT
        ( ( NameResolve.Result.IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
          , NameResolve.Result.IdenResolvedArena SIR.ValueRef
          , NameResolve.Result.IdenResolvedArena Type.ADT.VariantIndex
          , NameResolve.Result.TypeExprEvaledArena
          , NameResolve.Result.TypeExprEvaledAsTypeArena
          )
        , InfixGroup.Result.InfixGroupedArena
        , TypeWithInferVar.InferVarArena
        )
        ( ReaderT
            (Arena.Arena NameResolve.NameMaps.NameContext NameResolve.NameMaps.NameContextKey, NameResolve.NameMaps.SIRChildMaps, SIR.SIR Solving.SolvingStage)
            (Compiler.WithDiagnostics Error Void)
        )
        ()
solve'
    ( (decl_resolve_tasks, value_resolve_tasks, variant_resolve_tasks, type_expr_eval_tasks, type_expr_eval_as_type_tasks)
        , infix_group_tasks
        , type_solve_tasks
        ) = do
        (decl_resolve_tasks, changed1) <- go NameResolve.Task.iden_resolve_task_priority NameResolve.Solve.resolve_decl_iden decl_resolve_tasks
        (value_resolve_tasks, changed2) <- go NameResolve.Task.iden_resolve_task_priority NameResolve.Solve.resolve_value_iden value_resolve_tasks
        (variant_resolve_tasks, changed3) <- go NameResolve.Task.iden_resolve_task_priority NameResolve.Solve.resolve_variant_iden variant_resolve_tasks
        (type_expr_eval_tasks, changed4) <-
            go
                (either TypeSolve.Task.priority NameResolve.Task.type_expr_eval_task_priority)
                (either (fmap (fmap Left) . TypeSolve.Solve.solve) NameResolve.Solve.eval_type_expr)
                type_expr_eval_tasks -- TODO: this is a really hacky solution
        (type_expr_eval_as_type_tasks, changed5) <-
            go NameResolve.Task.type_expr_eval_as_type_priority NameResolve.Solve.eval_type_expr_as_type type_expr_eval_as_type_tasks

        (infix_group_tasks, changed6) <- go InfixGroup.Task.priority InfixGroup.Solve.group infix_group_tasks

        (type_solve_tasks, changed7) <- go TypeSolve.Task.priority TypeSolve.Solve.solve type_solve_tasks

        when (changed1 || changed2 || changed3 || changed4 || changed5 || changed6 || changed7) $
            solve'
                ( (decl_resolve_tasks, value_resolve_tasks, variant_resolve_tasks, type_expr_eval_tasks, type_expr_eval_as_type_tasks)
                , infix_group_tasks
                , type_solve_tasks
                )
        where
            go :: Monad m => (task -> Int) -> (task -> m (ProgressMade task)) -> [task] -> m ([task], Bool)
            go _ _ [] = pure ([], False)
            go priority solve tasks = do
                (changed, retained_tasks, new_tasks) <-
                    ( tasks
                            & mapM
                                ( \task ->
                                    solve task >>= \case
                                        NoProgressMade -> pure ([False], [task], [])
                                        ProgressMade new_tasks -> pure ([True], [], new_tasks)
                                )
                        )
                        <&> mconcat

                -- lower priority numbers mean higher priority (because lower numbers get sorted to the front)
                -- sort again by priority when new tasks are added, but if no new tasks are added then the new queue is a subsequence of the original queue so we dont need to resort
                let next_tasks = if not $ null new_tasks then sortOn priority (retained_tasks ++ new_tasks) else retained_tasks
                pure (next_tasks, or changed)
