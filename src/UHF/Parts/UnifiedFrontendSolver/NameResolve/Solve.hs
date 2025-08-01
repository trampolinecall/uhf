{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Solve (resolve_decl_iden, resolve_value_iden, resolve_variant_iden, eval_type_expr, eval_type_expr_as_type) where

import UHF.Prelude

import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.Error (Error (NRError))
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.EvaledAsType (evaled_as_type)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef (..), ValueRef)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenResults
    , ValueIdenResults
    , VariantIdenResults, TypeExprsEvaled, TypeExprsEvaledAsTypes
    )
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalAsTypeTask (..), TypeExprEvalTask (..))
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, get_decl_iden_resolved, get_type_expr_evaled)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (Constraint (InferVarIsApplyResult), TypeSolveTask (Constraint))
import UHF.Source.Located (Located (Located))
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

decl_iden_resolved_selector :: State DeclIdenResults () -> SolveMonad ()
decl_iden_resolved_selector s =
    state $
        \( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            , infer_vars
            ) ->
                let ((), decl_iden_results') = runState s decl_iden_results
                in ( ()
                   ,
                       ( (decl_iden_results', value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       , infer_vars
                       )
                   )

value_iden_resolved_selector :: State ValueIdenResults () -> SolveMonad ()
value_iden_resolved_selector s =
    state $
        \( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            , infer_vars
            ) ->
                let ((), value_iden_results') = runState s value_iden_results
                in ( ()
                   ,
                       ( (decl_iden_results, value_iden_results', variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       , infer_vars
                       )
                   )
variant_iden_resolved_selector :: State VariantIdenResults () -> SolveMonad ()
variant_iden_resolved_selector s =
    state $
        \( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            , infer_vars
            ) ->
                let ((), variant_iden_results') = runState s variant_iden_results
                in ( ()
                   ,
                       ( (decl_iden_results, value_iden_results, variant_iden_results', type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       , infer_vars
                       )
                   )

type_expr_evaled_selector :: State TypeExprsEvaled () -> SolveMonad ()
type_expr_evaled_selector s =
    state $
        \( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            , infer_vars
            ) ->
                let ((), type_expr_evaled_arena') = runState s type_expr_evaled_arena
                in ( ()
                   ,
                       ( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena', type_expr_evaled_as_type_arena)
                       , infix_group_results
                       , infer_vars
                       )
                   )
type_expr_evaled_as_type_selector :: State TypeExprsEvaledAsTypes () -> SolveMonad ()
type_expr_evaled_as_type_selector s =
    state $
        \( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            , infer_vars
            ) ->
                let ((), type_expr_evaled_as_type_arena') = runState s type_expr_evaled_as_type_arena
                in ( ()
                   ,
                       ( (decl_iden_results, value_iden_results, variant_iden_results, type_expr_evaled_arena, type_expr_evaled_as_type_arena')
                       , infix_group_results
                       , infer_vars
                       )
                   )

resolve_decl_iden :: IdenResolveTask (SIR.ID.ID "DeclIden") -> SolveMonad (ProgressMade (IdenResolveTask (SIR.ID.ID "DeclIden")))
resolve_decl_iden = resolve decl_iden_resolved_selector look_up_decl get_decl_child
resolve_value_iden :: IdenResolveTask (SIR.ID.ID "ValueIden") -> SolveMonad (ProgressMade (IdenResolveTask (SIR.ID.ID "ValueIden")))
resolve_value_iden = resolve value_iden_resolved_selector look_up_value get_value_child
resolve_variant_iden :: IdenResolveTask (SIR.ID.ID "VariantIden") -> SolveMonad (ProgressMade (IdenResolveTask (SIR.ID.ID "VariantIden")))
resolve_variant_iden = resolve variant_iden_resolved_selector look_up_variant get_variant_child

resolve ::
    (State (Map (SIR.ID.ID id_name) (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) () -> SolveMonad ()) ->
    (NameMaps.NameContextKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    (DeclRef TypeWithInferVar.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    IdenResolveTask (SIR.ID.ID id_name) ->
    SolveMonad (ProgressMade (IdenResolveTask (SIR.ID.ID id_name)))
resolve selector resolve_root _ (ResolveRoot name_context name result_key) = do
    result <- resolve_root name_context name
    put_result selector result_key result
resolve selector _ resolve_get (ResolveGet parent name result_key) = do
    parent_evaled <- get_type_expr_evaled parent
    result <- case parent_evaled of
        Solved texpr_evaled -> resolve_get texpr_evaled name
        Errored erp -> pure $ Errored erp
        Inconclusive _ -> pure (Inconclusive Nothing)
    put_result selector result_key result

put_result ::
    Ord key =>
    (State (Map key (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result)) () -> SolveMonad ()) ->
    key ->
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result ->
    SolveMonad (ProgressMade task)
put_result selector k res = do
    case res of
        Inconclusive _ -> pure NoProgressMade
        _ -> do
            selector $
                modify $
                    \map ->
                        Map.alter
                            ( \case
                                Just (Inconclusive _) -> Just res
                                _ -> Just res -- TODO: internal warning because there was already a result here and it was recomputed?
                            )
                            k
                            map
            pure $ ProgressMade []

look_up_decl ::
    NameMaps.NameContextKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
look_up_decl name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_decl name_maps_arena name_maps_stack_key name
look_up_value :: NameMaps.NameContextKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise ValueRef)
look_up_value name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_value name_maps_arena name_maps_stack_key name
look_up_variant ::
    NameMaps.NameContextKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
look_up_variant name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_variant name_maps_arena name_maps_stack_key name

get_decl_child ::
    DeclRef TypeWithInferVar.Type ->
    Located Text ->
    SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
get_decl_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_decl_child sir_child_maps parent name
get_value_child ::
    DeclRef TypeWithInferVar.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise ValueRef)
get_value_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_value_child sir_child_maps parent name
get_variant_child ::
    DeclRef TypeWithInferVar.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
get_variant_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_variant_child sir_child_maps parent name

report_errored :: SolveResult Error.Error Error.Error res -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise res)
report_errored (Solved res) = pure $ Solved res
report_errored (Errored err) = Errored <$> lift (lift $ Compiler.tell_error (NRError err))
report_errored (Inconclusive bee) = pure $ Inconclusive (Just bee)

eval_type_expr :: TypeExprEvalTask -> SolveMonad (ProgressMade (Either TypeSolveTask TypeExprEvalTask)) -- TODO: this Either is kind of a hacky solution but is needed for ApplyType to create a type solving constraint
eval_type_expr (GetFromDeclIdenResolved iden_resolved result_key) = do
    resolved <- get_decl_iden_resolved iden_resolved
    put_result type_expr_evaled_selector result_key resolved
eval_type_expr (MakeTuple (Located a_sp a) (Located b_sp b) result_key) = do
    a <- get_type_expr_evaled a
    b <- get_type_expr_evaled b

    evaled <- do
        a' <- type_expr_evaled_as_type a_sp a
        b' <- type_expr_evaled_as_type b_sp b
        pure $ DeclRef'Type <$> (TypeWithInferVar.Type'Tuple <$> a' <*> b')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeFunction (Located arg_sp arg) (Located res_sp res) result_key) = do
    arg <- get_type_expr_evaled arg
    res <- get_type_expr_evaled res

    evaled <- do
        arg' <- type_expr_evaled_as_type arg_sp arg
        res' <- type_expr_evaled_as_type res_sp res
        pure $ DeclRef'Type <$> (TypeWithInferVar.Type'Function <$> arg' <*> res')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeForall qvars (Located res_sp res) result_key) = do
    res <- get_type_expr_evaled res
    evaled <-
        do
            res' <- type_expr_evaled_as_type res_sp res
            pure $ DeclRef'Type <$> (TypeWithInferVar.Type'Forall qvars <$> res')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeApply whole_sp (Located ty_sp ty) (Located arg_sp arg) result_key) = do
    ty <- get_type_expr_evaled ty
    arg <- get_type_expr_evaled arg

    ty' <- type_expr_evaled_as_type ty_sp ty
    arg' <- type_expr_evaled_as_type arg_sp arg

    (result_ty, new_constraints) <- case (ty', arg') of
        (Solved ty', Solved arg') -> do
            infer_var <- make_infer_var (TypeWithInferVar.TypeExpr whole_sp)
            pure (Solved infer_var, [InferVarIsApplyResult whole_sp infer_var ty' arg'])
        (Inconclusive _, _) -> pure (Inconclusive Nothing, [])
        (_, Inconclusive _) -> pure (Inconclusive Nothing, [])
        (Errored e, _) -> pure (Errored e, [])
        (_, Errored e) -> pure (Errored e, [])

    case DeclRef'Type <$> result_ty of
        Inconclusive _ -> pure NoProgressMade
        _ -> do
            type_expr_evaled_selector $
                modify $
                    \map ->
                        Map.alter
                            ( \case
                                Just (Inconclusive _) -> Just $ DeclRef'Type . TypeWithInferVar.Type'InferVar <$> result_ty
                                _ -> Just $ DeclRef'Type . TypeWithInferVar.Type'InferVar <$> result_ty -- TODO: internal warning because there was already a result here and it was recomputed?
                            )
                            result_key
                            map
            pure $ ProgressMade $ map (Left . Constraint) new_constraints
eval_type_expr (MakeInferVar sp result_key) = do
    infer_var <- make_infer_var (TypeWithInferVar.TypeExpr sp)

    put_result type_expr_evaled_selector result_key (Solved $ DeclRef'Type $ TypeWithInferVar.Type'InferVar infer_var)

eval_type_expr_as_type :: TypeExprEvalAsTypeTask -> SolveMonad (ProgressMade TypeExprEvalAsTypeTask)
eval_type_expr_as_type (EvalAsType (Located sp te) result_key) = do
    te <- get_type_expr_evaled te
    te' <- type_expr_evaled_as_type sp te
    put_result type_expr_evaled_as_type_selector result_key te'

type_expr_evaled_as_type ::
    Span ->
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type) ->
    SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type)
type_expr_evaled_as_type sp (Solved dr) = case evaled_as_type sp dr of
    Right ty -> pure $ Solved ty
    Left err -> lift $ lift $ Errored <$> Compiler.tell_error (NRError $ Error.Error'NotAType err) -- TODO: report error and make an infer var instead of returning Errored? (this was the old behavior before the unified solver came in)
type_expr_evaled_as_type _ (Inconclusive _) = pure $ Inconclusive Nothing
type_expr_evaled_as_type sp (Errored _) = do
    Solved . TypeWithInferVar.Type'InferVar <$> make_infer_var (TypeWithInferVar.TypeExpr sp) -- TODO: make this message better

make_infer_var :: TypeWithInferVar.InferVarForWhat -> SolveMonad TypeWithInferVar.InferVarKey
make_infer_var for_what = state $ \(nr_things, ig_things, infer_vars) ->
    let (key, infer_vars') = Arena.put (TypeWithInferVar.InferVar for_what TypeWithInferVar.Fresh) infer_vars
    in (key, (nr_things, ig_things, infer_vars'))
