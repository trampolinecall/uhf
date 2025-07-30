module UHF.Parts.UnifiedFrontendSolver.NameResolve.Resolve (resolve_decl_iden, resolve_value_iden, resolve_variant_iden, eval_type_expr, eval_type_expr_as_type) where

import UHF.Prelude

import Control.Arrow (first, second)
import Data.Functor.Const (Const (Const))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedArena, InfixGroupedKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import UHF.Parts.UnifiedFrontendSolver.NameResolve.EvaledAsType (evaled_as_type)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NRReader as NRReader
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena
    ( IdenResolvedArena
    , IdenResolvedKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledKey
    )
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalAsTypeTask (..), TypeExprEvalTask (..))
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, ask_sir, get_decl_iden_resolved, get_type_expr_evaled)
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeWithInferVar
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.SolveMonad as SolveMonad
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

-- TODO: remove these
-- type PreResolve = (NameMaps.NameMapStackKey, SolveResult () Compiler.ErrorReportedPromise (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())
-- type PostResolve =
--     ( NameMaps.NameMapStackKey
--     , SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise () -- the best effort error is a Maybe in case some of the names cant be resolved because one of their dependencies is inconclusive, in which case we don't have an error to report for that name
--     , SIR.DeclRef TypeSolver.Type
--     , TypeSolver.Type
--     , ()
--     , ()
--     )
-- type PrevStep prev_bee =
--     (NameMaps.NameMapStackKey, SolveResult prev_bee Compiler.ErrorReportedPromise (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())

type NameMapStackArena = Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey

-- TODO: remove this?
-- ( ReaderT
--     NameMapStackArena -- TODO: put SIR here too
--     ( NRReader.NRReader
--         (Arena.Arena (SIR.ADT PreResolve) Type.ADTKey)
--         (Arena.Arena (SIR.TypeSynonym PreResolve) Type.TypeSynonymKey)
--         ()
--         (Arena.Arena Type.QuantVar Type.QuantVarKey)
--         NameMaps.SIRChildMaps
--         (WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad Error.WithErrors))
--     )
-- )
--
decl_iden_resolved_selector :: State (IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)) () -> SolveMonad ()
decl_iden_resolved_selector s =
    state $
        \( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            ) ->
                let ((), decl_iden_resolved_arena') = runState s decl_iden_resolved_arena
                in ( ()
                   ,
                       ( (decl_iden_resolved_arena', value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       )
                   )

value_iden_resolved_selector :: State (IdenResolvedArena SIR.ValueRef) () -> SolveMonad ()
value_iden_resolved_selector s =
    state $
        \( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            ) ->
                let ((), value_iden_resolved_arena') = runState s value_iden_resolved_arena
                in ( ()
                   ,
                       ( (decl_iden_resolved_arena, value_iden_resolved_arena', variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       )
                   )
variant_iden_resolved_selector :: State (IdenResolvedArena Type.ADT.VariantIndex) () -> SolveMonad ()
variant_iden_resolved_selector s =
    state $
        \( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            ) ->
                let ((), variant_iden_resolved_arena') = runState s variant_iden_resolved_arena
                in ( ()
                   ,
                       ( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena', type_expr_evaled_arena, type_expr_evaled_as_type_arena)
                       , infix_group_results
                       )
                   )

type_expr_evaled_selector :: State TypeExprEvaledArena () -> SolveMonad ()
type_expr_evaled_selector s =
    state $
        \( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            ) ->
                let ((), type_expr_evaled_arena') = runState s type_expr_evaled_arena
                in ( ()
                   ,
                       ( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena', type_expr_evaled_as_type_arena)
                       , infix_group_results
                       )
                   )
type_expr_evaled_as_type_selector :: State TypeExprEvaledAsTypeArena () -> SolveMonad ()
type_expr_evaled_as_type_selector s =
    state $
        \( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena)
            , infix_group_results
            ) ->
                let ((), type_expr_evaled_as_type_arena') = runState s type_expr_evaled_as_type_arena
                in ( ()
                   ,
                       ( (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena')
                       , infix_group_results
                       )
                   )

resolve_decl_iden ::
    IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type) -> SolveMonad (ProgressMade (IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)))
resolve_decl_iden = resolve decl_iden_resolved_selector look_up_decl get_decl_child
resolve_value_iden :: IdenResolveTask SIR.ValueRef -> SolveMonad (ProgressMade (IdenResolveTask SIR.ValueRef))
resolve_value_iden = resolve value_iden_resolved_selector look_up_value get_value_child
resolve_variant_iden :: IdenResolveTask Type.ADT.VariantIndex -> SolveMonad (ProgressMade (IdenResolveTask Type.ADT.VariantIndex))
resolve_variant_iden = resolve variant_iden_resolved_selector look_up_variant get_variant_child

resolve ::
    (State (Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved) (IdenResolvedKey resolved)) () -> SolveMonad ()) ->
    (NameMaps.NameMapStackKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    (SIR.DeclRef TypeSolver.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    IdenResolveTask resolved ->
    SolveMonad (ProgressMade (IdenResolveTask resolved))
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
    Arena.Key key =>
    (State (Arena.Arena (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result) key) () -> SolveMonad ()) ->
    key ->
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise result ->
    SolveMonad (ProgressMade task)
put_result selector k res = do
    case res of
        Inconclusive _ -> pure Unsuccessful
        _ -> do
            selector $
                modify $
                    \arena ->
                        Arena.modify
                            arena
                            k
                            ( \case
                                Inconclusive _ -> res
                                _ -> res -- TODO: internal warning because there was already a result here and it was recomputed?
                            )
            pure $ Successful []

look_up_decl ::
    NameMaps.NameMapStackKey ->
    Located Text ->
    SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeSolver.Type))
look_up_decl name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_decl name_maps_arena name_maps_stack_key name
look_up_value ::
    NameMaps.NameMapStackKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise SIR.ValueRef)
look_up_value name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_value name_maps_arena name_maps_stack_key name
look_up_variant ::
    NameMaps.NameMapStackKey -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
look_up_variant name_maps_stack_key name = ask >>= \(name_maps_arena, _, _) -> report_errored $ NameMaps.look_up_variant name_maps_arena name_maps_stack_key name

get_decl_child ::
    SIR.DeclRef TypeSolver.Type ->
    Located Text ->
    SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeSolver.Type))
get_decl_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_decl_child sir_child_maps parent name
get_value_child ::
    SIR.DeclRef TypeSolver.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise SIR.ValueRef)
get_value_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_value_child sir_child_maps parent name
get_variant_child ::
    SIR.DeclRef TypeSolver.Type -> Located Text -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
get_variant_child parent name = ask >>= \(_, sir_child_maps, _) -> report_errored $ NameMaps.get_variant_child sir_child_maps parent name

-- TODO: remove this?
report_errored :: SolveResult Error.Error Error.Error res -> SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise res)
report_errored (Solved res) = pure $ Solved res
report_errored (Errored err) = Errored <$> lift (lift $ lift $ Compiler.tell_error err)
report_errored (Inconclusive bee) = pure $ Inconclusive (Just bee)

eval_type_expr :: TypeExprEvalTask -> SolveMonad (ProgressMade TypeExprEvalTask)
eval_type_expr (GetFromDeclIdenResolved iden_resolved result_key) = do
    resolved <- get_decl_iden_resolved iden_resolved
    put_result type_expr_evaled_selector result_key resolved
eval_type_expr (MakeTuple a b result_key) = do
    a <- get_type_expr_evaled a
    b <- get_type_expr_evaled b

    evaled <- do
        a' <- type_expr_evaled_as_type a
        b' <- type_expr_evaled_as_type b
        pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Tuple <$> a' <*> b')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeFunction arg res result_key) = do
    arg <- get_type_expr_evaled arg
    res <- get_type_expr_evaled res

    evaled <- do
        arg' <- type_expr_evaled_as_type arg
        res' <- type_expr_evaled_as_type res
        pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Function <$> arg' <*> res')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeForall qvars res result_key) = do
    res <- get_type_expr_evaled res
    evaled <-
        do
            res' <- type_expr_evaled_as_type res
            pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Forall qvars <$> res')

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeApply ty arg result_key) = do
    ty <- get_type_expr_evaled ty
    arg <- get_type_expr_evaled arg

    evaled <- do
        ty' <- type_expr_evaled_as_type ty
        arg' <- type_expr_evaled_as_type arg

        result_ty <- case (ty', arg') of
            (Solved ty', Solved arg') -> do
                SIR.SIR _ adts type_synonyms quant_vars _ _ <- ask_sir
                lift (lift $ TypeSolver.apply_type (todo adts) (todo type_synonyms) todo quant_vars (TypeSolver.TypeExpr (todo {- sp -})) (todo {- sp -}) ty' arg') -- TODO: figure this out
                    >>= \case
                        TypeSolver.AppliedResult res -> pure $ Solved res
                        TypeSolver.AppliedError err -> do
                            _ <- lift $ lift $ lift $ Compiler.tell_error (Error.Error'SolveError err)
                            Solved <$> make_infer_var (TypeSolver.TypeExpr (todo {- sp -})) -- TODO: fix duplication of this for_what
                        TypeSolver.Inconclusive ty constraint -> do
                            -- tell [constraint] TODO: tell constraint
                            pure $ Solved ty
            (Inconclusive _, _) -> pure $ Inconclusive Nothing
            (_, Inconclusive _) -> pure $ Inconclusive Nothing
            (Errored e, _) -> pure $ Errored e
            (_, Errored e) -> pure $ Errored e

        pure $ SIR.DeclRef'Type <$> result_ty

    put_result type_expr_evaled_selector result_key evaled
eval_type_expr (MakeInferVar result_key) = do
    infer_var <- make_infer_var (TypeSolver.TypeExpr todo)

    put_result type_expr_evaled_selector result_key (Solved $ SIR.DeclRef'Type infer_var)

eval_type_expr_as_type :: TypeExprEvalAsTypeTask -> SolveMonad (ProgressMade TypeExprEvalAsTypeTask)
eval_type_expr_as_type (EvalAsType te result_key) = do
    te <- get_type_expr_evaled te
    te' <- type_expr_evaled_as_type te
    put_result type_expr_evaled_as_type_selector result_key te'

type_expr_evaled_as_type ::
    SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeWithInferVar.Type) ->
    SolveMonad (SolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type)
type_expr_evaled_as_type (Solved dr) = case evaled_as_type todo dr of -- TODO: span
    Right ty -> pure $ Solved ty
    Left err -> lift $ lift $ lift $ Errored <$> Compiler.tell_error (Error.Error'NotAType err) -- TODO: report error and make an infer var instead of returning Errored? (this was the old behavior before the unified solver came in)
type_expr_evaled_as_type (Inconclusive _) = pure $ Inconclusive Nothing
type_expr_evaled_as_type (Errored e) = do
    Solved <$> make_infer_var (TypeSolver.TypeExpr (todo)) -- TODO: make this message better
    -- TODO: span

make_infer_var :: TypeSolver.InferVarForWhat -> SolveMonad TypeSolver.Type
make_infer_var for_what = do
    ifv <- lift $ lift $ TypeSolver.new_infer_var for_what
    pure $ TypeSolver.Type'InferVar ifv

-- resolve ::
--     Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey ->
--     NameMaps.SIRChildMaps ->
--     SIR.SIR PreResolve ->
--     Error.WithErrors (SIR.SIR PostResolve, [TypeSolver.Constraint], TypeSolver.SolverState)
-- resolve name_map_stack_arena sir_child_maps sir@(SIR.SIR _ adts type_synonyms type_vars _ (SIR.CU _ _)) = do
--     ((sir, constraints), solver_state) <-
--         TypeSolver.run_solve_monad $ runWriterT $ runReaderT (runReaderT (go sir) name_map_stack_arena) (adts, type_synonyms, (), type_vars, sir_child_maps)
--     pure (sir, constraints, solver_state)
--     where
--         go ::
--             SIR.SIR (PrevStep prev_bee) ->
--             ReaderT
--                 NameMapStackArena
--                 ( NRReader.NRReader
--                     (Arena.Arena (SIR.ADT PreResolve) Type.ADTKey)
--                     (Arena.Arena (SIR.TypeSynonym PreResolve) Type.TypeSynonymKey)
--                     ()
--                     (Arena.Arena Type.QuantVar Type.QuantVarKey)
--                     NameMaps.SIRChildMaps
--                     (WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad Error.WithErrors))
--                 )
--                 (SIR.SIR PostResolve)
--         go sir = do
--             (sir', progress_made) <- runWriterT $ resolve_single_step sir
--             case progress_made of
--                 NoProgressMade -> pure sir'
--                 ProgressMade -> go sir'
--
--
-- resolve_single_step ::
--     SIR.SIR (PrevStep prev_bee) ->
--     SolveMonad (SIR.SIR PostResolve)
-- resolve_single_step (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
--     mods <- Arena.transformM resolve_in_module mods
--     adts <- Arena.transformM resolve_in_adt adts
--     type_synonyms <- Arena.transformM resolve_in_type_synonym type_synonyms
--     pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
--     where
--         change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n
--
-- resolve_in_module :: SIR.Module (PrevStep prev_bee) -> SolveMonad (SIR.Module PostResolve)
-- resolve_in_module (SIR.Module id name_maps bindings adts type_synonyms) = do
--     bindings <- mapM resolve_in_binding bindings
--     pure $ SIR.Module id name_maps bindings adts type_synonyms
--
-- resolve_in_adt :: SIR.ADT (PrevStep prev_bee) -> SolveMonad (SIR.ADT PostResolve)
-- resolve_in_adt (SIR.ADT id name type_vars variants) = SIR.ADT id name type_vars <$> mapM resolve_in_variant variants
--     where
--         resolve_in_variant :: SIR.ADTVariant (PrevStep prev_bee1) -> SolveMonad (SIR.ADTVariant PostResolve)
--         resolve_in_variant (SIR.ADTVariant'Named name id fields) =
--             SIR.ADTVariant'Named name id
--                 <$> mapM
--                     ( \(id, name, texpr) -> do
--                         texpr <- resolve_in_type_expr texpr
--                         -- as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
--                         pure (id, name, texpr)
--                     )
--                     fields
--         resolve_in_variant (SIR.ADTVariant'Anon name id fields) =
--             SIR.ADTVariant'Anon name id
--                 <$> mapM
--                     ( \(id, texpr) -> do
--                         texpr <- resolve_in_type_expr texpr
--                         -- as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
--                         pure (id, texpr)
--                     )
--                     fields
--
-- resolve_in_type_synonym :: SIR.TypeSynonym (PrevStep prev_bee) -> SolveMonad (SIR.TypeSynonym PostResolve)
-- resolve_in_type_synonym (SIR.TypeSynonym id name expansion) = do
--     expansion <- resolve_in_type_expr expansion
--     -- expansion_as_type <- if_inconclusive expansion_as_type (type_expr_evaled_as_type expansion)
--     pure $ SIR.TypeSynonym id name expansion
--
-- resolve_in_binding :: SIR.Binding (PrevStep prev_bee) -> SolveMonad (SIR.Binding PostResolve)
-- resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr
--
--
--
-- resolve_in_expr :: SIR.Expr (PrevStep prev_bee) -> SolveMonad (SIR.Expr PostResolve)
-- resolve_in_expr (SIR.Expr'Refer id type_info sp iden resolved) = do
--     iden <- resolve_split_iden look_up_value get_value_child iden
--     result <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved iden)
--     pure $ SIR.Expr'Refer id type_info sp iden result
-- resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
-- resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
-- resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
-- resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
-- resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
-- resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b
-- resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body
-- resolve_in_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) =
--     SIR.Expr'Let id type_info sp name_maps
--         <$> mapM resolve_in_binding bindings
--         <*> pure adts
--         <*> pure type_synonyms
--         <*> resolve_in_expr body
-- resolve_in_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = do
--     SIR.Expr'LetRec id type_info sp name_maps
--         <$> mapM resolve_in_binding bindings
--         <*> pure adts
--         <*> pure type_synonyms
--         <*> resolve_in_expr body
-- resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
--     SIR.Expr'BinaryOps id allowed type_info sp
--         <$> resolve_in_expr first
--         <*> mapM
--             ( \(sp, iden, resolved, rhs) -> do
--                 rhs <- resolve_in_expr rhs
--                 iden <- resolve_split_iden look_up_value get_value_child iden
--                 result <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved iden)
--                 pure (sp, iden, result, rhs)
--             )
--             ops
-- resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg
-- resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
-- resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
--     SIR.Expr'Match id type_info sp match_tok_sp
--         <$> resolve_in_expr e
--         <*> mapM
--             ( \(name_maps, pat, expr) -> do
--                 pat' <- resolve_in_pat pat
--                 expr' <- resolve_in_expr expr
--                 pure (name_maps, pat', expr')
--             )
--             arms
-- resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e) = do
--     e <- resolve_in_expr e
--     tye <- resolve_in_type_expr tye
--     tye_as_type <- if_inconclusive tye_as_type (type_expr_evaled_as_type tye)
--     pure $ SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e
-- resolve_in_expr (SIR.Expr'Forall id type_info sp name_maps vars e) = SIR.Expr'Forall id type_info sp name_maps vars <$> resolve_in_expr e
-- resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)) = do
--     e <- resolve_in_expr e
--     arg <- resolve_in_type_expr arg
--     arg_as_type <- if_inconclusive arg_as_type (type_expr_evaled_as_type arg)
--     pure $ SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)
-- resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
-- resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp
--
-- resolve_in_pat :: SIR.Pattern (PrevStep prev_bee) -> SolveMonad (SIR.Pattern PostResolve)
-- resolve_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
-- resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
-- resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
-- resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
-- resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats) = do
--     subpats <- mapM resolve_in_pat subpats
--     variant_iden <- resolve_split_iden look_up_variant get_variant_child variant_iden
--     resolved <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved variant_iden)
--     pure $ SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats
-- resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats) = do
--     subpats <- mapM (\(field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpats
--     variant_iden <- resolve_split_iden look_up_variant get_variant_child variant_iden
--     resolved <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved variant_iden)
--     pure $ SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats
-- resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp
