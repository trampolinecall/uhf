module UHF.Parts.UnifiedFrontendSolver.NameResolve.Resolve (resolve) where

import UHF.Prelude

import Control.Arrow (first, second)
import Data.Functor.Const (Const (Const))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NRReader as NRReader
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

-- TODO: ProgressMade is never updated

type PreResolve = (NameMaps.NameMapStackKey, ResolveResult () Compiler.ErrorReportedPromise (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())
type PostResolve =
    ( NameMaps.NameMapStackKey
    , ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise () -- the best effort error is a Maybe in case some of the names cant be resolved because one of their dependencies is inconclusive, in which case we don't have an error to report for that name
    , SIR.DeclRef TypeSolver.Type
    , TypeSolver.Type
    , ()
    , ()
    )

type PrevStep prev_bee =
    (NameMaps.NameMapStackKey, ResolveResult prev_bee Compiler.ErrorReportedPromise (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())

type NameMapStackArena = Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey
type ResolveMonad =
    WriterT
        ProgressMade
        ( ReaderT
            NameMapStackArena
            ( NRReader.NRReader
                (Arena.Arena (SIR.ADT PreResolve) Type.ADTKey)
                (Arena.Arena (SIR.TypeSynonym PreResolve) Type.TypeSynonymKey)
                ()
                (Arena.Arena Type.QuantVar Type.QuantVarKey)
                NameMaps.SIRChildMaps
                (WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad Error.WithErrors))
            )
        )

resolve ::
    Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey ->
    NameMaps.SIRChildMaps ->
    SIR.SIR PreResolve ->
    Error.WithErrors (SIR.SIR PostResolve, [TypeSolver.Constraint], TypeSolver.SolverState)
resolve name_map_stack_arena sir_child_maps sir@(SIR.SIR _ adts type_synonyms type_vars _ (SIR.CU _ _)) = do
    ((sir, constraints), solver_state) <-
        TypeSolver.run_solve_monad $ runWriterT $ runReaderT (runReaderT (go sir) name_map_stack_arena) (adts, type_synonyms, (), type_vars, sir_child_maps)
    pure (sir, constraints, solver_state)
    where
        go ::
            SIR.SIR (PrevStep prev_bee) ->
            ReaderT
                NameMapStackArena
                ( NRReader.NRReader
                    (Arena.Arena (SIR.ADT PreResolve) Type.ADTKey)
                    (Arena.Arena (SIR.TypeSynonym PreResolve) Type.TypeSynonymKey)
                    ()
                    (Arena.Arena Type.QuantVar Type.QuantVarKey)
                    NameMaps.SIRChildMaps
                    (WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad Error.WithErrors))
                )
                (SIR.SIR PostResolve)
        go sir = do
            (sir', progress_made) <- runWriterT $ resolve_single_step sir
            case progress_made of
                NoProgressMade -> pure sir'
                ProgressMade -> go sir'

data ProgressMade = NoProgressMade | ProgressMade deriving Show
instance Semigroup ProgressMade where
    NoProgressMade <> NoProgressMade = NoProgressMade
    ProgressMade <> NoProgressMade = ProgressMade
    NoProgressMade <> ProgressMade = ProgressMade
    ProgressMade <> ProgressMade = ProgressMade
instance Monoid ProgressMade where
    mempty = NoProgressMade

resolve_single_step ::
    SIR.SIR (PrevStep prev_bee) ->
    ResolveMonad (SIR.SIR PostResolve)
resolve_single_step (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    mods <- Arena.transformM resolve_in_module mods
    adts <- Arena.transformM resolve_in_adt adts
    type_synonyms <- Arena.transformM resolve_in_type_synonym type_synonyms
    pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

resolve_in_module :: SIR.Module (PrevStep prev_bee) -> ResolveMonad (SIR.Module PostResolve)
resolve_in_module (SIR.Module id name_maps bindings adts type_synonyms) = do
    bindings <- mapM resolve_in_binding bindings
    pure $ SIR.Module id name_maps bindings adts type_synonyms

resolve_in_adt :: SIR.ADT (PrevStep prev_bee) -> ResolveMonad (SIR.ADT PostResolve)
resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
    where
        resolve_in_variant ::
            Type.ADT.Variant (SIR.TypeExpr (PrevStep prev_bee1), ResolveResult prev_bee1 Compiler.ErrorReportedPromise TypeSolver.Type) ->
            ResolveMonad (Type.ADT.Variant (SIR.TypeExpr PostResolve, ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeSolver.Type))
        resolve_in_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    ( \(id, name, (texpr, as_type)) -> do
                        texpr <- resolve_in_type_expr texpr
                        as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
                        pure (id, name, (texpr, as_type))
                    )
                    fields
        resolve_in_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM
                    ( \(id, (texpr, as_type)) -> do
                        texpr <- resolve_in_type_expr texpr
                        as_type <- if_inconclusive as_type (type_expr_evaled_as_type texpr)
                        pure (id, (texpr, as_type))
                    )
                    fields

resolve_in_type_synonym :: SIR.TypeSynonym (PrevStep prev_bee) -> ResolveMonad (SIR.TypeSynonym PostResolve)
resolve_in_type_synonym (Type.TypeSynonym id name (expansion, expansion_as_type)) = do
    expansion <- resolve_in_type_expr expansion
    expansion_as_type <- if_inconclusive expansion_as_type (type_expr_evaled_as_type expansion)
    pure $ Type.TypeSynonym id name (expansion, expansion_as_type)

resolve_in_binding :: SIR.Binding (PrevStep prev_bee) -> ResolveMonad (SIR.Binding PostResolve)
resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr

resolve_in_type_expr :: SIR.TypeExpr (PrevStep prev_bee) -> ResolveMonad (SIR.TypeExpr PostResolve)
resolve_in_type_expr (SIR.TypeExpr'Refer _ sp nc_stack id resolved) = do
    resolved <- if_inconclusive resolved (look_up_decl nc_stack id)
    pure $ SIR.TypeExpr'Refer resolved sp nc_stack id resolved
resolve_in_type_expr (SIR.TypeExpr'Get evaled sp parent name) = do
    parent <- resolve_in_type_expr parent
    evaled <-
        if_inconclusive
            evaled
            ( case SIR.type_expr_evaled parent of
                Inconclusive _ -> pure (Inconclusive Nothing)
                Errored err -> pure $ Errored err
                Resolved r -> get_decl_child r name
            )
    pure $ SIR.TypeExpr'Get evaled sp parent name
resolve_in_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = do
    a <- resolve_in_type_expr a
    b <- resolve_in_type_expr b
    evaled <-
        if_inconclusive
            evaled
            ( do
                a' <- type_expr_evaled_as_type a
                b' <- type_expr_evaled_as_type b
                pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Tuple <$> a' <*> b')
            )
    pure $ SIR.TypeExpr'Tuple evaled sp a b
resolve_in_type_expr (SIR.TypeExpr'Hole evaled evaled_as_type sp hid) = do
    evaled_as_type <- if_inconclusive evaled_as_type (Resolved <$> make_infer_var (TypeSolver.TypeHole sp))
    evaled <- if_inconclusive evaled (pure $ SIR.DeclRef'Type <$> evaled_as_type)
    pure $ SIR.TypeExpr'Hole evaled evaled_as_type sp hid
resolve_in_type_expr (SIR.TypeExpr'Function evaled sp arg res) = do
    arg <- resolve_in_type_expr arg
    res <- resolve_in_type_expr res
    evaled <-
        if_inconclusive
            evaled
            ( do
                arg' <- type_expr_evaled_as_type arg
                res' <- type_expr_evaled_as_type res
                pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Function <$> arg' <*> res')
            )
    pure $ SIR.TypeExpr'Function evaled sp arg res
resolve_in_type_expr (SIR.TypeExpr'Forall evaled sp name_maps vars inner) = do
    inner <- resolve_in_type_expr inner
    evaled <-
        if_inconclusive
            evaled
            ( do
                inner' <- type_expr_evaled_as_type inner
                pure $ SIR.DeclRef'Type <$> (TypeSolver.Type'Forall vars <$> inner')
            )
    pure $ SIR.TypeExpr'Forall evaled sp name_maps vars inner
resolve_in_type_expr (SIR.TypeExpr'Apply evaled sp ty arg) = do
    ty <- resolve_in_type_expr ty
    arg <- resolve_in_type_expr arg
    evaled <-
        if_inconclusive
            evaled
            ( do
                ty' <- type_expr_evaled_as_type ty
                arg' <- type_expr_evaled_as_type arg

                result_ty <- case (ty', arg') of
                    (Resolved ty', Resolved arg') -> do
                        adts <- lift $ lift NRReader.ask_adt_arena
                        type_synonyms <- lift $ lift NRReader.ask_type_synonym_arena
                        quant_vars <- lift $ lift NRReader.ask_quant_var_arena
                        lift (lift $ lift $ lift $ TypeSolver.apply_type adts type_synonyms todo quant_vars (TypeSolver.TypeExpr sp) sp ty' arg') -- TODO: figure this out
                            >>= \case
                                TypeSolver.AppliedResult res -> pure $ Resolved res
                                TypeSolver.AppliedError err -> do
                                    _ <- lift $ lift $ lift $ lift $ lift $ Compiler.tell_error (Error.Error'SolveError err)
                                    Resolved <$> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: fix duplication of this for_what
                                TypeSolver.Inconclusive ty constraint -> do
                                    lift $ tell [constraint]
                                    pure $ Resolved ty
                    (Inconclusive _, _) -> pure $ Inconclusive Nothing
                    (_, Inconclusive _) -> pure $ Inconclusive Nothing
                    (Errored e, _) -> pure $ Errored e
                    (_, Errored e) -> pure $ Errored e

                pure $ SIR.DeclRef'Type <$> result_ty
            )
    pure $ SIR.TypeExpr'Apply evaled sp ty arg
resolve_in_type_expr (SIR.TypeExpr'Wild evaled sp) = do
    evaled <-
        if_inconclusive
            evaled
            ( do
                infer_var <- make_infer_var (TypeSolver.TypeExpr sp)
                pure $ SIR.DeclRef'Type <$> Resolved infer_var
            )
    pure $ SIR.TypeExpr'Wild evaled sp
resolve_in_type_expr (SIR.TypeExpr'Poison evaled sp) = do
    evaled <-
        if_inconclusive
            evaled
            ( do
                infer_var <- make_infer_var (TypeSolver.TypeExpr sp)
                pure $ SIR.DeclRef'Type <$> Resolved infer_var
            )
    pure $ SIR.TypeExpr'Poison evaled sp

type_expr_evaled_as_type ::
    SIR.TypeExpr (PrevStep prev_bee) -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise TypeSolver.Type)
type_expr_evaled_as_type te =
    let sp = SIR.type_expr_span te
        d = SIR.type_expr_evaled te
    in case d of
        Resolved (SIR.DeclRef'Module _) -> do
            _ <- lift $ lift $ lift $ lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "a module")
            Resolved <$> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
        Resolved (SIR.DeclRef'Type ty) -> pure $ Resolved ty
        Resolved (SIR.DeclRef'ExternPackage _) -> do
            _ <- lift $ lift $ lift $ lift $ lift $ Compiler.tell_error (Error.Error'NotAType sp "external package")
            Resolved <$> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: don't make variables for these?
        Errored _ -> Resolved <$> make_infer_var (TypeSolver.TypeExpr sp) -- TODO: make this message better
        Inconclusive _ -> pure (Inconclusive Nothing)

resolve_in_expr :: SIR.Expr (PrevStep prev_bee) -> ResolveMonad (SIR.Expr PostResolve)
resolve_in_expr (SIR.Expr'Refer id type_info sp iden resolved) = do
    iden <- resolve_split_iden look_up_value get_value_child iden
    result <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved iden)
    pure $ SIR.Expr'Refer id type_info sp iden result
resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b
resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body
resolve_in_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) =
    SIR.Expr'Let id type_info sp name_maps
        <$> mapM resolve_in_binding bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> resolve_in_expr body
resolve_in_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = do
    SIR.Expr'LetRec id type_info sp name_maps
        <$> mapM resolve_in_binding bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> resolve_in_expr body
resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr first
        <*> mapM
            ( \(sp, iden, resolved, rhs) -> do
                rhs <- resolve_in_expr rhs
                iden <- resolve_split_iden look_up_value get_value_child iden
                result <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved iden)
                pure (sp, iden, result, rhs)
            )
            ops
resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg
resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> resolve_in_expr e
        <*> mapM
            ( \(name_maps, pat, expr) -> do
                pat' <- resolve_in_pat pat
                expr' <- resolve_in_expr expr
                pure (name_maps, pat', expr')
            )
            arms
resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e) = do
    e <- resolve_in_expr e
    tye <- resolve_in_type_expr tye
    tye_as_type <- if_inconclusive tye_as_type (type_expr_evaled_as_type tye)
    pure $ SIR.Expr'TypeAnnotation id type_info sp (tye, tye_as_type) e
resolve_in_expr (SIR.Expr'Forall id type_info sp name_maps vars e) = SIR.Expr'Forall id type_info sp name_maps vars <$> resolve_in_expr e
resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)) = do
    e <- resolve_in_expr e
    arg <- resolve_in_type_expr arg
    arg_as_type <- if_inconclusive arg_as_type (type_expr_evaled_as_type arg)
    pure $ SIR.Expr'TypeApply id type_info sp e (arg, arg_as_type)
resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

resolve_in_pat :: SIR.Pattern (PrevStep prev_bee) -> ResolveMonad (SIR.Pattern PostResolve)
resolve_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats) = do
    subpats <- mapM resolve_in_pat subpats
    variant_iden <- resolve_split_iden look_up_variant get_variant_child variant_iden
    resolved <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved variant_iden)
    pure $ SIR.Pattern'AnonADTVariant type_info sp variant_iden resolved tyargs subpats
resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats) = do
    subpats <- mapM (\(field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpats
    variant_iden <- resolve_split_iden look_up_variant get_variant_child variant_iden
    resolved <- if_inconclusive resolved (pure $ SIR.split_identifier_resolved variant_iden)
    pure $ SIR.Pattern'NamedADTVariant type_info sp variant_iden resolved tyargs subpats
resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_split_iden ::
    (NameMaps.NameMapStackKey -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    (SIR.DeclRef TypeSolver.Type -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise resolved)) ->
    SIR.SplitIdentifier resolved (PrevStep prev_bee) ->
    ResolveMonad (SIR.SplitIdentifier resolved PostResolve)
resolve_split_iden _ resolve_get (SIR.SplitIdentifier'Get texpr next resolved) = do
    texpr' <- resolve_in_type_expr texpr
    let texpr_evaled = SIR.type_expr_evaled texpr'
    resolved <- if_inconclusive resolved $ case texpr_evaled of
        Resolved texpr_evaled -> resolve_get texpr_evaled next
        Errored err -> pure $ Errored err
        Inconclusive _ -> pure (Inconclusive Nothing)
    pure (SIR.SplitIdentifier'Get texpr' next resolved)
resolve_split_iden resolve_single _ (SIR.SplitIdentifier'Single name_maps name resolved) =
    case resolved of
        Inconclusive _ -> do
            result <- resolve_single name_maps name
            pure (SIR.SplitIdentifier'Single name_maps name result)
        Errored err -> pure (SIR.SplitIdentifier'Single name_maps name (Errored err))
        Resolved result -> pure (SIR.SplitIdentifier'Single name_maps name (Resolved result))

look_up_decl ::
    NameMaps.NameMapStackKey ->
    Located Text ->
    ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeSolver.Type))
look_up_decl name_maps_stack_key name = lift ask >>= \name_maps_arena -> report_errored $ NameMaps.look_up_decl name_maps_arena name_maps_stack_key name
look_up_value ::
    NameMaps.NameMapStackKey -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise SIR.ValueRef)
look_up_value name_maps_stack_key name = lift ask >>= \name_maps_arena -> report_errored $ NameMaps.look_up_value name_maps_arena name_maps_stack_key name
look_up_variant ::
    NameMaps.NameMapStackKey -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
look_up_variant name_maps_stack_key name = lift ask >>= \name_maps_arena -> report_errored $ NameMaps.look_up_variant name_maps_arena name_maps_stack_key name

get_decl_child ::
    SIR.DeclRef TypeSolver.Type ->
    Located Text ->
    ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeSolver.Type))
get_decl_child parent name = lift (lift $ NRReader.ask_sir_child_maps) >>= \sir_child_maps -> report_errored $ NameMaps.get_decl_child sir_child_maps parent name
get_value_child ::
    SIR.DeclRef TypeSolver.Type -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise SIR.ValueRef)
get_value_child parent name = lift (lift $ NRReader.ask_sir_child_maps) >>= \sir_child_maps -> report_errored $ NameMaps.get_value_child sir_child_maps parent name
get_variant_child ::
    SIR.DeclRef TypeSolver.Type -> Located Text -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
get_variant_child parent name = lift (lift $ NRReader.ask_sir_child_maps) >>= \sir_child_maps -> report_errored $ NameMaps.get_variant_child sir_child_maps parent name

report_errored :: ResolveResult Error.Error Error.Error res -> ResolveMonad (ResolveResult (Maybe Error.Error) Compiler.ErrorReportedPromise res)
report_errored (Resolved res) = pure $ Resolved res
report_errored (Errored err) = Errored <$> lift (lift $ lift $ lift $ lift $ Compiler.tell_error err)
report_errored (Inconclusive bee) = pure $ Inconclusive (Just bee)

make_infer_var :: TypeSolver.InferVarForWhat -> ResolveMonad TypeSolver.Type
make_infer_var for_what = do
    ifv <- lift $ lift $ lift $ lift $ TypeSolver.new_infer_var for_what
    pure $ TypeSolver.Type'InferVar ifv
