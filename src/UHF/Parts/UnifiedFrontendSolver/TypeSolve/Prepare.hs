module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Prepare (add_types) where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import UHF.Source.Located (Located (..))
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (TypeSolveTask (..), Constraint (..), ExpectInWhat (..), EqInWhat (..))
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey)
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import UHF.Data.IR.TypeWithInferVar (InferVarArena)

type Unadded = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), InfixGroupedKey)
type Added =
    (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, TypeWithInferVar.Type, InfixGroupedKey)

-- TODO: remove these
type TypedWithInferVarsDIden = Maybe (SIR.DeclRef TypeWithInferVar.Type)
type TypedWithInferVars = (NameMaps.NameContextKey, Maybe (), SIR.DeclRef TypeWithInferVar.Type, TypeWithInferVar.Type, TypeWithInferVar.Type, Void)
type TypedWithInferVarsSIR = SIR.SIR TypedWithInferVars
type TypedWithInferVarsModule = SIR.Module TypedWithInferVars
type TypedWithInferVarsADT = SIR.ADT TypedWithInferVars
type TypedWithInferVarsTypeSynonym = SIR.TypeSynonym TypedWithInferVars
type TypedWithInferVarsBinding = SIR.Binding TypedWithInferVars
type TypedWithInferVarsExpr = SIR.Expr TypedWithInferVars
type TypedWithInferVarsPattern = SIR.Pattern TypedWithInferVars
type TypedWithInferVarsVariable = SIR.Variable TypedWithInferVars
type TypedWithInferVarsTypeExpr = SIR.TypeExpr TypedWithInferVars
type TypedWithInferVarsADTArena = Arena.Arena TypedWithInferVarsADT Type.ADTKey
type TypedWithInferVarsTypeSynonymArena = Arena.Arena TypedWithInferVarsTypeSynonym Type.TypeSynonymKey
type TypedWithInferVarsVariableArena = Arena.Arena TypedWithInferVarsVariable SIR.VariableKey
type TypedWithInferVarsModuleArena = Arena.Arena TypedWithInferVarsModule SIR.ModuleKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type AddMonad adts type_synonyms quant_vars vars =
    ReaderT (adts, type_synonyms, quant_vars, vars) (WriterT [TypeSolveTask] (State InferVarArena))

-- TODO: maybe it is a bad idea to have this type synonym here?
type AddedVariableArena = (Arena.Arena (SIR.Variable Added) SIR.VariableKey)

get_var_type :: SIR.VariableKey -> AddMonad adts type_synonyms quant_vars AddedVariableArena TypeWithInferVar.Type
get_var_type var = do
    (_, _, _, vars) <- ask
    let SIR.Variable _ ty _ = Arena.get vars var
    pure ty

-- apply_type :: TypeSolver.InferVarForWhat -> Span -> TypeWithInferVar.Type -> TypeWithInferVar.Type -> AddMonad TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena vars TypeWithInferVar.Type
-- apply_type for_what sp ty arg = do
--     (adts, type_synonyms, quant_vars, _) <- ask
--     get_type_synonym <- get_type_synonym
--     apply_result <- lift $ lift $ TypeSolver.apply_type adts type_synonyms get_type_synonym quant_vars for_what sp ty arg
--     case apply_result of
--         TypeSolver.AppliedResult res -> pure res
--         TypeSolver.AppliedError e -> do
--             _ <- lift $ lift $ lift $ Compiler.tell_error (SolveError e)
--             result_ifv <- new_infer_var for_what
--             pure $ TypeWithInferVar.Type'InferVar result_ifv
--         TypeSolver.Inconclusive ty constraint -> tell [constraint] >> pure ty

new_infer_var :: TypeWithInferVar.InferVarForWhat -> AddMonad adts type_synonyms quant_vars vars TypeWithInferVar.InferVarKey
new_infer_var for_what = state $ \ vars -> let (k, vars') = Arena.put (TypeWithInferVar.InferVar for_what TypeWithInferVar.Fresh) vars in (k, vars')

add_types :: SIR.SIR Unadded -> (SIR.SIR Added, InferVarArena, [TypeSolveTask])
add_types (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    let ((sir, tasks), arenas) =
                runState
                ( runWriterT
                    ( do
                        type_synonyms <- runReaderT (Arena.transformM add_in_type_synonym type_synonyms) ((), (), (), ())
                        variables <- runReaderT (Arena.transformM add_in_variable variables) ((), (), (), ())
                        adts <- runReaderT (Arena.transformM add_in_adt adts) ((), (), (), ())
                        modules <- runReaderT (Arena.transformM add_in_module modules) (todo)

                        case main_function of
                            Just main_function -> runReaderT (add_main_function_constraint main_function) ((), (), (), variables)
                            Nothing ->pure () -- missing main function errir is reported in ToSIR phase

                        pure $ SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)
                    )
                )
                Arena.new
    in (sir, arenas, tasks)

add_main_function_constraint :: SIR.VariableKey -> AddMonad adts type_synonyms quant_vars (Arena.Arena (SIR.Variable Added) SIR.VariableKey) ()
add_main_function_constraint main_function = do
    (_, _, _, vars) <- ask
    let SIR.Variable _ ty (Located var_sp _) = Arena.get vars main_function
    tell [Constraint $ Expect InMainFunction (Located var_sp ty) TypeWithInferVar.Type'String]

add_in_variable :: SIR.Variable Unadded -> AddMonad adts type_synonyms quant_vars vars (SIR.Variable Added)
add_in_variable (SIR.Variable id () name@(Located def_span _)) = SIR.Variable id <$> (TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.Variable def_span)) <*> pure name

add_in_module :: SIR.Module Unadded -> AddMonad TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena AddedVariableArena (SIR.Module Added)
add_in_module (SIR.Module id name_context_key bindings adts type_synonyms) = SIR.Module id name_context_key <$> mapM add_in_binding bindings <*> pure adts <*> pure type_synonyms

add_in_adt :: SIR.ADT Unadded -> AddMonad adts type_synonyms quant_avrs vars (SIR.ADT Added)
add_in_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM add_in_variant variants
    where
        add_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (field, as_type)) -> (id, name,) <$> ((,as_type) <$> do_field field)) fields
        add_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (field, as_type)) -> (id,) <$> ((,as_type) <$> do_field field)) fields

        do_field ty_expr = do
            ty_expr <- add_in_type_expr ty_expr
            ty <- todo {- evaled_as_type -} $ SIR.type_expr_evaled ty_expr
            (adt_arena, type_synonym_arena, quant_var_arena, _) <- ask
            tell [Constraint $ Expect InADTFieldType (Located (SIR.type_expr_span ty_expr) (TypeWithInferVar.kind_of (todo adt_arena) (todo type_synonym_arena) (todo quant_var_arena) ty)) TypeWithInferVar.Type'Kind'Type]
            pure ty_expr

add_in_type_synonym :: SIR.TypeSynonym Unadded -> AddMonad adts type_synonyms quant_vars vars (SIR.TypeSynonym Added)
add_in_type_synonym (Type.TypeSynonym id name (expansion, as_type)) = Type.TypeSynonym id name <$> ((,as_type) <$> add_in_type_expr expansion)

add_in_type_expr :: SIR.TypeExpr Unadded -> AddMonad adts type_synonyms quant_vars vars (SIR.TypeExpr Added)
add_in_type_expr (SIR.TypeExpr'Refer evaled resolved sp name_context iden) = pure (SIR.TypeExpr'Refer evaled resolved sp name_context iden)
add_in_type_expr (SIR.TypeExpr'Get evaled resolved sp parent name) = SIR.TypeExpr'Get evaled resolved sp <$> add_in_type_expr parent <*> pure name
add_in_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp <$> add_in_type_expr a <*> add_in_type_expr b
add_in_type_expr (SIR.TypeExpr'Hole evaled ty sp hid) = pure (SIR.TypeExpr'Hole evaled ty sp hid)
add_in_type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp <$> add_in_type_expr arg <*> add_in_type_expr res
add_in_type_expr (SIR.TypeExpr'Forall evaled sp name_context names t) = SIR.TypeExpr'Forall evaled sp name_context names <$> add_in_type_expr t
add_in_type_expr (SIR.TypeExpr'Apply evaled sp t arg) = SIR.TypeExpr'Apply evaled sp <$> add_in_type_expr t <*> add_in_type_expr arg
add_in_type_expr (SIR.TypeExpr'Wild evaled sp) = pure (SIR.TypeExpr'Wild evaled sp)
add_in_type_expr (SIR.TypeExpr'Poison evaled sp) = pure (SIR.TypeExpr'Poison evaled sp)

add_in_binding :: SIR.Binding Unadded -> AddMonad TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena AddedVariableArena (SIR.Binding Added)
add_in_binding (SIR.Binding p eq_sp e) = do
    p <- add_in_pattern p
    e <- add_in_expr e
    tell [Constraint $ Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]
    pure $ SIR.Binding p eq_sp e

add_in_pattern :: SIR.Pattern Unadded -> AddMonad TypedWithInferVarsADTArena type_synonyms quant_vars AddedVariableArena (SIR.Pattern Added)
add_in_pattern (SIR.Pattern'Variable () sp var) = do
    ty <- get_var_type var
    pure $ SIR.Pattern'Variable ty sp var

add_in_pattern (SIR.Pattern'Wildcard () sp) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.WildcardPattern sp)
    pure $ SIR.Pattern'Wildcard ty sp

add_in_pattern (SIR.Pattern'Tuple () sp l r) = do
    l <- add_in_pattern l
    r <- add_in_pattern r
    pure $ SIR.Pattern'Tuple (TypeWithInferVar.Type'Tuple (SIR.pattern_type l) (SIR.pattern_type r)) sp l r

add_in_pattern (SIR.Pattern'Named () sp at_sp var@(Located var_span var_key) subpat) = do
    subpat <- add_in_pattern subpat
    var_ty <- get_var_type var_key
    tell [Constraint $ Eq InNamedPattern at_sp (Located var_span var_ty) (loc_pat_type subpat)]
    pure $ SIR.Pattern'Named var_ty sp at_sp var subpat

add_in_pattern (SIR.Pattern'AnonADTVariant () sp variant_iden _ fields) =
    mapM add_in_pattern fields >>= \ pattern_fields ->

    -- TODO
    -- ask >>= \ (adts, _, _, _) ->
    -- let SIR.ADT _ _ type_params _ = Arena.get adts adt_key
    --     variant = Type.ADT.get_variant adts variant_index
    -- in

    -- mapM (\ var -> TypeWithInferVar.Type'InferVar <$> new_infer_var $ TypeSolver.ImplicitTyParam sp {- var TODO -}) type_params >>= \ type_param_unks -> -- TODO: declared span

    -- let substitute_adt_params = lift . lift . foldl' (>=>) pure (zipWith TypeSolver.substitute_quant_var type_params type_param_unks)
    --     whole_pat_type = TypeWithInferVar.Type'ADT adt_key type_param_unks

    -- in case variant of
    --      Type.ADT.Variant'Anon _ _ variant_fields ->
    --         mapM (substitute_adt_params . snd . snd) variant_fields >>= \ variant_field_tys_substituted ->
    --         if length pattern_fields /= length variant_field_tys_substituted
    --             then error "wrong number of fields in anonymous variant pattern" -- TODO: report proper error
    --             else
    --                 zipWithM
    --                     (\ pat_field variant_field_ty ->
    --                         lift (tell [Expect InADTVariantPatternField (loc_pat_type pat_field) variant_field_ty]))
    --                     pattern_fields
    --                     variant_field_tys_substituted
    --      Type.ADT.Variant'Named _ _ _ -> error "named variant pattern used with anonymous variant" -- TODO: also report proper error
    --     >>

    add_in_split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'AnonADTVariant todo {- whole_pat_type -} sp variant_iden todo {- type_param_unks -} pattern_fields)

add_in_pattern (SIR.Pattern'NamedADTVariant () sp variant_iden _ fields) = do
    todo
    -- TODO 4 things for if this is resolved:
    --     - check variant is named variant
    --     - check field names are correct
    --     - check all fields are covered
    --     - put type constraints on all fields

    --  this is the logic for ifthe variant iden is not resolved
    fields <- mapM (\ (field_name, field_pat) -> (field_name,) <$> add_in_pattern field_pat) fields
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.UnresolvedADTVariantPattern sp)
    variant_iden <- add_in_split_iden variant_iden
    pure $ SIR.Pattern'NamedADTVariant ty sp variant_iden [] fields

add_in_pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.PoisonPattern sp)) <*> pure sp

add_in_expr :: SIR.Expr Unadded -> AddMonad TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena AddedVariableArena (SIR.Expr Added)
add_in_expr (SIR.Expr'Refer id () sp iden) = do
    let resolved = SIR.split_identifier_resolved iden

    ty <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [DefinedToBeTypeOfValueRef ty resolved]

    iden <- add_in_split_iden iden

    pure (SIR.Expr'Refer id (TypeWithInferVar.Type'InferVar ty) sp iden)

add_in_expr (SIR.Expr'Char id () sp c) = pure (SIR.Expr'Char id TypeWithInferVar.Type'Char sp c)
add_in_expr (SIR.Expr'String id () sp t) = pure (SIR.Expr'String id TypeWithInferVar.Type'String sp t)
add_in_expr (SIR.Expr'Int id () sp i) = pure (SIR.Expr'Int id TypeWithInferVar.Type'Int sp i)
add_in_expr (SIR.Expr'Float id () sp r) = pure (SIR.Expr'Float id TypeWithInferVar.Type'Float sp r)
add_in_expr (SIR.Expr'Bool id () sp b) = pure (SIR.Expr'Bool id TypeWithInferVar.Type'Bool sp b)

add_in_expr (SIR.Expr'Tuple id () sp l r) = add_in_expr l >>= \ l -> add_in_expr r >>= \ r -> pure (SIR.Expr'Tuple id (TypeWithInferVar.Type'Tuple (SIR.expr_type l) (SIR.expr_type r)) sp l r)

add_in_expr (SIR.Expr'Lambda id () sp param body) =
    add_in_pattern param >>= \ param ->
    add_in_expr body >>= \ body ->
    pure (SIR.Expr'Lambda id (TypeWithInferVar.Type'Function (SIR.pattern_type param) (SIR.expr_type body)) sp param body)

add_in_expr (SIR.Expr'Let id () sp name_context_key bindings adts type_synonyms result) =
    mapM add_in_binding bindings >>= \ bindings ->
    add_in_expr result >>= \ result ->
    pure (SIR.Expr'Let id (SIR.expr_type result) sp name_context_key bindings adts type_synonyms result)

add_in_expr (SIR.Expr'LetRec id () sp name_context_key bindings adts type_synonyms result) =
    mapM add_in_binding bindings >>= \ bindings ->
    add_in_expr result >>= \ result ->
    pure (SIR.Expr'LetRec id (SIR.expr_type result) sp name_context_key bindings adts type_synonyms result)

add_in_expr (SIR.Expr'BinaryOps _ void _ _ _ _) = todo

add_in_expr (SIR.Expr'Call id () sp callee arg) = do
    callee <- add_in_expr callee
    arg <- add_in_expr arg

    res_ty_var <- new_infer_var (TypeWithInferVar.CallExpr sp)

    tell [Constraint $ Expect InCallExpr (loc_expr_type callee) (TypeWithInferVar.Type'Function (SIR.expr_type arg) (TypeWithInferVar.Type'InferVar res_ty_var))]

    pure $ SIR.Expr'Call id (TypeWithInferVar.Type'InferVar res_ty_var) sp callee arg

add_in_expr (SIR.Expr'If id () sp if_sp cond true false) = do
    cond <- add_in_expr cond
    true <- add_in_expr true
    false <- add_in_expr false

    tell
        [ Constraint $ Expect InIfCondition (loc_expr_type cond) TypeWithInferVar.Type'Bool
        , Constraint $ Eq InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
        ]

    pure $ SIR.Expr'If id (SIR.expr_type true) sp if_sp cond true false

add_in_expr (SIR.Expr'Match id () sp match_tok_sp testing arms) = do
    testing <- add_in_expr testing
    arms <- mapM (\ (name_context_key, p, e) -> (name_context_key,,) <$> add_in_pattern p <*> add_in_expr e) arms

    -- first expr matches all pattern types
    tell (map (\ (_, arm_pat, _) -> Constraint $ Eq InMatchPatterns match_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms)
    -- all arm result types are the same
    tell (zipWith (\ (_, _, arm_result_1) (_, _, arm_result_2) -> Constraint $ Eq InMatchArms match_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms))

    result_ty <- case headMay arms of
        Just (_, _, first_arm_result) -> pure $ SIR.expr_type first_arm_result
        Nothing -> TypeWithInferVar.Type'InferVar <$> (new_infer_var $ TypeWithInferVar.MatchExpr sp)

    pure $ SIR.Expr'Match id result_ty sp match_tok_sp testing arms

add_in_expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id <$> (TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.PoisonExpr sp)) <*> pure sp
add_in_expr (SIR.Expr'Hole id () sp hid) = SIR.Expr'Hole id <$> (TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.HoleExpr sp)) <*> pure sp <*> pure hid

add_in_expr (SIR.Expr'Forall id () sp name_context_key vars e) =
    add_in_expr e >>= \ e ->
    pure (SIR.Expr'Forall id (TypeWithInferVar.Type'Forall vars (SIR.expr_type e)) sp name_context_key vars e)
add_in_expr (SIR.Expr'TypeApply id () sp e (arg, arg_ty)) = do
    e <- add_in_expr e
    arg <- add_in_type_expr arg

    result_ty <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [ConstraintWhenTypeExprEvaledAsType (todo {- evaled_as_type -} $ SIR.type_expr_evaled arg) (\ arg -> InferVarIsApplyResult sp result_ty (SIR.expr_type e) arg)]

    pure $ SIR.Expr'TypeApply id (TypeWithInferVar.Type'InferVar result_ty) sp e (arg, arg_ty)

add_in_expr (SIR.Expr'TypeAnnotation id () sp (annotation, annotation_ty) e) = do
    annotation <- add_in_type_expr annotation
    e <- add_in_expr e
    tell [ConstraintWhenTypeExprEvaledAsType annotation_ty (\annotation_ty -> Expect InTypeAnnotation (Located (SIR.type_expr_span annotation) (SIR.expr_type e)) annotation_ty)]

    ty <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [DefinedToBeTypeOfTypeExpr ty annotation_ty]

    pure $ SIR.Expr'TypeAnnotation id (TypeWithInferVar.Type'InferVar ty) sp (annotation, annotation_ty) e

add_in_split_iden :: SIR.SplitIdentifier resolved Unadded -> AddMonad TypedWithInferVarsADTArena type_synonyms quant_vars vars (SIR.SplitIdentifier resolved Added)
add_in_split_iden (SIR.SplitIdentifier'Get parent name resolved) = SIR.SplitIdentifier'Get <$> add_in_type_expr parent <*> pure name <*> pure resolved
add_in_split_iden (SIR.SplitIdentifier'Single name_context_key name resolved) = pure $ SIR.SplitIdentifier'Single name_context_key name resolved

-- TODO: deal with these
-- -- TODO: make helper functions to not use lift
--
-- get_bv_type :: SIR.ValueRef -> ContextReader TypedWithInferVarsADTArena type_synonyms quant_vars TypedWithInferVarsVariableArena TypeWithInferVars
-- get_bv_type bv =
--     case bv of
--         SIR.ValueRef'Variable var -> get_var_type var
--         SIR.ValueRef'ADTVariantConstructor variant_index@(Type.ADT.VariantIndex _ adt_key _) -> do
--             (adts, _, _, _) <- ask
--             let (Type.ADT _ _ adt_type_params _) = Arena.get adts adt_key
--             let variant = Type.ADT.get_variant adts variant_index
--             case variant of
--                     Type.ADT.Variant'Named _ _ _ -> error "bound value should not be made for a named adt variant" -- TODO: statically make sure this cant happen?
--                     Type.ADT.Variant'Anon _ _ fields ->
--                         let change_quant_vars ty = foldlM (\ ty (adt_typaram, var_typaram) -> lift $ lift $ TypeSolver.substitute_quant_var adt_typaram (TypeWithInferVar.Type'QuantVar var_typaram) ty) ty (zip adt_type_params adt_type_params)
--                         in mapM (change_quant_vars . snd . snd) fields >>= \ arg_tys ->
--                         let wrap_in_forall = case adt_type_params of -- TODO: duplicate these type params
--                                 [] -> identity
--                                 param:more -> TypeWithInferVar.Type'Forall (param :| more)
--                         in pure $ wrap_in_forall $ foldr TypeWithInferVar.Type'Function (TypeWithInferVar.Type'ADT adt_key (map TypeWithInferVar.Type'QuantVar adt_type_params)) arg_tys -- function type that takes all the field types and then results in the adt type
--         SIR.ValueRef'Intrinsic i -> pure $ TypeSolver.from_ir_type $ Intrinsics.intrinsic_bv_type i
--
--
-- -- TODO: come up with a better way of doing this
-- get_type_synonym :: ContextReader adts (Arena.Arena (Type.TypeSynonym t) Type.TypeSynonymKey) quant_vars vars (Type.TypeSynonymKey -> TypeSolver.SolveMonad (Compiler.WithDiagnostics Error Void) (Type.TypeSynonym t))
-- get_type_synonym = do
--     (_, type_synonyms, _, _) <- ask
--     pure (\ ts_key -> pure $ Arena.get type_synonyms ts_key)
--
--
-- -- TODO: sort constraints by priority so that certain weird things dont happen (sort by depth?)
-- -- for example:
-- -- ```
-- -- use_wrong = thing(0);
-- -- thing = \ (x) -> :string x;
-- -- ```
-- -- produces "
-- --     scratch.uhf:51:7: error: conflicting types in assignment: 'int' vs 'string'
-- --        ╭ scratch.uhf
-- --     50 │ use_wrong = thing(0);
-- --     51 │ thing = \ (x) -> :string x;
-- --        │ ───── ━ ──────────────────
-- --        │     ╰── int -> _         ╰── string -> string
-- --       ═╧══[E0401] type-mismatch
-- -- "
-- -- but it really should produce an error at `thing(0)` saying that thing takes a string and not an int
-- -- (this happens because bindings are processed in order and the constraint from 'thing(0)' is processed before the constraint from 'thing = ...')

-- TODO: see if these are actually necessary
loc_pat_type :: SIR.Pattern stage -> Located (SIR.TypeInfo stage)
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr stage -> Located (SIR.TypeInfo stage)
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)
