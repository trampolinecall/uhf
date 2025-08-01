{-# LANGUAGE DataKinds #-}
module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Prepare (add_types) where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Source.Located (Located (..))
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Task (TypeSolveTask (..), Constraint (..), ExpectInWhat (..), EqInWhat (..))
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import UHF.Data.IR.TypeWithInferVar (InferVarArena)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef)
import Data.Functor.Const (Const)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.Result (TypeInfo (..), empty_type_info)
import qualified Data.Map as Map
import qualified UHF.Data.SIR.ID as SIR.ID

type Unadded = ((), Const () (), (), (), (), (), ())
type Added = ((), Const () (), (), (), (), (), ())

-- TODO: remove these
type TypedWithInferVarsDIden = Maybe (DeclRef TypeWithInferVar.Type)
type TypedWithInferVars = (NameMaps.NameContextKey, Maybe (), DeclRef TypeWithInferVar.Type, TypeWithInferVar.Type, TypeWithInferVar.Type, Void)
type TypedWithInferVarsSIR = SIR.SIR
type TypedWithInferVarsModule = SIR.Module
type TypedWithInferVarsADT = SIR.ADT
type TypedWithInferVarsTypeSynonym = SIR.TypeSynonym
type TypedWithInferVarsBinding = SIR.Binding
type TypedWithInferVarsExpr = SIR.Expr
type TypedWithInferVarsPattern = SIR.Pattern
type TypedWithInferVarsVariable = SIR.Variable
type TypedWithInferVarsTypeExpr = SIR.TypeExpr
type TypedWithInferVarsADTArena = Arena.Arena TypedWithInferVarsADT Type.ADTKey
type TypedWithInferVarsTypeSynonymArena = Arena.Arena TypedWithInferVarsTypeSynonym Type.TypeSynonymKey
type TypedWithInferVarsVariableArena = Arena.Arena TypedWithInferVarsVariable SIR.VariableKey
type TypedWithInferVarsModuleArena = Arena.Arena TypedWithInferVarsModule SIR.ModuleKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

-- TODO: refactor this monad (pattern assigning reads vars and adt assigning reads adts, type_synonyms, and quant_vars)
type AddMonad adts type_synonyms quant_vars vars =
    ReaderT (adts, type_synonyms, quant_vars, vars) (WriterT [TypeSolveTask] (State (TypeInfo, InferVarArena)))

-- TODO: maybe it is a bad idea to have this type synonym here?
-- TODO: rename this type synonym
type AddedVariableArena = (Arena.Arena SIR.Variable SIR.VariableKey)

put_var_type :: SIR.ID.ID "Variable" -> TypeWithInferVar.Type -> AddMonad adts type_synonyms quant_vars vars ()
put_var_type id t = state $ \ (typeinfo, infer_vars) -> ((), (typeinfo { variable_types = Map.insert id t (variable_types typeinfo) }, infer_vars))

put_expr_type :: SIR.ID.ID "Expr" -> TypeWithInferVar.Type -> AddMonad adts type_synonyms quant_vars vars ()
put_expr_type id t = state $ \ (typeinfo, infer_vars) -> ((), (typeinfo { expr_types = Map.insert id t (expr_types typeinfo) }, infer_vars))

put_pattern_type :: SIR.ID.ID "Pattern" -> TypeWithInferVar.Type -> AddMonad adts type_synonyms quant_vars vars ()
put_pattern_type id t = state $ \ (typeinfo, infer_vars) -> ((), (typeinfo { pattern_types = Map.insert id t (pattern_types typeinfo) }, infer_vars))

get_var_type :: SIR.VariableKey -> AddMonad adts type_synonyms quant_vars AddedVariableArena TypeWithInferVar.Type
get_var_type var = do
    (_, _, _, vars) <- ask
    let SIR.Variable id _ _ = Arena.get vars var
    (TypeInfo { variable_types = variable_types }, _) <- get
    pure $ variable_types Map.! id

new_infer_var :: TypeWithInferVar.InferVarForWhat -> AddMonad adts type_synonyms quant_vars vars TypeWithInferVar.InferVarKey
new_infer_var for_what =
    state $ \ (typeinfo, vars) -> let (k, vars') = Arena.put (TypeWithInferVar.InferVar for_what TypeWithInferVar.Fresh) vars in (k, (typeinfo, vars'))

-- TODO: this does not modify the sir so do not return it
add_types :: SIR.SIR -> (SIR.SIR, (TypeInfo, InferVarArena), [TypeSolveTask])
add_types (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    let ((sir, tasks), solving_state) =
                runState
                ( runWriterT
                    ( do
                        type_synonyms <- runReaderT (Arena.transformM add_in_type_synonym type_synonyms) ((), (), (), ())
                        variables <- runReaderT (Arena.transformM add_in_variable variables) ((), (), (), ())
                        adts <- runReaderT (Arena.transformM add_in_adt adts) (adts, type_synonyms, type_vars, ())
                        modules <- runReaderT (Arena.transformM add_in_module modules) ((), (), (), variables)

                        case main_function of
                            Just main_function -> runReaderT (add_main_function_constraint main_function) ((), (), (), variables)
                            Nothing ->pure () -- missing main function errir is reported in ToSIR phase

                        pure $ SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)
                    )
                )
                (empty_type_info, Arena.new)
    in (sir, solving_state, tasks)

add_main_function_constraint :: SIR.VariableKey -> AddMonad adts type_synonyms quant_vars AddedVariableArena ()
add_main_function_constraint main_function = do
    (_, _, _, vars) <- ask
    let SIR.Variable _ _ (Located var_sp _) = Arena.get vars main_function
    var_ty <- get_var_type main_function
    tell [Constraint $ Expect InMainFunction (Located var_sp var_ty) TypeWithInferVar.Type'String]

add_in_variable :: SIR.Variable -> AddMonad adts type_synonyms quant_vars vars SIR.Variable
-- TODO: rename vid to id
add_in_variable (SIR.Variable id mid name@(Located def_span _)) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.Variable def_span)
    put_var_type id ty
    pure $ SIR.Variable id mid name

add_in_module :: SIR.Module -> AddMonad adts type_synonyms quant_vars AddedVariableArena SIR.Module
-- TODO: rename mid to id
add_in_module (SIR.Module mid id name_context_key bindings adts type_synonyms) = SIR.Module mid id name_context_key <$> mapM add_in_binding bindings <*> pure adts <*> pure type_synonyms

-- TODO: figure this out
add_in_adt :: SIR.ADT -> AddMonad (Arena.Arena SIR.ADT Type.ADTKey) (Arena.Arena SIR.TypeSynonym Type.TypeSynonymKey) QuantVarArena vars SIR.ADT
add_in_adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM add_in_variant variants
    where
        add_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (field, as_type)) -> (id, name,) <$> ((,as_type) <$> do_field field)) fields
        add_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (field, as_type)) -> (id,) <$> ((,as_type) <$> do_field field)) fields

        do_field ty_expr = do
            ty_expr <- add_in_type_expr ty_expr
            let ty = SIR.type_expr_evaled ty_expr
            (adt_arena, type_synonym_arena, quant_var_arena, _) <- ask
            tell [WhenTypeExprEvaled ty $ \ty -> EvalAsType (SIR.type_expr_span ty_expr) ty $ \ty -> Constraint $ Expect InADTFieldType (Located (SIR.type_expr_span ty_expr) (TypeWithInferVar.kind_of adt_arena (todo type_synonym_arena) quant_var_arena ty)) TypeWithInferVar.Type'Kind'Type]
            pure ty_expr

add_in_type_synonym :: SIR.TypeSynonym -> AddMonad adts type_synonyms quant_vars vars SIR.TypeSynonym
add_in_type_synonym (Type.TypeSynonym id name (expansion, as_type)) = Type.TypeSynonym id name <$> ((,as_type) <$> add_in_type_expr expansion)

add_in_type_expr :: SIR.TypeExpr -> AddMonad adts type_synonyms quant_vars vars SIR.TypeExpr
add_in_type_expr (SIR.TypeExpr'Refer id nrid sp name_context iden) = pure (SIR.TypeExpr'Refer id nrid sp name_context iden)
add_in_type_expr (SIR.TypeExpr'Get id nrid sp parent name) = SIR.TypeExpr'Get id nrid sp <$> add_in_type_expr parent <*> pure name
add_in_type_expr (SIR.TypeExpr'Tuple id sp a b) = SIR.TypeExpr'Tuple id sp <$> add_in_type_expr a <*> add_in_type_expr b
add_in_type_expr (SIR.TypeExpr'Hole id hid sp hiden) = pure (SIR.TypeExpr'Hole id hid sp hiden)
add_in_type_expr (SIR.TypeExpr'Function id sp arg res) = SIR.TypeExpr'Function id sp <$> add_in_type_expr arg <*> add_in_type_expr res
add_in_type_expr (SIR.TypeExpr'Forall id sp name_context names t) = SIR.TypeExpr'Forall id sp name_context names <$> add_in_type_expr t
add_in_type_expr (SIR.TypeExpr'Apply id sp t arg) = SIR.TypeExpr'Apply id sp <$> add_in_type_expr t <*> add_in_type_expr arg
add_in_type_expr (SIR.TypeExpr'Wild id sp) = pure (SIR.TypeExpr'Wild id sp)
add_in_type_expr (SIR.TypeExpr'Poison id sp) = pure (SIR.TypeExpr'Poison id sp)

add_in_binding :: SIR.Binding -> AddMonad adts type_synonyms quant_vars AddedVariableArena SIR.Binding
add_in_binding (SIR.Binding id p eq_sp e) = do
    (p, p_ty) <- add_in_pattern p
    (e, e_ty) <- add_in_expr e
    tell [Constraint $ Eq InAssignment eq_sp (Located (SIR.pattern_span p) p_ty) (Located (SIR.expr_span e) e_ty)]
    pure $ SIR.Binding id p eq_sp e

add_in_pattern :: SIR.Pattern -> AddMonad adts type_synonyms quant_vars AddedVariableArena (SIR.Pattern, TypeWithInferVar.Type)
add_in_pattern (SIR.Pattern'Variable id sp var) = do
    ty <- get_var_type var
    put_pattern_type id ty
    pure (SIR.Pattern'Variable id sp var, ty)

add_in_pattern (SIR.Pattern'Wildcard id sp) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.WildcardPattern sp)
    put_pattern_type id ty
    pure $ (SIR.Pattern'Wildcard id sp, ty)

add_in_pattern (SIR.Pattern'Tuple id sp l r) = do
    (l, l_ty) <- add_in_pattern l
    (r, r_ty) <- add_in_pattern r
    put_pattern_type id (TypeWithInferVar.Type'Tuple l_ty r_ty)
    pure $ (SIR.Pattern'Tuple id sp l r, TypeWithInferVar.Type'Tuple l_ty r_ty)

add_in_pattern (SIR.Pattern'Named id sp at_sp var@(Located var_span var_key) subpat) = do
    (subpat, subpat_ty) <- add_in_pattern subpat
    var_ty <- get_var_type var_key
    tell [Constraint $ Eq InNamedPattern at_sp (Located var_span var_ty) (Located (SIR.pattern_span subpat) subpat_ty)]
    put_pattern_type id var_ty
    pure $ (SIR.Pattern'Named id sp at_sp var subpat, var_ty)

add_in_pattern (SIR.Pattern'AnonADTVariant id variant_id sp variant_iden fields) =
    unzip <$> mapM add_in_pattern fields >>= \ (pattern_fields, pattern_field_types) ->

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
    -- TODO: put type of whole pattern
    -- TODO: dont forget to put inferred quant var applications

    add_in_split_iden variant_iden >>= \ variant_iden ->
    put_pattern_type id todo >>
    pure (SIR.Pattern'AnonADTVariant id variant_id sp variant_iden pattern_fields, todo)

add_in_pattern (SIR.Pattern'NamedADTVariant id variant_id sp variant_iden fields) = do
    todo
    -- TODO 4 things for if this is resolved:
    --     - check variant is named variant
    --     - check field names are correct
    --     - check all fields are covered
    --     - put type constraints on all fields
    -- TODO: put type of whole pattern
    -- TODO: dont forget to put inferred quant var applications

    --  this is the logic for ifthe variant iden is not resolved
    (fields, field_tys) <- unzip <$> mapM (\ (field_name, field_pat) -> add_in_pattern field_pat >>= \ (pat, pat_ty) -> pure ((field_name, pat), pat_ty)) fields
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.UnresolvedADTVariantPattern sp)
    variant_iden <- add_in_split_iden variant_iden
    put_pattern_type id todo
    pure (SIR.Pattern'NamedADTVariant id variant_id sp variant_iden fields, todo)

add_in_pattern (SIR.Pattern'Poison id sp) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.PoisonPattern sp)
    put_pattern_type id ty
    pure (SIR.Pattern'Poison id sp, ty)

add_in_expr :: SIR.Expr -> AddMonad adts type_synonyms quant_vars AddedVariableArena (SIR.Expr, TypeWithInferVar.Type)
-- TODO: rename eid to id
add_in_expr (SIR.Expr'Refer eid id sp iden) = do
    let iden_id = SIR.split_identifier_id iden

    ifv <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [WhenValueRefResolved iden_id $ \resolved -> GetValueRefType resolved $ \ty -> Constraint $ DefinedToBe InVariable sp ifv ty]

    iden <- add_in_split_iden iden

    put_expr_type eid (TypeWithInferVar.Type'InferVar ifv)
    pure (SIR.Expr'Refer eid id sp iden, TypeWithInferVar.Type'InferVar ifv)

add_in_expr (SIR.Expr'Char eid id sp c) = do
    put_expr_type eid TypeWithInferVar.Type'Char
    pure (SIR.Expr'Char eid id sp c, TypeWithInferVar.Type'Char)
add_in_expr (SIR.Expr'String eid id sp t) = do
    put_expr_type eid TypeWithInferVar.Type'String
    pure (SIR.Expr'String eid id sp t, TypeWithInferVar.Type'String)
add_in_expr (SIR.Expr'Int eid id sp i) = do
    put_expr_type eid TypeWithInferVar.Type'Int
    pure (SIR.Expr'Int eid id sp i, TypeWithInferVar.Type'Int)
add_in_expr (SIR.Expr'Float eid id sp r) = do
    put_expr_type eid TypeWithInferVar.Type'Float
    pure (SIR.Expr'Float eid id sp r, TypeWithInferVar.Type'Float)
add_in_expr (SIR.Expr'Bool eid id sp b) = do
    put_expr_type eid TypeWithInferVar.Type'Bool
    pure (SIR.Expr'Bool eid id sp b, TypeWithInferVar.Type'Bool)

add_in_expr (SIR.Expr'Tuple eid id sp l r) = do
    (l, l_ty) <- add_in_expr l
    (r, r_ty) <- add_in_expr r
    put_expr_type eid (TypeWithInferVar.Type'Tuple l_ty r_ty)
    pure (SIR.Expr'Tuple eid id sp l r, TypeWithInferVar.Type'Tuple l_ty r_ty)

add_in_expr (SIR.Expr'Lambda eid id sp param body) =
    add_in_pattern param >>= \ (param, param_ty) ->
    add_in_expr body >>= \ (body, body_ty) ->
    put_expr_type eid (TypeWithInferVar.Type'Function param_ty body_ty) >>
    pure (SIR.Expr'Lambda eid id sp param body, TypeWithInferVar.Type'Function param_ty body_ty)

add_in_expr (SIR.Expr'Let eid id sp name_context_key bindings adts type_synonyms result) =
    mapM add_in_binding bindings >>= \ bindings ->
    add_in_expr result >>= \ (result, result_ty) ->
    put_expr_type eid result_ty >>
    pure (SIR.Expr'Let eid id sp name_context_key bindings adts type_synonyms result, result_ty)

add_in_expr (SIR.Expr'LetRec eid id sp name_context_key bindings adts type_synonyms result) =
    mapM add_in_binding bindings >>= \ bindings ->
    add_in_expr result >>= \ (result, result_ty) ->
    put_expr_type eid result_ty >>
    pure (SIR.Expr'LetRec eid id sp name_context_key bindings adts type_synonyms result, result_ty)

add_in_expr (SIR.Expr'BinaryOps _ void _ _ _ _) = todo

add_in_expr (SIR.Expr'Call eid id sp callee arg) = do
    (callee, callee_ty) <- add_in_expr callee
    (arg, arg_ty) <- add_in_expr arg

    res_ty_var <- new_infer_var (TypeWithInferVar.CallExpr sp)

    tell [Constraint $ Expect InCallExpr (Located (SIR.expr_span callee) callee_ty) (TypeWithInferVar.Type'Function arg_ty (TypeWithInferVar.Type'InferVar res_ty_var))]

    put_expr_type eid (TypeWithInferVar.Type'InferVar res_ty_var)
    pure (SIR.Expr'Call eid id sp callee arg, TypeWithInferVar.Type'InferVar res_ty_var)

add_in_expr (SIR.Expr'If eid id sp if_sp cond true false) = do
    (cond, cond_ty) <- add_in_expr cond
    (true, true_ty) <- add_in_expr true
    (false, false_ty) <- add_in_expr false

    tell
        [ Constraint $ Expect InIfCondition (Located (SIR.expr_span cond) cond_ty) TypeWithInferVar.Type'Bool
        , Constraint $ Eq InIfBranches if_sp (Located (SIR.expr_span true) true_ty) (Located (SIR.expr_span false) false_ty)
        ]

    put_expr_type eid true_ty
    pure (SIR.Expr'If eid id sp if_sp cond true false, true_ty)

add_in_expr (SIR.Expr'Match eid id sp match_tok_sp testing arms) = do
    (testing, testing_ty) <- add_in_expr testing
    arms <- mapM (\ (name_context_key, p, e) -> (name_context_key,,) <$> add_in_pattern p <*> add_in_expr e) arms

    -- first expr matches all pattern types
    tell (map (\ (_, (arm_pat, arm_pat_ty), _) -> Constraint $ Eq InMatchPatterns match_tok_sp (Located (SIR.pattern_span arm_pat) arm_pat_ty) (Located (SIR.expr_span testing) testing_ty)) arms)
    -- all arm result types are the same
    tell (zipWith (\ (_, _, (arm_result_1, arm_result_ty_1)) (_, _, (arm_result_2, arm_result_ty_2)) -> Constraint $ Eq InMatchArms match_tok_sp (Located (SIR.expr_span arm_result_1) arm_result_ty_1) (Located (SIR.expr_span arm_result_2) arm_result_ty_2)) arms (drop 1 arms))

    result_ty <- case headMay arms of
        Just (_, _, (_, first_arm_ty)) -> pure first_arm_ty
        Nothing -> TypeWithInferVar.Type'InferVar <$> (new_infer_var $ TypeWithInferVar.MatchExpr sp)

    put_expr_type eid result_ty
    pure (SIR.Expr'Match eid id sp match_tok_sp testing (map (\ (nc, p, e) -> (nc, fst p, fst e)) arms), result_ty)

add_in_expr (SIR.Expr'Poison eid id sp) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.PoisonExpr sp)
    put_expr_type eid ty
    pure (SIR.Expr'Poison eid id sp, ty)
add_in_expr (SIR.Expr'Hole eid id sp hid) = do
    ty <- TypeWithInferVar.Type'InferVar <$> new_infer_var (TypeWithInferVar.HoleExpr sp)
    put_expr_type eid ty
    pure (SIR.Expr'Hole eid id sp hid, ty)

add_in_expr (SIR.Expr'Forall eid id sp name_context_key vars e) =
    add_in_expr e >>= \ (e, e_ty) ->
    put_expr_type eid (TypeWithInferVar.Type'Forall vars e_ty) >>
    pure (SIR.Expr'Forall eid id  sp name_context_key vars e, TypeWithInferVar.Type'Forall vars e_ty)
add_in_expr (SIR.Expr'TypeApply eid id sp e (arg, arg_ty)) = do
    (e, e_ty) <- add_in_expr e
    arg' <- add_in_type_expr arg

    result_ty <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [WhenTypeExprEvaled (SIR.type_expr_evaled arg') $ \ arg_evaled -> EvalAsType (SIR.type_expr_span arg) arg_evaled $ \ arg_as_type -> Constraint $ InferVarIsApplyResult sp result_ty e_ty arg_as_type]

    put_expr_type eid (TypeWithInferVar.Type'InferVar result_ty)
    pure $ (SIR.Expr'TypeApply eid id sp e (arg', arg_ty), TypeWithInferVar.Type'InferVar result_ty)

add_in_expr (SIR.Expr'TypeAnnotation eid id sp (annotation, annotation_ty) e) = do
    annotation <- add_in_type_expr annotation
    (e, e_ty) <- add_in_expr e
    tell [WhenTypeExprEvaledAsType annotation_ty $ \annotation_ty -> Constraint $ Expect InTypeAnnotation (Located (SIR.type_expr_span annotation) e_ty) annotation_ty]

    ifv <- new_infer_var (TypeWithInferVar.IdenExpr sp)
    tell [WhenTypeExprEvaledAsType annotation_ty $ \ annotation_ty -> Constraint $ DefinedToBe InTypeAnnotation sp ifv annotation_ty]

    put_expr_type eid (TypeWithInferVar.Type'InferVar ifv)
    pure (SIR.Expr'TypeAnnotation eid id sp (annotation, annotation_ty) e, TypeWithInferVar.Type'InferVar ifv)

add_in_split_iden :: SIR.SplitIdentifier id_name -> AddMonad typedWithInferVarsADTArena type_synonyms quant_vars vars (SIR.SplitIdentifier id_name)
add_in_split_iden (SIR.SplitIdentifier'Get id parent name) = SIR.SplitIdentifier'Get id <$> add_in_type_expr parent <*> pure name
add_in_split_iden (SIR.SplitIdentifier'Single id name_context_key name) = pure $ SIR.SplitIdentifier'Single id name_context_key name

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
