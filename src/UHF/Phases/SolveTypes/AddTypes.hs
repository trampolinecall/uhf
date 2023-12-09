module UHF.Phases.SolveTypes.AddTypes (add) where

import UHF.Prelude

import Control.Monad ((>=>))
import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Error
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Util.Arena as Arena

type ContextReader adts type_synonyms quant_vars vars = ReaderT (adts, type_synonyms, quant_vars, vars) (WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad (Compiler.WithDiagnostics Error Void)))

-- TODO: make helper functions to not use lift

get_var_type :: SIR.VariableKey -> ContextReader adts type_synonyms quant_vars TypedWithInferVarsVariableArena TypeWithInferVars
get_var_type var = do
    (_, _, _, vars) <- ask
    case Arena.get vars var of
        SIR.Variable _ ty _ -> pure ty
        SIR.Variable'ADTVariant _ _ _ ty _ -> pure ty

apply_type :: TypeSolver.InferVarForWhat -> Span -> TypeSolver.Type -> TypeSolver.Type -> ContextReader TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena vars TypeSolver.Type
apply_type for_what sp ty arg = do
    (adts, type_synonyms, quant_vars, _) <- ask
    lift $ lift $ TypeSolver.apply_type adts type_synonyms quant_vars for_what sp ty arg

-- TODO: sort constraints by priority so that certain weird things dont happen (sort by depth?)
-- for example:
-- ```
-- use_wrong = thing(0);
-- thing = \ (x) -> :string x;
-- ```
-- produces "
--     scratch.uhf:51:7: error: conflicting types in assignment: 'int' vs 'string'
--        ╭ scratch.uhf
--     50 │ use_wrong = thing(0);
--     51 │ thing = \ (x) -> :string x;
--        │ ───── ━ ──────────────────
--        │     ╰── int -> _         ╰── string -> string
--       ═╧══[E0401] type-mismatch
-- "
-- but it really should produce an error at `thing(0)` saying that thing takes a string and not an int
-- (this happens because bindings are processed in order and the constraint from 'thing(0)' is processed before the constraint from 'thing = ...')

add :: UntypedModuleArena -> UntypedADTArena -> UntypedTypeSynonymArena -> QuantVarArena -> UntypedVariableArena -> WriterT [TypeSolver.Constraint] (TypeSolver.SolveMonad (Compiler.WithDiagnostics Error Void)) (TypedWithInferVarsModuleArena, TypedWithInferVarsADTArena, TypedWithInferVarsTypeSynonymArena, TypedWithInferVarsVariableArena)
add mods adts type_synonyms quant_vars variables =
    runReaderT (
        Arena.transformM adt adts >>= \ adts ->
        Arena.transformM type_synonym  type_synonyms >>= \ type_synonyms ->
        pure (adts, type_synonyms)
    ) ((), (), (), ()) >>= \ (adts, type_synonyms) ->
    runReaderT (
        Arena.transformM variable variables
    ) (adts, (), (), ()) >>= \ variables ->
    runReaderT (
        Arena.transformM module_ mods >>= \ mods ->
        pure (mods, adts, type_synonyms, variables)
    ) (adts, type_synonyms, quant_vars, variables)

variable :: UntypedVariable -> ContextReader TypedWithInferVarsADTArena type_synonyms quant_vars vars TypedWithInferVarsVariable
variable (SIR.Variable id () name@(Located def_span _)) = SIR.Variable id <$> lift (lift $ TypeSolver.Type'InferVar <$> TypeSolver.new_infer_var (TypeSolver.Variable def_span)) <*> pure name
variable (SIR.Variable'ADTVariant id variant_index@(Type.ADT.VariantIndex adt_key _) var_type_params () def_span) = do
    (adts, _, _, _) <- ask
    let (Type.ADT _ _ adt_type_params _) = Arena.get adts adt_key
    let variant = Type.ADT.get_variant adts variant_index
    ty <- case variant of
            Type.ADT.Variant'Named _ _ _ -> error "bound value should not be made for a named adt variant" -- TODO: statically make sure this cant happen?
            Type.ADT.Variant'Anon _ _ fields ->
                let change_quant_vars ty = foldlM (\ ty (adt_typaram, var_typaram) -> lift $ lift $ TypeSolver.substitute_quant_var adt_typaram (TypeSolver.Type'QuantVar var_typaram) ty) ty (zip adt_type_params var_type_params)
                in mapM (change_quant_vars . snd . snd) fields >>= \ arg_tys ->
                let wrap_in_forall = case var_type_params of
                        [] -> identity
                        param:more -> TypeSolver.Type'Forall (param :| more)
                in pure $ wrap_in_forall $ foldr TypeSolver.Type'Function (TypeSolver.Type'ADT adt_key (map TypeSolver.Type'QuantVar var_type_params)) arg_tys -- function type that takes all the field types and then results in the adt type
    pure $ SIR.Variable'ADTVariant id variant_index var_type_params ty def_span

module_ :: UntypedModule -> ContextReader TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena TypedWithInferVarsVariableArena TypedWithInferVarsModule
module_ (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM binding bindings <*> pure adts <*> pure type_synonyms

adt :: UntypedADT -> ContextReader adts type_synonyms quant_vars vars TypedWithInferVarsADT
adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM convert_variant variants
    where
        convert_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty_expr, ty)) -> type_expr ty_expr >>= \ ty_expr -> pure (id, name, (ty_expr, ty))) fields
        convert_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty_expr, ty)) -> type_expr ty_expr >>= \ ty_expr -> pure (id, (ty_expr, ty))) fields

type_synonym :: UntypedTypeSynonym -> ContextReader adts type_synonyms quant_vars vars TypedWithInferVarsTypeSynonym
type_synonym (Type.TypeSynonym id name (expansion, exp_as_type)) = type_expr expansion >>= \ expansion -> pure (Type.TypeSynonym id name (expansion, exp_as_type))

type_expr :: UntypedTypeExpr -> ContextReader adts type_synonyms quant_vars vars TypedWithInferVarsTypeExpr
type_expr (SIR.TypeExpr'Refer evaled sp iden) = pure (SIR.TypeExpr'Refer evaled sp iden)
type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp <$> type_expr parent <*> pure name
type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp <$> type_expr a <*> type_expr b
type_expr (SIR.TypeExpr'Hole evaled ty sp hid) = pure (SIR.TypeExpr'Hole evaled ty sp hid)
type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp <$> type_expr arg <*> type_expr res
type_expr (SIR.TypeExpr'Forall evaled sp names t) = SIR.TypeExpr'Forall evaled sp names <$> type_expr t
type_expr (SIR.TypeExpr'Apply evaled sp t arg) = SIR.TypeExpr'Apply evaled sp <$> type_expr t <*> type_expr arg
type_expr (SIR.TypeExpr'Wild evaled sp) = pure (SIR.TypeExpr'Wild evaled sp)
type_expr (SIR.TypeExpr'Poison evaled sp) = pure (SIR.TypeExpr'Poison evaled sp)

binding :: UntypedBinding -> ContextReader TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena TypedWithInferVarsVariableArena TypedWithInferVarsBinding
binding (SIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [TypeSolver.Eq TypeSolver.InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (SIR.Binding p eq_sp e)
binding (SIR.Binding'ADTVariant sp var_key vars variant) = pure $ SIR.Binding'ADTVariant sp var_key vars variant

loc_pat_type :: SIR.Pattern stage -> Located (SIR.TypeInfo stage)
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr stage -> Located (SIR.TypeInfo stage)
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)

pattern :: UntypedPattern -> ContextReader TypedWithInferVarsADTArena type_synonyms quant_vars TypedWithInferVarsVariableArena TypedWithInferVarsPattern
pattern (SIR.Pattern'Identifier () sp var) =
    get_var_type var >>= \ ty ->
    pure (SIR.Pattern'Identifier ty sp var)

pattern (SIR.Pattern'Wildcard () sp) =
    TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var (TypeSolver.WildcardPattern sp)) >>= \ ty ->
    pure (SIR.Pattern'Wildcard ty sp)

pattern (SIR.Pattern'Tuple () sp l r) =
    pattern l >>= \ l ->
    pattern r >>= \ r ->
    pure (SIR.Pattern'Tuple (TypeSolver.Type'Tuple (SIR.pattern_type l) (SIR.pattern_type r)) sp l r)

pattern (SIR.Pattern'Named () sp at_sp var_key subpat) =
    pattern subpat >>= \ subpat ->
    get_var_type (unlocate var_key) >>= \ var_ty ->
    lift (tell [TypeSolver.Eq TypeSolver.InNamedPattern at_sp (Located (just_span var_key) var_ty) (loc_pat_type subpat)]) >>
    pure (SIR.Pattern'Named var_ty sp at_sp var_key subpat)

pattern (SIR.Pattern'AnonADTVariant () sp variant_iden Nothing _ fields) =
    mapM pattern fields >>= \ fields ->
    TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var (TypeSolver.UnresolvedADTVariantPattern sp)) >>= \ ty ->
    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'AnonADTVariant ty sp variant_iden Nothing [] fields)
pattern (SIR.Pattern'AnonADTVariant () sp variant_iden (Just variant_index@(Type.ADT.VariantIndex adt_key _)) _ fields) =
    mapM pattern fields >>= \ pattern_fields ->

    ask >>= \ (adts, _, _, _) ->
    let Type.ADT _ _ type_params _ = Arena.get adts adt_key
        variant = Type.ADT.get_variant adts variant_index
    in

    mapM (\ var -> TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var $ TypeSolver.ImplicitTyParam sp {- var TODO -})) type_params >>= \ type_param_unks -> -- TODO: declared span

    let substitute_adt_params = lift . lift . foldl' (>=>) pure (zipWith (TypeSolver.substitute_quant_var) type_params type_param_unks)
        whole_pat_type = TypeSolver.Type'ADT adt_key type_param_unks

    in case variant of
         Type.ADT.Variant'Anon _ _ variant_fields ->
            mapM (substitute_adt_params . snd . snd) variant_fields >>= \ variant_field_tys_substituted ->
            if length pattern_fields /= length variant_field_tys_substituted
                then error "wrong number of fields in anonymous variant pattern" -- TODO: report proper error
                else
                    zipWithM
                        (\ pat_field variant_field_ty ->
                            lift (tell [TypeSolver.Expect TypeSolver.InADTVariantPatternField (loc_pat_type pat_field) variant_field_ty]))
                        pattern_fields
                        variant_field_tys_substituted
         Type.ADT.Variant'Named _ _ _ -> error "named variant pattern used with anonymous variant" -- TODO: also report proper error
        >>

    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'AnonADTVariant whole_pat_type sp variant_iden (Just variant_index) type_param_unks pattern_fields)

pattern (SIR.Pattern'NamedADTVariant () sp variant_iden Nothing _ fields) =
    mapM (\ (field_name, field_pat) -> (field_name,) <$> pattern field_pat) fields >>= \ fields ->
    TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var (TypeSolver.UnresolvedADTVariantPattern sp)) >>= \ ty ->
    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'NamedADTVariant ty sp variant_iden Nothing [] fields)
pattern (SIR.Pattern'NamedADTVariant () _ _ (Just _) _ _) = todo
-- 4 things:
--     - check variant is named variant
--     - check field names are correct
--     - check all fields are covered
--     - put type constraints on all fields

pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var $ TypeSolver.PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> ContextReader TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena TypedWithInferVarsVariableArena TypedWithInferVarsExpr
expr (SIR.Expr'Identifier id () sp iden var) =
    (case var of
        Just var -> get_var_type var
        Nothing -> TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var (TypeSolver.UnresolvedIdenExpr sp))) >>= \ ty ->

    split_iden iden >>= \ iden ->

    pure (SIR.Expr'Identifier id ty sp iden var)

expr (SIR.Expr'Char id () sp c) = pure (SIR.Expr'Char id TypeSolver.Type'Char sp c)
expr (SIR.Expr'String id () sp t) = pure (SIR.Expr'String id TypeSolver.Type'String sp t)
expr (SIR.Expr'Int id () sp i) = pure (SIR.Expr'Int id TypeSolver.Type'Int sp i)
expr (SIR.Expr'Float id () sp r) = pure (SIR.Expr'Float id TypeSolver.Type'Float sp r)
expr (SIR.Expr'Bool id () sp b) = pure (SIR.Expr'Bool id TypeSolver.Type'Bool sp b)

expr (SIR.Expr'Tuple id () sp l r) = expr l >>= \ l -> expr r >>= \ r -> pure (SIR.Expr'Tuple id (TypeSolver.Type'Tuple (SIR.expr_type l) (SIR.expr_type r)) sp l r)

expr (SIR.Expr'Lambda id () sp param body) =
    pattern param >>= \ param ->
    expr body >>= \ body ->
    pure (SIR.Expr'Lambda id (TypeSolver.Type'Function (SIR.pattern_type param) (SIR.expr_type body)) sp param body)

expr (SIR.Expr'Let id () sp bindings result) =
    mapM binding bindings >>= \ bindings ->
    expr result >>= \ result ->
    pure (SIR.Expr'Let id (SIR.expr_type result) sp bindings result)

expr (SIR.Expr'LetRec id () sp bindings result) =
    mapM binding bindings >>= \ bindings ->
    expr result >>= \ result ->
    pure (SIR.Expr'LetRec id (SIR.expr_type result) sp bindings result)

expr (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void

expr (SIR.Expr'Call id () sp callee arg) =
    expr callee >>= \ callee ->
    expr arg >>= \ arg ->
    lift (lift $ TypeSolver.new_infer_var (TypeSolver.CallExpr sp)) >>= \ res_ty_var ->

    lift (tell [TypeSolver.Expect TypeSolver.InCallExpr (loc_expr_type callee) (TypeSolver.Type'Function (SIR.expr_type arg) (TypeSolver.Type'InferVar res_ty_var))]) >>

    pure (SIR.Expr'Call id (TypeSolver.Type'InferVar res_ty_var) sp callee arg)

expr (SIR.Expr'If id () sp if_sp cond true false) =
    expr cond >>= \ cond ->
    expr true >>= \ true ->
    expr false >>= \ false ->

    lift (tell
        [ TypeSolver.Expect TypeSolver.InIfCondition (loc_expr_type cond) TypeSolver.Type'Bool
        , TypeSolver.Eq TypeSolver.InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
        ]) >>

    pure (SIR.Expr'If id (SIR.expr_type true) sp if_sp cond true false)

expr (SIR.Expr'Match id () sp match_tok_sp testing arms) =
    expr testing >>= \ testing ->
    mapM (\ (p, e) -> (,) <$> pattern p <*> expr e) arms >>= \ arms ->

    -- first expr matches all pattern types
    lift (tell (map (\ (arm_pat, _) -> TypeSolver.Eq TypeSolver.InMatchPatterns match_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms)) >>
    -- all arm types are the same
    lift (tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> TypeSolver.Eq TypeSolver.InMatchArms match_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms))) >>

    (case headMay arms of
        Just (_, first_arm_result) -> pure $ SIR.expr_type first_arm_result
        Nothing -> TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var $ TypeSolver.MatchExpr sp)) >>= \ result_ty ->

    pure (SIR.Expr'Match id result_ty sp match_tok_sp testing arms)

expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id <$> (TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var $ TypeSolver.PoisonExpr sp)) <*> pure sp
expr (SIR.Expr'Hole id () sp hid) = SIR.Expr'Hole id <$> (TypeSolver.Type'InferVar <$> lift (lift $ TypeSolver.new_infer_var $ TypeSolver.HoleExpr sp)) <*> pure sp <*> pure hid

expr (SIR.Expr'Forall id () sp vars e) =
    expr e >>= \ e ->
    pure (SIR.Expr'Forall id (TypeSolver.Type'Forall vars (SIR.expr_type e)) sp vars e)
expr (SIR.Expr'TypeApply id () sp e (arg, arg_ty)) =
    expr e >>= \ e ->
    type_expr arg >>= \ arg ->
    apply_type (TypeSolver.TypeApplyExpr sp) sp (SIR.expr_type e) arg_ty >>= \ result_ty ->
    pure (SIR.Expr'TypeApply id result_ty sp e (arg, arg_ty))

expr (SIR.Expr'TypeAnnotation id () sp (annotation, annotation_ty) e) =
    type_expr annotation >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [TypeSolver.Expect TypeSolver.InTypeAnnotation (Located (SIR.type_expr_span annotation) (SIR.expr_type e)) annotation_ty]) >>
    pure (SIR.Expr'TypeAnnotation id annotation_ty sp (annotation, annotation_ty) e)

split_iden :: SIR.SplitIdentifier Untyped start -> ContextReader TypedWithInferVarsADTArena type_synonyms quant_vars vars (SIR.SplitIdentifier TypedWithInferVars start)
split_iden (SIR.SplitIdentifier'Get texpr name) = type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr name)
split_iden (SIR.SplitIdentifier'Single start) = pure $ SIR.SplitIdentifier'Single start
