module UHF.Phases.SolveTypes.AddTypes (add) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.TypeWithInferVar hiding (Type)
import qualified UHF.Phases.SolveTypes.Solver.TypeWithInferVar as TypeWithInferVar (Type (..))
import UHF.Phases.SolveTypes.StateWithInferVars
import UHF.Phases.SolveTypes.Utils
import UHF.Phases.SolveTypes.Solver.Utils -- TODO: organize these modules better
import UHF.Source.Located (Located (..))
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Util.Arena as Arena

type ContextReader vars adts = ReaderT (vars, adts) (WriterT [Constraint] StateWithInferVars)

-- TODO: make helper functions to not use lift

get_var_type :: SIR.VariableKey -> ContextReader TypedWithInferVarsVariableArena adts TypeWithInferVars
get_var_type var = do
    (vars, _) <- ask
    case Arena.get vars var of
        SIR.Variable _ ty _ -> pure ty
        SIR.Variable'ADTVariant _ _ _ ty _ -> pure ty

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

add :: UntypedModuleArena -> UntypedADTArena -> UntypedTypeSynonymArena -> UntypedVariableArena -> WriterT [Constraint] StateWithInferVars (TypedWithInferVarsModuleArena, TypedWithInferVarsADTArena, TypedWithInferVarsTypeSynonymArena, TypedWithInferVarsVariableArena)
add mods adts type_synonyms variables =
    runReaderT (
        Arena.transformM adt adts >>= \ adts ->
        Arena.transformM type_synonym  type_synonyms >>= \ type_synonyms ->
        pure (adts, type_synonyms)
    ) ((), ()) >>= \ (adts, type_synonyms) ->
    runReaderT (
        Arena.transformM variable variables
    ) ((), adts) >>= \ variables ->
    runReaderT (
        Arena.transformM module_ mods >>= \ mods ->
        pure (mods, adts, type_synonyms, variables)
    ) (variables, adts)

variable :: UntypedVariable -> ContextReader vars TypedWithInferVarsADTArena TypedWithInferVarsVariable
variable (SIR.Variable id () name@(Located def_span _)) = SIR.Variable id <$> lift (lift $ TypeWithInferVar.Type'InferVar <$> new_type_unknown (Variable def_span)) <*> pure name
variable (SIR.Variable'ADTVariant id variant_index@(Type.ADT.VariantIndex adt_key _) var_type_params () def_span) = do
    (_, adts) <- ask
    unk_arena <- lift (lift get)
    let (Type.ADT _ _ adt_type_params _) = Arena.get adts adt_key
    let variant = Type.ADT.get_variant adts variant_index
    let ty = case variant of
            Type.ADT.Variant'Named _ _ _ -> error "bound value should not be made for a named adt variant" -- TODO: statically make sure this cant happen?
            Type.ADT.Variant'Anon _ _ fields ->
                let change_type_params ty = foldl' (\ ty (adt_typaram, var_typaram) -> substitute unk_arena adt_typaram (TypeWithInferVar.Type'QuantVar var_typaram) ty) ty (zip adt_type_params var_type_params)
                    arg_tys = map (change_type_params . snd . snd) fields
                    wrap_in_forall = case var_type_params of
                        [] -> identity
                        param:more -> TypeWithInferVar.Type'Forall (param :| more)
                 in wrap_in_forall $ foldr TypeWithInferVar.Type'Function (TypeWithInferVar.Type'ADT adt_key (map TypeWithInferVar.Type'QuantVar var_type_params)) arg_tys -- function type that takes all the field types and then results in the adt type
    pure $ SIR.Variable'ADTVariant id variant_index var_type_params ty def_span

module_ :: UntypedModule -> ContextReader TypedWithInferVarsVariableArena TypedWithInferVarsADTArena TypedWithInferVarsModule
module_ (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM binding bindings <*> pure adts <*> pure type_synonyms

adt :: UntypedADT -> ContextReader vars adts TypedWithInferVarsADT
adt (Type.ADT id name quant_vars variants) = Type.ADT id name quant_vars <$> mapM convert_variant variants
    where
        convert_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty_expr, ty)) -> type_expr ty_expr >>= \ ty_expr -> pure (id, name, (ty_expr, ty))) fields
        convert_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty_expr, ty)) -> type_expr ty_expr >>= \ ty_expr -> pure (id, (ty_expr, ty))) fields

type_synonym :: UntypedTypeSynonym -> ContextReader vars adts TypedWithInferVarsTypeSynonym
type_synonym (Type.TypeSynonym id name (expansion, exp_as_type)) = type_expr expansion >>= \ expansion -> pure (Type.TypeSynonym id name (expansion, exp_as_type))

type_expr :: UntypedTypeExpr -> ContextReader vars adts TypedWithInferVarsTypeExpr
-- TODO: do these ForWhats better
type_expr (SIR.TypeExpr'Refer evaled sp iden) = pure (SIR.TypeExpr'Refer evaled sp iden)
type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp <$> type_expr parent <*> pure name
type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp <$> type_expr a <*> type_expr b
type_expr (SIR.TypeExpr'Hole evaled ty sp hid) = pure (SIR.TypeExpr'Hole evaled ty sp hid)
type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp <$> type_expr arg <*> type_expr res
type_expr (SIR.TypeExpr'Forall evaled sp names t) = SIR.TypeExpr'Forall evaled sp names <$> type_expr t
type_expr (SIR.TypeExpr'Apply evaled sp t arg) = SIR.TypeExpr'Apply evaled sp <$> type_expr t <*> type_expr arg
type_expr (SIR.TypeExpr'Wild evaled sp) = pure (SIR.TypeExpr'Wild evaled sp)
type_expr (SIR.TypeExpr'Poison evaled sp) = pure (SIR.TypeExpr'Poison evaled sp)

binding :: UntypedBinding -> ContextReader TypedWithInferVarsVariableArena TypedWithInferVarsADTArena TypedWithInferVarsBinding
binding (SIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (SIR.Binding p eq_sp e)
binding (SIR.Binding'ADTVariant sp var_key vars variant) = pure $ SIR.Binding'ADTVariant sp var_key vars variant

loc_pat_type :: SIR.Pattern stage -> Located (SIR.TypeInfo stage)
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr stage -> Located (SIR.TypeInfo stage)
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)

pattern :: UntypedPattern -> ContextReader TypedWithInferVarsVariableArena TypedWithInferVarsADTArena TypedWithInferVarsPattern
pattern (SIR.Pattern'Identifier () sp var) =
    get_var_type var >>= \ ty ->
    pure (SIR.Pattern'Identifier ty sp var)

pattern (SIR.Pattern'Wildcard () sp) =
    TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown (WildcardPattern sp)) >>= \ ty ->
    pure (SIR.Pattern'Wildcard ty sp)

pattern (SIR.Pattern'Tuple () sp l r) =
    pattern l >>= \ l ->
    pattern r >>= \ r ->
    pure (SIR.Pattern'Tuple (TypeWithInferVar.Type'Tuple (SIR.pattern_type l) (SIR.pattern_type r)) sp l r)

pattern (SIR.Pattern'Named () sp at_sp var_key subpat) =
    pattern subpat >>= \ subpat ->
    get_var_type (unlocate var_key) >>= \ var_ty ->
    lift (tell [Eq InNamedPattern at_sp (Located (just_span var_key) var_ty) (loc_pat_type subpat)]) >>
    pure (SIR.Pattern'Named var_ty sp at_sp var_key subpat)

pattern (SIR.Pattern'AnonADTVariant () sp variant_iden Nothing _ fields) =
    mapM pattern fields >>= \ fields ->
    TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown (UnresolvedADTVariantPattern sp)) >>= \ ty ->
    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'AnonADTVariant ty sp variant_iden Nothing [] fields)
pattern (SIR.Pattern'AnonADTVariant () sp variant_iden (Just variant_index@(Type.ADT.VariantIndex adt_key _)) _ fields) =
    mapM pattern fields >>= \ pattern_fields ->

    ask >>= \ (_, adts) ->
    let Type.ADT _ _ type_params _ = Arena.get adts adt_key
        variant = Type.ADT.get_variant adts variant_index
    in

    mapM (\ var -> TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown $ ImplicitTyParam sp {- var TODO -})) type_params >>= \ type_param_unks -> -- TODO: declared span

    lift (lift get) >>= \ unk_arena ->
    let substitute_adt_params = foldr (.) identity (zipWith (substitute unk_arena) type_params type_param_unks)
        whole_pat_type = TypeWithInferVar.Type'ADT adt_key type_param_unks

    in case variant of
         Type.ADT.Variant'Anon _ _ variant_fields ->
            let variant_field_tys_substituted = map (substitute_adt_params . snd . snd) variant_fields
            in if length pattern_fields /= length variant_field_tys_substituted
                then error "wrong number of fields in anonymous variant pattern" -- TODO: report proper error
                else
                    zipWithM
                        (\ pat_field variant_field_ty ->
                            lift (tell [Expect InADTVariantPatternField (loc_pat_type pat_field) variant_field_ty]))
                        pattern_fields
                        variant_field_tys_substituted
         Type.ADT.Variant'Named _ _ _ -> error "named variant pattern used with anonymous variant" -- TODO: also report proper error
        >>

    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'AnonADTVariant whole_pat_type sp variant_iden (Just variant_index) type_param_unks pattern_fields)

pattern (SIR.Pattern'NamedADTVariant () sp variant_iden Nothing _ fields) =
    mapM (\ (field_name, field_pat) -> (field_name,) <$> pattern field_pat) fields >>= \ fields ->
    TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown (UnresolvedADTVariantPattern sp)) >>= \ ty ->
    split_iden variant_iden >>= \ variant_iden ->
    pure (SIR.Pattern'NamedADTVariant ty sp variant_iden Nothing [] fields)
pattern (SIR.Pattern'NamedADTVariant () _ _ (Just _) _ _) = todo
-- 4 things:
--     - check variant is named variant
--     - check field names are correct
--     - check all fields are covered
--     - put type constraints on all fields

pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown $ PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> ContextReader TypedWithInferVarsVariableArena TypedWithInferVarsADTArena TypedWithInferVarsExpr
expr (SIR.Expr'Identifier id () sp iden var) =
    (case var of
        Just var -> get_var_type var
        Nothing -> TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown (UnresolvedIdenExpr sp))) >>= \ ty ->

    split_iden iden >>= \ iden ->

    pure (SIR.Expr'Identifier id ty sp iden var)

expr (SIR.Expr'Char id () sp c) = pure (SIR.Expr'Char id TypeWithInferVar.Type'Char sp c)
expr (SIR.Expr'String id () sp t) = pure (SIR.Expr'String id TypeWithInferVar.Type'String sp t)
expr (SIR.Expr'Int id () sp i) = pure (SIR.Expr'Int id TypeWithInferVar.Type'Int sp i)
expr (SIR.Expr'Float id () sp r) = pure (SIR.Expr'Float id TypeWithInferVar.Type'Float sp r)
expr (SIR.Expr'Bool id () sp b) = pure (SIR.Expr'Bool id TypeWithInferVar.Type'Bool sp b)

expr (SIR.Expr'Tuple id () sp l r) = expr l >>= \ l -> expr r >>= \ r -> pure (SIR.Expr'Tuple id (TypeWithInferVar.Type'Tuple (SIR.expr_type l) (SIR.expr_type r)) sp l r)

expr (SIR.Expr'Lambda id () sp param body) =
    pattern param >>= \ param ->
    expr body >>= \ body ->
    pure (SIR.Expr'Lambda id (TypeWithInferVar.Type'Function (SIR.pattern_type param) (SIR.expr_type body)) sp param body)

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
    lift (lift $ new_type_unknown (CallExpr sp)) >>= \ res_ty_var ->

    lift (tell [Expect InCallExpr (loc_expr_type callee) (TypeWithInferVar.Type'Function (SIR.expr_type arg) (TypeWithInferVar.Type'InferVar res_ty_var))]) >>

    pure (SIR.Expr'Call id (TypeWithInferVar.Type'InferVar res_ty_var) sp callee arg)

expr (SIR.Expr'If id () sp if_sp cond true false) =
    expr cond >>= \ cond ->
    expr true >>= \ true ->
    expr false >>= \ false ->

    lift (tell
        [ Expect InIfCondition (loc_expr_type cond) TypeWithInferVar.Type'Bool
        , Eq InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
        ]) >>

    pure (SIR.Expr'If id (SIR.expr_type true) sp if_sp cond true false)

expr (SIR.Expr'Match id () sp match_tok_sp testing arms) =
    expr testing >>= \ testing ->
    mapM (\ (p, e) -> (,) <$> pattern p <*> expr e) arms >>= \ arms ->

    -- first expr matches all pattern types
    lift (tell (map (\ (arm_pat, _) -> Eq InMatchPatterns match_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms)) >>
    -- all arm types are the same
    lift (tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> Eq InMatchArms match_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms))) >>

    (case headMay arms of
        Just (_, first_arm_result) -> pure $ SIR.expr_type first_arm_result
        Nothing -> TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown $ MatchExpr sp)) >>= \ result_ty ->

    pure (SIR.Expr'Match id result_ty sp match_tok_sp testing arms)

expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id <$> (TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown $ PoisonExpr sp)) <*> pure sp
expr (SIR.Expr'Hole id () sp hid) = SIR.Expr'Hole id <$> (TypeWithInferVar.Type'InferVar <$> lift (lift $ new_type_unknown $ HoleExpr sp)) <*> pure sp <*> pure hid

expr (SIR.Expr'Forall id () sp vars e) =
    expr e >>= \ e ->
    pure (SIR.Expr'Forall id (TypeWithInferVar.Type'Forall vars (SIR.expr_type e)) sp vars e)
expr (SIR.Expr'TypeApply id () sp e (arg, arg_ty)) =
    expr e >>= \ e ->
    type_expr arg >>= \ arg ->
    lift (lift $ apply_type (TypeApplyExpr sp) sp (SIR.expr_type e) arg_ty) >>= \ (constraint, result_ty) ->
    lift (tell [constraint]) >>
    pure (SIR.Expr'TypeApply id result_ty sp e (arg, arg_ty))

expr (SIR.Expr'TypeAnnotation id () sp (annotation, annotation_ty) e) =
    type_expr annotation >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [Expect InTypeAnnotation (Located (SIR.type_expr_span annotation) (SIR.expr_type e)) annotation_ty]) >>
    pure (SIR.Expr'TypeAnnotation id annotation_ty sp (annotation, annotation_ty) e)

split_iden :: SIR.SplitIdentifier Untyped start -> ContextReader TypedWithInferVarsVariableArena TypedWithInferVarsADTArena (SIR.SplitIdentifier TypedWithInferVars start)
split_iden (SIR.SplitIdentifier'Get texpr name) = type_expr texpr >>= \ texpr -> pure (SIR.SplitIdentifier'Get texpr name)
split_iden (SIR.SplitIdentifier'Single start) = pure $ SIR.SplitIdentifier'Single start
