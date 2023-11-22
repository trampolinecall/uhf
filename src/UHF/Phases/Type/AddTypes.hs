module UHF.Phases.Type.AddTypes (add) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))
import UHF.IO.Span (Span)

import UHF.Phases.Type.Unknown
import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Constraint
import UHF.Phases.Type.StateWithUnk
import UHF.Phases.Type.Utils
import UHF.Phases.Type.Error

import qualified UHF.Compiler as Compiler

type ContextReader decls bvs adts = ReaderT (decls, bvs, adts) (WriterT [Constraint] StateWithUnk)

-- TODO: make helper functions to not use lift

get_bv_type :: SIR.BoundValueKey -> ContextReader decls TypedWithUnkBoundValueArena adts TypeWithUnk
get_bv_type bv = do
    (_, bvs, _) <- ask
    case Arena.get bvs bv of
        SIR.BoundValue _ ty _ -> pure ty
        SIR.BoundValue'ADTVariant _ _ _ ty _ -> pure ty

-- TODO: sort constraints by priority so that certain weird things dont happen
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
--
add :: UntypedModuleArena -> UntypedADTArena -> UntypedTypeSynonymArena -> UntypedBoundValueArena -> UntypedDeclArena -> WriterT [Constraint] StateWithUnk (TypedWithUnkModuleArena, TypedWithUnkADTArena, TypedWithUnkTypeSynonymArena, TypedWithUnkBoundValueArena, TypedWithUnkDeclArena)
add mods adts type_synonyms bound_values decls =
    runReaderT (
        Arena.transformM adt adts >>= \ adts ->
        Arena.transformM type_synonym  type_synonyms >>= \ type_synonyms ->
        pure (adts, type_synonyms)
    ) (decls, (), ()) >>= \ (adts, type_synonyms) ->
    runReaderT (
        Arena.transformM bound_value bound_values
    ) ((), (), adts) >>= \ bound_values ->
    runReaderT (
        Arena.transformM module_ mods >>= \ mods ->
        pure (mods, adts, type_synonyms, bound_values, decls)
    ) (decls, bound_values, adts)

bound_value :: UntypedBoundValue -> ContextReader decls bvs TypedWithUnkADTArena TypedWithUnkBoundValue
bound_value (SIR.BoundValue id () name@(Located def_span _)) = SIR.BoundValue id <$> lift (lift $ Type.Type'Unknown <$> new_type_unknown (BoundValue def_span)) <*> pure name
bound_value (SIR.BoundValue'ADTVariant id variant_index@(Type.ADTVariantIndex adt_key _) bv_type_params () def_span) = do
    (_, _, adts) <- ask
    unk_arena <- lift (lift get)
    let (Type.ADT _ _ adt_type_params _) = Arena.get adts adt_key
    let variant = Type.get_adt_variant adts variant_index
    let ty = case variant of
            Type.ADTVariant'Named _ _ _ -> error "bound value should not be made for a named adt variant" -- TODO: statically make sure this cant happen?
            Type.ADTVariant'Anon _ _ fields ->
                let change_type_params ty = foldl' (\ ty (adt_typaram, bv_typaram) -> substitute unk_arena adt_typaram (Type.Type'Variable bv_typaram) ty) ty (zip adt_type_params bv_type_params)
                    arg_tys = map (change_type_params . SIR.type_expr_type_info . snd) fields
                    wrap_in_forall = case bv_type_params of
                        [] -> identity
                        param:more -> Type.Type'Forall (param :| more)
                 in wrap_in_forall $ foldr Type.Type'Function (Type.Type'ADT adt_key (map Type.Type'Variable bv_type_params)) arg_tys -- function type that takes all the field types and then results in the adt type
    pure $ SIR.BoundValue'ADTVariant id variant_index bv_type_params ty def_span

module_ :: UntypedModule -> ContextReader UntypedDeclArena TypedWithUnkBoundValueArena TypedWithUnkADTArena TypedWithUnkModule
module_ (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM binding bindings <*> pure adts <*> pure type_synonyms

adt :: UntypedADT -> ContextReader UntypedDeclArena bvs adts TypedWithUnkADT
adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM convert_variant variants
    where
        convert_variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, ty) -> (id, name,) <$> type_expr ty) fields
        convert_variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, ty) -> (id,) <$> type_expr ty) fields

type_synonym :: UntypedTypeSynonym -> ContextReader UntypedDeclArena bvs adts TypedWithUnkTypeSynonym
type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> type_expr expansion

apply_type :: TypeUnknownForWhat -> Span -> TypeWithUnk -> TypeWithUnk -> ContextReader decls bvs adts TypeWithUnk
apply_type for_what sp ty arg =
    lift (lift $ new_type_unknown for_what) >>= \ tyu ->
    lift (tell [UnkIsApplyResult sp tyu ty arg]) >>
    pure (Type.Type'Unknown tyu)

type_expr :: UntypedTypeExpr -> ContextReader UntypedDeclArena bvs adts TypedWithUnkTypeExpr
type_expr (SIR.TypeExpr'Identifier () sp iden) = do
    (decls, _, _) <- ask
    ty <- case iden of -- TODO: make poison type variable
        Just i -> case Arena.get decls i of
            SIR.Decl'Module _ -> lift (lift (lift (Compiler.tell_error $ NotAType sp "a module"))) >> Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp))
            SIR.Decl'Type ty -> pure $ void_var_to_key ty
        Nothing -> Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp))
    pure (SIR.TypeExpr'Identifier ty sp iden)
    where
        -- basically useless function for converting Type Void to Type TypeUnknownKey
        void_var_to_key (Type.Type'ADT k params) = Type.Type'ADT k (map void_var_to_key params)
        void_var_to_key (Type.Type'Synonym k) = Type.Type'Synonym k
        void_var_to_key Type.Type'Int = Type.Type'Int
        void_var_to_key Type.Type'Float = Type.Type'Float
        void_var_to_key Type.Type'Char = Type.Type'Char
        void_var_to_key Type.Type'String = Type.Type'String
        void_var_to_key Type.Type'Bool = Type.Type'Bool
        void_var_to_key (Type.Type'Function a r) = Type.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (Type.Type'Tuple a b) = Type.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (Type.Type'Unknown void) = absurd void
        void_var_to_key (Type.Type'Variable v) = Type.Type'Variable v
        void_var_to_key (Type.Type'Forall vars ty) = Type.Type'Forall vars (void_var_to_key ty)

type_expr (SIR.TypeExpr'Tuple () a b) =
    type_expr a >>= \ a_conv ->
    type_expr b >>= \ b_conv ->
    pure (SIR.TypeExpr'Tuple (Type.Type'Tuple (SIR.type_expr_type_info a_conv) (SIR.type_expr_type_info b_conv)) a_conv b_conv)

type_expr (SIR.TypeExpr'Hole () sp hid) = SIR.TypeExpr'Hole <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeHole sp))) <*> pure sp <*> pure hid
type_expr (SIR.TypeExpr'Function () sp arg res) =
    type_expr arg >>= \ arg ->
    type_expr res >>= \ res ->
    pure (SIR.TypeExpr'Function (Type.Type'Function (SIR.type_expr_type_info arg) (SIR.type_expr_type_info res)) sp arg res)
type_expr (SIR.TypeExpr'Forall () names ty) =
    type_expr ty >>= \ ty ->
    pure (SIR.TypeExpr'Forall (Type.Type'Forall names (SIR.type_expr_type_info ty)) names ty)
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) sp (SIR.type_expr_type_info ty) (SIR.type_expr_type_info arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
type_expr (SIR.TypeExpr'Wild () sp) = Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp)) >>= \ ty -> pure (SIR.TypeExpr'Wild ty sp)
type_expr (SIR.TypeExpr'Poison () sp) = Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp)) >>= \ ty -> pure (SIR.TypeExpr'Poison ty sp)

binding :: UntypedBinding -> ContextReader UntypedDeclArena TypedWithUnkBoundValueArena TypedWithUnkADTArena TypedWithUnkBinding
binding (SIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (SIR.Binding p eq_sp e)
binding (SIR.Binding'ADTVariant sp bvk vars variant) = pure $ SIR.Binding'ADTVariant sp bvk vars variant

loc_pat_type :: SIR.Pattern p_iden type_info -> Located type_info
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr d_iden v_iden p_iden type_info binary_ops_allowed -> Located type_info
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)

pattern :: UntypedPattern -> ContextReader decls TypedWithUnkBoundValueArena TypedWithUnkADTArena TypedWithUnkPattern
pattern (SIR.Pattern'Identifier () sp bv) =
    get_bv_type bv >>= \ ty ->
    pure (SIR.Pattern'Identifier ty sp bv)

pattern (SIR.Pattern'Wildcard () sp) =
    Type.Type'Unknown <$> lift (lift $ new_type_unknown (WildcardPattern sp)) >>= \ ty ->
    pure (SIR.Pattern'Wildcard ty sp)

pattern (SIR.Pattern'Tuple () sp l r) =
    pattern l >>= \ l ->
    pattern r >>= \ r ->
    pure (SIR.Pattern'Tuple (Type.Type'Tuple (SIR.pattern_type l) (SIR.pattern_type r)) sp l r)

pattern (SIR.Pattern'Named () sp at_sp bvk subpat) =
    pattern subpat >>= \ subpat ->
    get_bv_type (unlocate bvk) >>= \ bv_ty ->
    lift (tell [Eq InNamedPattern at_sp (Located (just_span bvk) bv_ty) (loc_pat_type subpat)]) >>
    pure (SIR.Pattern'Named bv_ty sp at_sp bvk subpat)

pattern (SIR.Pattern'AnonADTVariant () sp Nothing _ fields) =
    mapM pattern fields >>= \ fields ->
    Type.Type'Unknown <$> lift (lift $ new_type_unknown (UnresolvedADTVariantPattern sp)) >>= \ ty ->
    pure (SIR.Pattern'AnonADTVariant ty sp Nothing [] fields)
pattern (SIR.Pattern'AnonADTVariant () sp (Just variant_index@(Type.ADTVariantIndex adt_key _)) _ fields) =
    mapM pattern fields >>= \ pattern_fields ->

    ask >>= \ (_, _, adts) ->
    let Type.ADT _ _ type_params _ = Arena.get adts adt_key
        variant = Type.get_adt_variant adts variant_index
    in

    mapM (\ var -> Type.Type'Unknown <$> lift (lift $ new_type_unknown $ ImplicitTyParam sp {- var TODO -})) type_params >>= \ type_param_unks -> -- TODO: declared span

    lift (lift get) >>= \ unk_arena ->
    let substitute_adt_params = foldr (.) identity (zipWith (substitute unk_arena) type_params type_param_unks)
        whole_pat_type = Type.Type'ADT adt_key type_param_unks

    in case variant of
         Type.ADTVariant'Anon _ _ variant_fields ->
            let variant_field_tys_substituted = map (substitute_adt_params . SIR.type_expr_type_info . snd) variant_fields
            in if length pattern_fields /= length variant_field_tys_substituted
                then error "wrong number of fields in anonymous variant pattern" -- TODO: report proper error
                else
                    zipWithM
                        (\ pat_field variant_field_ty ->
                            lift (tell [Expect InADTVariantPatternField (loc_pat_type pat_field) variant_field_ty]))
                        pattern_fields
                        variant_field_tys_substituted
         Type.ADTVariant'Named _ _ _ -> error "named variant pattern used with anonymous variant" -- TODO: also report proper error
        >>

    pure (SIR.Pattern'AnonADTVariant whole_pat_type sp (Just variant_index) type_param_unks pattern_fields)

pattern (SIR.Pattern'NamedADTVariant () sp Nothing _ fields) =
    mapM (\ (field_name, field_pat) -> (field_name,) <$> pattern field_pat) fields >>= \ fields ->
    Type.Type'Unknown <$> lift (lift $ new_type_unknown (UnresolvedADTVariantPattern sp)) >>= \ ty ->
    pure (SIR.Pattern'NamedADTVariant ty sp Nothing [] fields)
pattern (SIR.Pattern'NamedADTVariant () _ (Just _) _ _) = todo
-- 4 things:
--     - check variant is named variant
--     - check field names are correct
--     - check all fields are covered
--     - put type constraints on all fields

pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> ContextReader UntypedDeclArena TypedWithUnkBoundValueArena TypedWithUnkADTArena TypedWithUnkExpr
expr (SIR.Expr'Identifier id () sp bv) =
    (case unlocate bv of
        Just bv -> get_bv_type bv
        Nothing -> Type.Type'Unknown <$> lift (lift $ new_type_unknown (UnresolvedIdenExpr sp))) >>= \ ty ->

    pure (SIR.Expr'Identifier id ty sp bv)

expr (SIR.Expr'Char id () sp c) = pure (SIR.Expr'Char id Type.Type'Char sp c)
expr (SIR.Expr'String id () sp t) = pure (SIR.Expr'String id Type.Type'String sp t)
expr (SIR.Expr'Int id () sp i) = pure (SIR.Expr'Int id Type.Type'Int sp i)
expr (SIR.Expr'Float id () sp r) = pure (SIR.Expr'Float id Type.Type'Float sp r)
expr (SIR.Expr'Bool id () sp b) = pure (SIR.Expr'Bool id Type.Type'Bool sp b)

expr (SIR.Expr'Tuple id () sp l r) = expr l >>= \ l -> expr r >>= \ r -> pure (SIR.Expr'Tuple id (Type.Type'Tuple (SIR.expr_type l) (SIR.expr_type r)) sp l r)

expr (SIR.Expr'Lambda id () sp param body) =
    pattern param >>= \ param ->
    expr body >>= \ body ->
    pure (SIR.Expr'Lambda id (Type.Type'Function (SIR.pattern_type param) (SIR.expr_type body)) sp param body)

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

    lift (tell [Expect InCallExpr (loc_expr_type callee) (Type.Type'Function (SIR.expr_type arg) (Type.Type'Unknown res_ty_var))]) >>

    pure (SIR.Expr'Call id (Type.Type'Unknown res_ty_var) sp callee arg)

expr (SIR.Expr'If id () sp if_sp cond true false) =
    expr cond >>= \ cond ->
    expr true >>= \ true ->
    expr false >>= \ false ->

    lift (tell
        [ Expect InIfCondition (loc_expr_type cond) Type.Type'Bool
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
        Nothing -> Type.Type'Unknown <$> lift (lift $ new_type_unknown $ MatchExpr sp)) >>= \ result_ty ->

    pure (SIR.Expr'Match id result_ty sp match_tok_sp testing arms)

expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ PoisonExpr sp)) <*> pure sp
expr (SIR.Expr'Hole id () sp hid) = SIR.Expr'Hole id <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ HoleExpr sp)) <*> pure sp <*> pure hid

expr (SIR.Expr'Forall id () sp vars e) =
    expr e >>= \ e ->
    pure (SIR.Expr'Forall id (Type.Type'Forall vars (SIR.expr_type e)) sp vars e)
expr (SIR.Expr'TypeApply id () sp e arg) =
    expr e >>= \ e ->
    type_expr arg >>= \ arg ->
    apply_type (TypeApplyExpr sp) sp (SIR.expr_type e) (SIR.type_expr_type_info arg) >>= \ result_ty ->
    pure (SIR.Expr'TypeApply id result_ty sp e arg)

expr (SIR.Expr'TypeAnnotation id () sp annotation e) =
    type_expr annotation >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [Expect InTypeAnnotation (loc_expr_type e) (SIR.type_expr_type_info annotation)]) >> -- TODO: use annotation span
    pure (SIR.Expr'TypeAnnotation id (SIR.type_expr_type_info annotation) sp annotation e)
