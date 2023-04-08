module UHF.Phases.Middle.Type.AddTypes (add) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithUnk
import UHF.Phases.Middle.Type.Error

import qualified UHF.Compiler as Compiler

type DeclBVReader = ReaderT (UntypedDeclArena, TypedWithUnkBoundValueArena) (WriterT [Constraint] StateWithUnk)

get_bv_type :: SIR.BoundValueKey -> DeclBVReader TypeWithUnk
get_bv_type bv = do
    (_, bvs) <- ask
    let (SIR.BoundValue _ ty _) = Arena.get bvs bv
    pure ty

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
add :: UntypedADTArena -> UntypedTypeSynonymArena -> UntypedBoundValueArena -> UntypedDeclArena -> WriterT [Constraint] StateWithUnk (TypedWithUnkADTArena, TypedWithUnkTypeSynonymArena, TypedWithUnkBoundValueArena, TypedWithUnkDeclArena)
add adts type_synonyms bound_values decls =
    lift (Arena.transformM bound_value bound_values) >>= \ bound_values ->
    runReaderT (
        Arena.transformM adt adts >>= \ adts ->
        Arena.transformM type_synonym  type_synonyms >>= \ type_synonyms ->
        Arena.transformM decl decls >>= \ decls ->
        pure (adts, type_synonyms, bound_values, decls)
    ) (decls, bound_values)

bound_value :: UntypedBoundValue -> StateWithUnk TypedWithUnkBoundValue
bound_value (SIR.BoundValue id () def_span) = SIR.BoundValue id <$> (Type.Type'Unknown <$> new_type_unknown (BoundValue def_span)) <*> pure def_span

decl :: UntypedDecl -> DeclBVReader TypedWithUnkDecl
decl (SIR.Decl'Type ty) = pure $ SIR.Decl'Type ty
decl (SIR.Decl'Module id nc bindings adts type_synonyms) = SIR.Decl'Module id nc <$> mapM binding bindings <*> pure adts <*> pure type_synonyms

adt :: UntypedADT -> DeclBVReader TypedWithUnkADT
adt (Type.ADT id name variants) = Type.ADT id name <$> mapM convert_variant variants
    where
        convert_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> type_expr ty) fields
        convert_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM type_expr fields

type_synonym :: UntypedTypeSynonym -> DeclBVReader TypedWithUnkTypeSynonym
type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> type_expr expansion

apply_type :: TypeUnknownForWhat -> TypeWithUnk -> TypeWithUnk -> DeclBVReader TypeWithUnk
apply_type for_what ty arg =
    lift (lift $ new_type_unknown for_what) >>= \ tyu ->
    -- tell [UnkIsApplyResult tyu ty arg] >> TODO
    pure (Type.Type'Unknown tyu)

type_expr :: UntypedTypeExpr -> DeclBVReader TypedWithUnkTypeExpr
type_expr (SIR.TypeExpr'Identifier () sp iden) = do
    (decls, _) <- ask
    ty <- case iden of -- TODO: make poison type variable
        Just i -> case Arena.get decls i of
            SIR.Decl'Module _ _ _ _ _ -> lift (lift (lift (Compiler.tell_error $ NotAType sp "a module"))) >> Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp))
            SIR.Decl'Type ty -> pure $ void_var_to_key ty
        Nothing -> Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp))
    pure (SIR.TypeExpr'Identifier ty sp iden)
    where
        -- basically useless function for converting Type Void to Type TypeUnknownKey
        void_var_to_key (Type.Type'ADT k) = Type.Type'ADT k
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

type_expr (SIR.TypeExpr'Hole () hid) = SIR.TypeExpr'Hole <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr $ just_span hid))) <*> pure hid
type_expr (SIR.TypeExpr'Forall () names ty) =
    type_expr ty >>= \ ty ->
    pure (SIR.TypeExpr'Forall (Type.Type'Forall names (SIR.type_expr_type_info ty)) names ty)
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) (SIR.type_expr_type_info ty) (SIR.type_expr_type_info arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
type_expr (SIR.TypeExpr'Wild () sp) = Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp)) >>= \ ty -> pure (SIR.TypeExpr'Wild ty sp)
type_expr (SIR.TypeExpr'Poison () sp) = Type.Type'Unknown <$> lift (lift $ new_type_unknown (TypeExpr sp)) >>= \ ty -> pure (SIR.TypeExpr'Poison ty sp)

binding :: UntypedBinding -> DeclBVReader TypedWithUnkBinding
binding (SIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (SIR.Binding p eq_sp e)

loc_pat_type :: SIR.Pattern type_info -> Located type_info
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr identifier type_expr type_info binary_ops_allowed -> Located type_info
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)

pattern :: UntypedPattern -> DeclBVReader TypedWithUnkPattern
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

pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> DeclBVReader TypedWithUnkExpr
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

expr (SIR.Expr'Case id () sp case_tok_sp testing arms) =
    expr testing >>= \ testing ->
    mapM (\ (p, e) -> (,) <$> pattern p <*> expr e) arms >>= \ arms ->

    -- first expr matches all pattern types
    lift (tell (map (\ (arm_pat, _) -> Eq InCasePatterns case_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms)) >>
    -- all arm types are the same
    lift (tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> Eq InCaseArms case_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms))) >>

    (case headMay arms of
        Just (_, first_arm_result) -> pure $ SIR.expr_type first_arm_result
        Nothing -> Type.Type'Unknown <$> lift (lift $ new_type_unknown $ CaseExpr sp)) >>= \ result_ty ->

    pure (SIR.Expr'Case id result_ty sp case_tok_sp testing arms)

expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ PoisonExpr sp)) <*> pure sp
expr (SIR.Expr'Hole id () sp hid) = SIR.Expr'Hole id <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ HoleExpr sp)) <*> pure sp <*> pure hid

expr (SIR.Expr'Forall id () sp vars e) =
    expr e >>= \ e ->
    pure (SIR.Expr'Forall id (Type.Type'Forall vars (SIR.expr_type e)) sp vars e)
expr (SIR.Expr'TypeApply id () sp e arg) =
    expr e >>= \ e ->
    type_expr arg >>= \ arg ->
    apply_type (TypeApplyExpr sp) (SIR.expr_type e) (SIR.type_expr_type_info arg) >>= \ result_ty ->
    pure (SIR.Expr'TypeApply id result_ty sp e arg)

expr (SIR.Expr'TypeAnnotation id () sp annotation e) =
    type_expr annotation >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [Expect InTypeAnnotation (loc_expr_type e) (SIR.type_expr_type_info annotation)]) >> -- TODO: use annotation span
    pure (SIR.Expr'TypeAnnotation id (SIR.type_expr_type_info annotation) sp annotation e)
