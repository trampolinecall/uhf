module UHF.Phases.Middle.Type.CollectConstraints (collect) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithUnk
import qualified UHF.Phases.Middle.Type.ConvertTypeExpr as ConvertTypeExpr

type DeclBVReader = ReaderT (UntypedDeclArena, TypedWithUnkBoundValueArena) (WriterT [Constraint] StateWithUnk)

read_decls :: DeclBVReader UntypedDeclArena
read_decls = ReaderT $ \ (ds, _) -> pure ds
read_bvs :: DeclBVReader TypedWithUnkBoundValueArena
read_bvs = ReaderT $ \ (_, bvs) -> pure bvs

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

collect :: UntypedDeclArena -> TypedWithUnkBoundValueArena -> UntypedDecl -> WriterT [Constraint] StateWithUnk TypedWithUnkDecl
collect _ _ (SIR.Decl'Type ty) = pure $ SIR.Decl'Type ty
collect decls bva (SIR.Decl'Module id nc bindings adts type_synonyms) = runReaderT (SIR.Decl'Module id nc <$> mapM binding bindings <*> pure adts <*> pure type_synonyms) (decls, bva)

binding :: UntypedBinding -> DeclBVReader TypedWithUnkBinding
binding (SIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (SIR.Binding p eq_sp e)

loc_pat_type :: SIR.Pattern type_expr type_info -> Located type_info
loc_pat_type pattern = Located (SIR.pattern_span pattern) (SIR.pattern_type pattern)
loc_expr_type :: SIR.Expr identifier type_expr type_info binary_ops_allowed -> Located type_info
loc_expr_type expr = Located (SIR.expr_span expr) (SIR.expr_type expr)

pattern :: UntypedPattern -> DeclBVReader TypedWithUnkPattern
pattern (SIR.Pattern'Identifier () sp bv) =
    read_bvs >>= \ bvs ->
    let (SIR.BoundValue _ ty _) = Arena.get bvs bv
    in pure (SIR.Pattern'Identifier ty sp bv)

pattern (SIR.Pattern'Wildcard () sp) =
    lift (lift (Type.Type'Unknown <$> new_type_unknown (WildcardPattern sp))) >>= \ ty ->
    pure (SIR.Pattern'Wildcard ty sp)

pattern (SIR.Pattern'Tuple () sp l r) =
    pattern l >>= \ l ->
    pattern r >>= \ r ->
    pure (SIR.Pattern'Tuple (Type.Type'Tuple (SIR.pattern_type l) (SIR.pattern_type r)) sp l r)

pattern (SIR.Pattern'Named () sp at_sp bvk subpat) =
    pattern subpat >>= \ subpat ->
    read_bvs >>= \ bvs ->
    let (SIR.BoundValue _ bv_ty _) = Arena.get bvs (unlocate bvk)
    in lift (tell [Eq InNamedPattern at_sp (Located (just_span bvk) bv_ty) (loc_pat_type subpat)]) >>
    pure (SIR.Pattern'Named bv_ty sp at_sp bvk subpat)

pattern (SIR.Pattern'Poison () sp) = SIR.Pattern'Poison <$> (Type.Type'Unknown <$> lift (lift $ new_type_unknown $ PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> DeclBVReader TypedWithUnkExpr
expr (SIR.Expr'Identifier id () sp bv) =
    read_bvs >>= \ bvs ->
    (case unlocate bv of
        Just bv -> let (SIR.BoundValue _ ty _) = Arena.get bvs bv in pure ty
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

expr (SIR.Expr'Forall _ () _ _ _) = todo
expr (SIR.Expr'TypeApply _ () _ _ _) = todo

expr (SIR.Expr'TypeAnnotation id () sp annotation e) =
    read_decls >>= \ decls ->
    lift (lift $ ConvertTypeExpr.type_expr decls annotation) >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [Expect InTypeAnnotation (loc_expr_type e) annotation]) >> -- TODO: use annotation span
    pure (SIR.Expr'TypeAnnotation id annotation sp annotation e)
