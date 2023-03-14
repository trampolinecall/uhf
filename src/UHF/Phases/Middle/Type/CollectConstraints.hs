module UHF.Phases.Middle.Type.CollectConstraints (collect) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Constraint
import UHF.Phases.Middle.Type.StateWithVars
import qualified UHF.Phases.Middle.Type.ConvertTypeExpr as ConvertTypeExpr

type DeclBVReader = ReaderT (UntypedDeclArena, TypedWithVarsBoundValueArena) (WriterT [Constraint] StateWithVars)

read_decls :: DeclBVReader UntypedDeclArena
read_decls = ReaderT $ \ (ds, _) -> pure ds
read_bvs :: DeclBVReader TypedWithVarsBoundValueArena
read_bvs = ReaderT $ \ (_, bvs) -> pure bvs

-- TODO: sort constraints by priority so that certain weird things dont happen
-- TODO: reconsider this todo now that bindings are stored in let blocks
-- for example:
-- ```
-- test = let x = \ (a) -> :string a; x(0);
-- ```
-- produces "
--     scratch.uhf:1:14: error: conflicting types in assignment: 'int' vs 'string'
--       > scratch.uhf
--     1 | test = let x = \ (a) -> :string a;
--       |            ~ ^ ~~~~~~~~~~~~~~~~~~
--       |            `-- int -> _         `-- string -> string
-- "
-- but it really should produce an error at `x(0)` saying that x takes a string and not an int

collect :: UntypedDeclArena -> TypedWithVarsBoundValueArena -> UntypedDecl -> WriterT [Constraint] StateWithVars TypedWithVarsDecl
collect _ _ (HIR.Decl'Type ty) = pure $ HIR.Decl'Type ty
collect decls bva (HIR.Decl'Module id nc bindings adts type_synonyms) = runReaderT (HIR.Decl'Module id nc <$> mapM binding bindings <*> pure adts <*> pure type_synonyms) (decls, bva)

binding :: UntypedBinding -> DeclBVReader TypedWithVarsBinding
binding (HIR.Binding p eq_sp e) =
    pattern p >>= \ p ->
    expr e >>= \ e ->
    lift (tell [Eq InAssignment eq_sp (loc_pat_type p) (loc_expr_type e)]) >>
    pure (HIR.Binding p eq_sp e)

loc_pat_type :: HIR.Pattern type_expr type_info -> Located type_info
loc_pat_type pattern = Located (HIR.pattern_span pattern) (HIR.pattern_type pattern)
loc_expr_type :: HIR.Expr identifier type_expr type_info binary_ops_allowed -> Located type_info
loc_expr_type expr = Located (HIR.expr_span expr) (HIR.expr_type expr)

pattern :: UntypedPattern -> DeclBVReader TypedWithVarsPattern
pattern (HIR.Pattern'Identifier () sp bv) =
    read_bvs >>= \ bvs ->
    let (HIR.BoundValue ty _) = Arena.get bvs bv
    in pure (HIR.Pattern'Identifier ty sp bv)

pattern (HIR.Pattern'Wildcard () sp) =
    lift (lift (Type.Type'Variable <$> new_type_variable (WildcardPattern sp))) >>= \ ty ->
    pure (HIR.Pattern'Wildcard ty sp)

pattern (HIR.Pattern'Tuple () sp l r) =
    pattern l >>= \ l ->
    pattern r >>= \ r ->
    pure (HIR.Pattern'Tuple (Type.Type'Tuple (HIR.pattern_type l) (HIR.pattern_type r)) sp l r)

pattern (HIR.Pattern'Named () sp at_sp bvk subpat) =
    pattern subpat >>= \ subpat ->
    read_bvs >>= \ bvs ->
    let (HIR.BoundValue bv_ty _) = Arena.get bvs (unlocate bvk)
    in lift (tell [Eq InNamedPattern at_sp (Located (just_span bvk) bv_ty) (loc_pat_type subpat)]) >>
    pure (HIR.Pattern'Named bv_ty sp at_sp bvk subpat)

pattern (HIR.Pattern'Poison () sp) = HIR.Pattern'Poison <$> (Type.Type'Variable <$> lift (lift $ new_type_variable $ PoisonPattern sp)) <*> pure sp

expr :: UntypedExpr -> DeclBVReader TypedWithVarsExpr
expr (HIR.Expr'Identifier () sp bv) =
    read_bvs >>= \ bvs ->
    (case unlocate bv of
        Just bv -> let (HIR.BoundValue ty _) = Arena.get bvs bv in pure ty
        Nothing -> Type.Type'Variable <$> lift (lift $ new_type_variable (UnresolvedIdenExpr sp))) >>= \ ty ->

    pure (HIR.Expr'Identifier ty sp bv)

expr (HIR.Expr'Char () sp c) = pure (HIR.Expr'Char Type.Type'Char sp c)
expr (HIR.Expr'String () sp t) = pure (HIR.Expr'String Type.Type'String sp t)
expr (HIR.Expr'Int () sp i) = pure (HIR.Expr'Int Type.Type'Int sp i)
expr (HIR.Expr'Float () sp r) = pure (HIR.Expr'Float Type.Type'Float sp r)
expr (HIR.Expr'Bool () sp b) = pure (HIR.Expr'Bool Type.Type'Bool sp b)

expr (HIR.Expr'Tuple () sp l r) = expr l >>= \ l -> expr r >>= \ r -> pure (HIR.Expr'Tuple (Type.Type'Tuple (HIR.expr_type l) (HIR.expr_type r)) sp l r)

expr (HIR.Expr'Lambda () sp param body) =
    pattern param >>= \ param ->
    expr body >>= \ body ->
    pure (HIR.Expr'Lambda (Type.Type'Function (HIR.pattern_type param) (HIR.expr_type body)) sp param body)

expr (HIR.Expr'Let () sp bindings result) =
    mapM binding bindings >>= \ bindings ->
    expr result >>= \ result ->
    pure (HIR.Expr'Let (HIR.expr_type result) sp bindings result)

expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void

expr (HIR.Expr'Call () sp callee arg) =
    expr callee >>= \ callee ->
    expr arg >>= \ arg ->
    lift (lift $ new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

    lift (tell [Expect InCallExpr (loc_expr_type callee) (Type.Type'Function (HIR.expr_type arg) (Type.Type'Variable res_ty_var))]) >>

    pure (HIR.Expr'Call (Type.Type'Variable res_ty_var) sp callee arg)

expr (HIR.Expr'If () sp if_sp cond true false) =
    expr cond >>= \ cond ->
    expr true >>= \ true ->
    expr false >>= \ false ->

    lift (tell
        [ Expect InIfCondition (loc_expr_type cond) Type.Type'Bool
        , Eq InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
        ]) >>

    pure (HIR.Expr'If (HIR.expr_type true) sp if_sp cond true false)

expr (HIR.Expr'Case () sp case_tok_sp testing arms) =
    expr testing >>= \ testing ->
    mapM (\ (p, e) -> (,) <$> pattern p <*> expr e) arms >>= \ arms ->

    -- first expr matches all pattern types
    lift (tell (map (\ (arm_pat, _) -> Eq InCasePatterns case_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms)) >>
    -- all arm types are the same
    lift (tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> Eq InCaseArms case_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms))) >>

    (case headMay arms of
        Just (_, first_arm_result) -> pure $ HIR.expr_type first_arm_result
        Nothing -> Type.Type'Variable <$> lift (lift $ new_type_variable $ CaseExpr sp)) >>= \ result_ty ->

    pure (HIR.Expr'Case result_ty sp case_tok_sp testing arms)

expr (HIR.Expr'Poison () sp) = HIR.Expr'Poison <$> (Type.Type'Variable <$> lift (lift $ new_type_variable $ PoisonExpr sp)) <*> pure sp

expr (HIR.Expr'TypeAnnotation () sp annotation e) =
    read_decls >>= \ decls ->
    lift (lift $ ConvertTypeExpr.type_expr decls annotation) >>= \ annotation ->
    expr e >>= \ e ->
    lift (tell [Expect InTypeAnnotation (loc_expr_type e) annotation]) >> -- TODO: use annotation span
    pure (HIR.Expr'TypeAnnotation annotation sp annotation e)
