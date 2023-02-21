module UHF.Type (typecheck) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.HIR as HIR

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))

import UHF.Type.Var
import UHF.Type.Aliases
import UHF.Type.Error
import UHF.Type.Constraint

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

type StateWithVars = StateT TypeVarArena (Writer [Error])

new_type_variable :: TypeVarForWhat -> StateWithVars TypeVarKey
new_type_variable for_what =
    state $ \ type_vars ->
        Arena.put (TypeVar for_what Fresh) type_vars

-- also does type inference
typecheck :: (DeclArena, UntypedNominalTypeArena, UntypedBindingArena, UntypedBoundValueArena) -> Compiler.Compiler (DeclArena, TypedNominalTypeArena, TypedBindingArena, TypedBoundValueArena)
typecheck (decls, nominal_types, bindings, bound_values) =
    let (res, errs) =
            runWriter (
                runStateT
                    (
                        Arena.transformM (convert_type_exprs_in_nominal_types decls) nominal_types >>= \ nominal_types ->
                        Arena.transformM assign_type_variable_to_bound_value bound_values >>= \ bound_values ->
                        runWriterT (Arena.transformM (collect_constraints decls bound_values) bindings) >>= \ (bindings, constraints) ->
                        solve_constraints nominal_types constraints >>
                        pure (nominal_types, bound_values, bindings)
                    )
                    Arena.new >>= \ ((nominal_types, bound_values, bindings), vars) ->

                convert_vars vars >>= \ vars ->
                let nominal_types' = Arena.transform (remove_vars_from_nominal_type vars) nominal_types
                    bound_values' = Arena.transform (remove_vars_from_bound_value vars) bound_values
                    bindings' = Arena.transform (remove_vars_from_binding vars) bindings
                in
                pure (decls, nominal_types', bindings', bound_values')
            )
    in Compiler.errors errs >> pure res

convert_type_exprs_in_nominal_types :: DeclArena -> UntypedNominalType -> StateWithVars TypedWithVarsNominalType
convert_type_exprs_in_nominal_types decls (HIR.NominalType'Data name variants) = HIR.NominalType'Data name <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (HIR.DataVariant'Named name fields) = HIR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> convert_type_expr decls ty) fields
        convert_variant decls (HIR.DataVariant'Anon name fields) = HIR.DataVariant'Anon name <$> mapM (convert_type_expr decls) fields

convert_type_exprs_in_nominal_types decls (HIR.NominalType'Synonym name expansion) = HIR.NominalType'Synonym name <$> convert_type_expr decls expansion

convert_type_expr :: DeclArena -> TypeExpr -> StateWithVars TypeWithVars
convert_type_expr decls (HIR.TypeExpr'Identifier sp iden) = case iden of -- TODO: make poison type variable
    Just i -> case Arena.get decls i of
        HIR.Decl'Module _ -> HIR.Type'Variable <$> new_type_variable (TypeExpr sp) -- TODO: report error for this
        HIR.Decl'Type ty -> pure $ void_var_to_key ty
    Nothing -> HIR.Type'Variable <$> new_type_variable (TypeExpr sp)
    where
        -- basically useless function for converting Type Void to Type TypeVarKey
        void_var_to_key (HIR.Type'Nominal k) = HIR.Type'Nominal k
        void_var_to_key HIR.Type'Int = HIR.Type'Int
        void_var_to_key HIR.Type'Float = HIR.Type'Float
        void_var_to_key HIR.Type'Char = HIR.Type'Char
        void_var_to_key HIR.Type'String = HIR.Type'String
        void_var_to_key HIR.Type'Bool = HIR.Type'Bool
        void_var_to_key (HIR.Type'Function a r) = HIR.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (HIR.Type'Tuple a b) = HIR.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (HIR.Type'Variable void) = absurd void

convert_type_expr decls (HIR.TypeExpr'Tuple a b) = HIR.Type'Tuple <$> convert_type_expr decls a <*> convert_type_expr decls b
convert_type_expr _ (HIR.TypeExpr'Poison sp) = HIR.Type'Variable <$> new_type_variable (TypeExpr sp)

assign_type_variable_to_bound_value :: UntypedBoundValue -> StateWithVars TypedWithVarsBoundValue
assign_type_variable_to_bound_value (HIR.BoundValue () def_span) = HIR.BoundValue <$> (HIR.Type'Variable <$> new_type_variable (BoundValue def_span)) <*> pure def_span

collect_constraints :: DeclArena -> TypedWithVarsBoundValueArena -> UntypedBinding -> WriterT [Constraint] StateWithVars TypedWithVarsBinding
collect_constraints decls bna (HIR.Binding pat eq_sp expr) =
    collect_for_pat pat >>= \ pat ->
    collect_for_expr expr >>= \ expr ->
    tell [Eq InAssignment eq_sp (loc_pat_type pat) (loc_expr_type expr)] >>
    pure (HIR.Binding pat eq_sp expr)
    where
        -- TODO: sort constraints by priority so that certain weird things dont happen
        -- for example:
        -- ```
        -- test = let x = \ (a) -> :string a;
        --     x(0)
        -- ```
        -- produces "
        --     scratch.uhf:1:14: error: conflicting types in assignment: 'int' vs 'string'
        --       > scratch.uhf
        --     1 | test = let x = \ (a) -> :string a;
        --       |            ~ ^ ~~~~~~~~~~~~~~~~~~
        --       |            `-- int -> _         `-- string -> string
        -- "
        -- but it really should produce an error at `x(0)` saying that x takes a string and not an int

        loc_pat_type pat = Located (HIR.pattern_span pat) (HIR.pattern_type pat)
        loc_expr_type expr = Located (HIR.expr_span expr) (HIR.expr_type expr)

        collect_for_pat (HIR.Pattern'Identifier () sp bn) =
            let (HIR.BoundValue ty _) = Arena.get bna bn
            in pure (HIR.Pattern'Identifier ty sp bn)

        collect_for_pat (HIR.Pattern'Wildcard () sp) =
            lift (HIR.Type'Variable <$> new_type_variable (WildcardPattern sp)) >>= \ ty ->
            pure (HIR.Pattern'Wildcard ty sp)

        collect_for_pat (HIR.Pattern'Tuple () sp l r) =
            collect_for_pat l >>= \ l ->
            collect_for_pat r >>= \ r ->
            pure (HIR.Pattern'Tuple (HIR.Type'Tuple (HIR.pattern_type l) (HIR.pattern_type r)) sp l r)

        collect_for_pat (HIR.Pattern'Named () sp at_sp bnk subpat) =
            collect_for_pat subpat >>= \ subpat ->
            let (HIR.BoundValue bn_ty _) = Arena.get bna (unlocate bnk)
            in tell [Eq InNamedPattern at_sp (Located (just_span bnk) bn_ty) (loc_pat_type subpat)] >>
            pure (HIR.Pattern'Named bn_ty sp at_sp bnk subpat)

        collect_for_pat (HIR.Pattern'Poison () sp) = HIR.Pattern'Poison <$> (HIR.Type'Variable <$> lift (new_type_variable $ PoisonPattern sp)) <*> pure sp

        collect_for_expr (HIR.Expr'Identifier () sp bn) =
            (case unlocate bn of
                Just bn -> let (HIR.BoundValue ty _) = Arena.get bna bn in pure ty
                Nothing -> HIR.Type'Variable <$> lift (new_type_variable (UnresolvedIdenExpr sp))) >>= \ ty ->

            pure (HIR.Expr'Identifier ty sp bn)

        collect_for_expr (HIR.Expr'Char () sp c) = pure (HIR.Expr'Char HIR.Type'Char sp c)
        collect_for_expr (HIR.Expr'String () sp t) = pure (HIR.Expr'String HIR.Type'String sp t)
        collect_for_expr (HIR.Expr'Int () sp i) = pure (HIR.Expr'Int HIR.Type'Int sp i)
        collect_for_expr (HIR.Expr'Float () sp r) = pure (HIR.Expr'Float HIR.Type'Float sp r)
        collect_for_expr (HIR.Expr'Bool () sp b) = pure (HIR.Expr'Bool HIR.Type'Bool sp b)

        collect_for_expr (HIR.Expr'Tuple () sp l r) = collect_for_expr l >>= \ l -> collect_for_expr r >>= \ r -> pure (HIR.Expr'Tuple (HIR.Type'Tuple (HIR.expr_type l) (HIR.expr_type r)) sp l r)

        collect_for_expr (HIR.Expr'Lambda () sp param body) =
            collect_for_pat param >>= \ param ->
            collect_for_expr body >>= \ body ->
            pure (HIR.Expr'Lambda (HIR.Type'Function (HIR.pattern_type param) (HIR.expr_type body)) sp param body)

        collect_for_expr (HIR.Expr'Let () sp result) =
            collect_for_expr result >>= \ result ->
            pure (HIR.Expr'Let (HIR.expr_type result) sp result)
        collect_for_expr (HIR.Expr'LetRec () sp result) =
            collect_for_expr result >>= \ result ->
            pure (HIR.Expr'LetRec (HIR.expr_type result) sp result)

        collect_for_expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void

        collect_for_expr (HIR.Expr'Call () sp callee arg) =
            collect_for_expr callee >>= \ callee ->
            collect_for_expr arg >>= \ arg ->
            lift (new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

            tell [Expect InCallExpr (loc_expr_type callee) (HIR.Type'Function (HIR.expr_type arg) (HIR.Type'Variable res_ty_var))] >>

            pure (HIR.Expr'Call (HIR.Type'Variable res_ty_var) sp callee arg)

        collect_for_expr (HIR.Expr'If () sp if_sp cond true false) =
            collect_for_expr cond >>= \ cond ->
            collect_for_expr true >>= \ true ->
            collect_for_expr false >>= \ false ->

            tell
                [ Expect InIfCondition (loc_expr_type cond) HIR.Type'Bool
                , Eq InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
                ] >>

            pure (HIR.Expr'If (HIR.expr_type true) sp if_sp cond true false)

        collect_for_expr (HIR.Expr'Case () sp case_tok_sp testing arms) =
            collect_for_expr testing >>= \ testing ->
            mapM (\ (p, e) -> (,) <$> collect_for_pat p <*> collect_for_expr e) arms >>= \ arms ->

            -- first expr matches all pattern types
            tell (map (\ (arm_pat, _) -> Eq InCasePatterns case_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms) >>
            -- all arm types are the same
            tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> Eq InCaseArms case_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms)) >>

            (case headMay arms of
                Just (_, first_arm_result) -> pure $ HIR.expr_type first_arm_result
                Nothing -> HIR.Type'Variable <$> lift (new_type_variable (CaseExpr sp))) >>= \ result_ty ->

            pure (HIR.Expr'Case result_ty sp case_tok_sp testing arms)

        collect_for_expr (HIR.Expr'Poison () sp) = HIR.Expr'Poison <$> (HIR.Type'Variable <$> lift (new_type_variable $ PoisonExpr sp)) <*> pure sp

        collect_for_expr (HIR.Expr'TypeAnnotation () sp annotation e) =
            lift (convert_type_expr decls annotation) >>= \ annotation ->
            collect_for_expr e >>= \ e ->
            tell [Expect InTypeAnnotation (loc_expr_type e) annotation] >> -- TODO: use annotation span
            pure (HIR.Expr'TypeAnnotation annotation sp annotation e)

solve_constraints :: TypedWithVarsNominalTypeArena -> [Constraint] -> StateWithVars ()
solve_constraints nominal_types = mapM_ solve
    where
        -- TODO: figure out how to gracefully handle errors because the type variables become ambiguous if they cant be unified
        solve :: Constraint -> StateWithVars ()
        solve (Eq in_what sp a b) =
            runExceptT (unify (unlocate a) (unlocate b)) >>= \case
                Right () -> pure ()

                Left (Left (a_part, b_part)) -> -- mismatch error
                    get >>= \ vars ->
                    lift (tell [EqError { eq_error_nominal_types = nominal_types, eq_error_vars = vars, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part }]) >>
                    pure ()

                Left (Right (var, ty)) -> -- occurs check failure
                    get >>= \ vars ->
                    lift (tell [OccursCheckError nominal_types vars sp var ty ]) >>
                    pure ()

        solve (Expect in_what got expect) =
            runExceptT (unify (unlocate got) expect) >>= \case
                Right () -> pure ()

                Left (Left (got_part, expect_part)) -> -- mismatch error
                    get >>= \ vars ->
                    lift (tell [ExpectError { expect_error_nominal_types = nominal_types, expect_error_vars = vars, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part }]) >>
                    pure ()

                Left (Right (var, ty)) -> -- occurs check failure
                    get >>= \ vars ->
                    lift (tell [OccursCheckError nominal_types vars (just_span got) var ty]) >>
                    pure ()

        unify :: TypeWithVars -> TypeWithVars -> ExceptT (Either (TypeWithVars, TypeWithVars) (TypeVarKey, TypeWithVars)) StateWithVars ()
        unify a@(HIR.Type'Nominal a_nominal_key) b@(HIR.Type'Nominal b_nominal_key) =
            -- TODO: fix bug with unifying this: synonyms will not be expanded if for example a is nominal pointing to a synonym b is not a nominal
            case (Arena.get nominal_types a_nominal_key, Arena.get nominal_types b_nominal_key) of
                (HIR.NominalType'Synonym _ a_expansion, _) -> unify a_expansion b
                (_, HIR.NominalType'Synonym _ b_expansion) -> unify a b_expansion
                (HIR.NominalType'Data _ _, HIR.NominalType'Data _ _) -> if a_nominal_key == b_nominal_key
                    then pure ()
                    else ExceptT (pure $ Left $ Left (a, b))

        unify (HIR.Type'Variable a) b = unify_var a b False
        unify a (HIR.Type'Variable b) = unify_var b a True
        unify HIR.Type'Int HIR.Type'Int = pure ()
        unify HIR.Type'Float HIR.Type'Float = pure ()
        unify HIR.Type'Char HIR.Type'Char = pure ()
        unify HIR.Type'String HIR.Type'String = pure ()
        unify HIR.Type'Bool HIR.Type'Bool = pure ()
        unify (HIR.Type'Function a1 r1) (HIR.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
        unify (HIR.Type'Tuple a1 b1) (HIR.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
        unify a b = ExceptT (pure $ Left $ Left (a, b))

        unify_var :: TypeVarKey -> TypeWithVars -> Bool -> ExceptT (Either (TypeWithVars, TypeWithVars) (TypeVarKey, TypeWithVars)) StateWithVars ()
        unify_var var other var_on_right = Arena.get <$> lift get <*> pure var >>= \ case
            -- if this variable can be expanded, unify its expansion
            TypeVar _ (Substituted var_sub) ->
                if var_on_right
                    then unify other var_sub
                    else unify var_sub other

            -- if this variable has no substitution, what happens depends on the other type
            TypeVar _ Fresh ->
                case other of
                    HIR.Type'Variable other_var ->
                        Arena.get <$> lift get <*> pure other_var >>= \case
                            -- if the other type is a substituted type variable, unify this variable with the other's expansion
                            TypeVar _ (Substituted other_var_sub) -> unify_var var other_var_sub var_on_right

                            -- if the other type is a fresh type variable, both of them are fresh type variables and the only thing that can be done is to unify them
                            TypeVar _ Fresh ->
                                if var /= other_var
                                    then lift (set_type_var_state var (Substituted other))
                                    else pure ()

                    -- if the other type is a type and not a variable
                    _ -> lift (occurs_check var other) >>= \case
                        True -> ExceptT (pure $ Left $ Right (var, other))
                        False -> lift (set_type_var_state var (Substituted other))

        set_type_var_state :: TypeVarKey -> TypeVarState -> StateWithVars ()
        set_type_var_state var new_state = modify (\ ty_arena -> Arena.modify ty_arena var (\ (TypeVar for _) -> TypeVar for new_state))

        occurs_check :: TypeVarKey -> TypeWithVars -> StateWithVars Bool
        -- does the variable v occur anywhere in the type ty?
        occurs_check v ty =
            case ty of
                HIR.Type'Variable other_v ->
                    if v == other_v
                        then pure True
                        else
                            Arena.get <$> get <*> pure other_v >>= \case
                                TypeVar _ (Substituted other_sub) -> occurs_check v other_sub
                                TypeVar _ Fresh -> pure False

                HIR.Type'Nominal nominal_key ->
                    case Arena.get nominal_types nominal_key of
                        HIR.NominalType'Synonym _ other_expansion -> occurs_check v other_expansion
                        HIR.NominalType'Data _ _ -> pure False -- TODO: check type arguemnts when those are added

                HIR.Type'Int -> pure False
                HIR.Type'Float -> pure False
                HIR.Type'Char -> pure False
                HIR.Type'String -> pure False
                HIR.Type'Bool -> pure False
                HIR.Type'Function a r -> (||) <$> occurs_check v a <*> occurs_check v r
                HIR.Type'Tuple a b -> (||) <$> occurs_check v a <*> occurs_check v b

convert_vars :: TypeVarArena -> Writer [Error] (Arena.Arena (Maybe Type) TypeVarKey)
convert_vars vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    mfix (\ vars_converted -> Arena.transformM (runMaybeT . convert_var vars_converted) vars)
    where
        r _ HIR.Type'Int = pure HIR.Type'Int
        r _ HIR.Type'Float = pure HIR.Type'Float
        r _ HIR.Type'Char = pure HIR.Type'Char
        r _ HIR.Type'String = pure HIR.Type'String
        r _ HIR.Type'Bool = pure HIR.Type'Bool
        r _ (HIR.Type'Nominal n) = pure $ HIR.Type'Nominal n
        r vars_converted (HIR.Type'Function arg res) = HIR.Type'Function <$> r vars_converted arg <*> r vars_converted res
        r vars_converted (HIR.Type'Tuple a b) = HIR.Type'Tuple <$> r vars_converted a <*> r vars_converted b
        r vars_converted (HIR.Type'Variable v) = MaybeT $ pure $ Arena.get vars_converted v

        convert_var vars_converted (TypeVar _ (Substituted s)) = r vars_converted s
        convert_var _ (TypeVar for_what Fresh) = lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)

remove_vars_from_bound_value :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBoundValue -> TypedBoundValue
remove_vars_from_bound_value vars (HIR.BoundValue ty sp) = HIR.BoundValue (remove_vars vars ty) sp

remove_vars_from_nominal_type :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsNominalType -> TypedNominalType
remove_vars_from_nominal_type vars (HIR.NominalType'Data name variants) = HIR.NominalType'Data name (map remove_from_variant variants)
    where
        remove_from_variant (HIR.DataVariant'Named name fields) = HIR.DataVariant'Named name (map (\ (name, ty) -> (name, remove_vars vars ty)) fields)
        remove_from_variant (HIR.DataVariant'Anon name fields) = HIR.DataVariant'Anon name (map (remove_vars vars) fields)
remove_vars_from_nominal_type vars (HIR.NominalType'Synonym name expansion) = HIR.NominalType'Synonym name (remove_vars vars expansion)

remove_vars_from_binding :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBinding -> TypedBinding
remove_vars_from_binding vars (HIR.Binding pat eq_sp expr) = HIR.Binding (remove_from_pat pat) eq_sp (remove_from_expr expr)
    where
        remove_from_pat (HIR.Pattern'Identifier ty sp bn) = HIR.Pattern'Identifier (remove_vars vars ty) sp bn
        remove_from_pat (HIR.Pattern'Wildcard ty sp) = HIR.Pattern'Wildcard (remove_vars vars ty) sp
        remove_from_pat (HIR.Pattern'Tuple ty sp l r) = HIR.Pattern'Tuple (remove_vars vars ty) sp (remove_from_pat l) (remove_from_pat r)
        remove_from_pat (HIR.Pattern'Named ty sp at_sp bnk subpat) = HIR.Pattern'Named (remove_vars vars ty) sp at_sp bnk (remove_from_pat subpat)
        remove_from_pat (HIR.Pattern'Poison ty sp) = HIR.Pattern'Poison (remove_vars vars ty) sp

        remove_from_expr (HIR.Expr'Identifier ty sp bn) = HIR.Expr'Identifier (remove_vars vars ty) sp bn
        remove_from_expr (HIR.Expr'Char ty sp c) = HIR.Expr'Char (remove_vars vars ty) sp c
        remove_from_expr (HIR.Expr'String ty sp t) = HIR.Expr'String (remove_vars vars ty) sp t
        remove_from_expr (HIR.Expr'Int ty sp i) = HIR.Expr'Int (remove_vars vars ty) sp i
        remove_from_expr (HIR.Expr'Float ty sp r) = HIR.Expr'Float (remove_vars vars ty) sp r
        remove_from_expr (HIR.Expr'Bool ty sp b) = HIR.Expr'Bool (remove_vars vars ty) sp b
        remove_from_expr (HIR.Expr'Tuple ty sp l r) = HIR.Expr'Tuple (remove_vars vars ty) sp (remove_from_expr l) (remove_from_expr r)
        remove_from_expr (HIR.Expr'Lambda ty sp param body) = HIR.Expr'Lambda (remove_vars vars ty) sp (remove_from_pat param) (remove_from_expr body)
        remove_from_expr (HIR.Expr'Let ty sp result) = HIR.Expr'Let (remove_vars vars ty) sp (remove_from_expr result)
        remove_from_expr (HIR.Expr'LetRec ty sp result) = HIR.Expr'LetRec (remove_vars vars ty) sp (remove_from_expr result)
        remove_from_expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void
        remove_from_expr (HIR.Expr'Call ty sp callee arg) = HIR.Expr'Call (remove_vars vars ty) sp (remove_from_expr callee) (remove_from_expr arg)
        remove_from_expr (HIR.Expr'If ty sp if_sp cond true false) = HIR.Expr'If (remove_vars vars ty) sp if_sp (remove_from_expr cond) (remove_from_expr true) (remove_from_expr false)
        remove_from_expr (HIR.Expr'Case ty sp case_sp testing arms) = HIR.Expr'Case (remove_vars vars ty) sp case_sp (remove_from_expr testing) (map (\ (p, e) -> (remove_from_pat p, remove_from_expr e)) arms)
        remove_from_expr (HIR.Expr'Poison ty sp) = HIR.Expr'Poison (remove_vars vars ty) sp
        remove_from_expr (HIR.Expr'TypeAnnotation ty sp annotation e) = HIR.Expr'TypeAnnotation (remove_vars vars ty) sp (remove_vars vars annotation) (remove_from_expr e)

remove_vars :: Arena.Arena (Maybe Type) TypeVarKey -> TypeWithVars -> Maybe Type
remove_vars vars = r
    where
        r HIR.Type'Int = pure HIR.Type'Int
        r HIR.Type'Float = pure HIR.Type'Float
        r HIR.Type'Char = pure HIR.Type'Char
        r HIR.Type'String = pure HIR.Type'String
        r HIR.Type'Bool = pure HIR.Type'Bool
        r (HIR.Type'Nominal n) = pure $ HIR.Type'Nominal n
        r (HIR.Type'Function arg res) = HIR.Type'Function <$> r arg <*> r res
        r (HIR.Type'Tuple a b) = HIR.Type'Tuple <$> r a <*> r b
        r (HIR.Type'Variable v) = Arena.get vars v
