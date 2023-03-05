module UHF.Phases.Middle.Type (typecheck) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.IO.Located (Located (..))

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.Constraint

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

type StateWithVars = StateT TypeVarArena (Compiler.WithDiagnostics Error Void)

new_type_variable :: TypeVarForWhat -> StateWithVars TypeVarKey
new_type_variable for_what =
    state $ \ type_vars ->
        Arena.put (TypeVar for_what Fresh) type_vars

-- also does type inference
typecheck :: UntypedHIR -> Compiler.WithDiagnostics Error Void TypedHIR
typecheck (HIR.HIR decls adts type_synonyms bound_values mod) =
    runStateT
        (
            Arena.transformM (convert_type_exprs_in_adts decls) adts >>= \ adts ->
            Arena.transformM (convert_type_exprs_in_type_synonyms decls) type_synonyms >>= \ type_synonyms ->
            Arena.transformM assign_type_variable_to_bound_value bound_values >>= \ bound_values ->
            runWriterT (Arena.transformM (collect_constraints decls bound_values) decls) >>= \ (decls, constraints) ->
            solve_constraints adts type_synonyms constraints >>
            pure (decls, adts, type_synonyms, bound_values)
        )
        Arena.new >>= \ ((decls, adts, type_synonyms, bound_values), vars) ->

    convert_vars vars >>= \ vars ->
    let decls' = Arena.transform (remove_vars_from_decl vars) decls
        adts' = Arena.transform (remove_vars_from_adt vars) adts
        type_synonyms' = Arena.transform (remove_vars_from_type_synonym vars) type_synonyms
        bound_values' = Arena.transform (remove_vars_from_bound_value vars) bound_values
    in
    pure (HIR.HIR decls' adts' type_synonyms' bound_values' mod)

convert_type_exprs_in_adts :: UntypedDeclArena -> UntypedADT -> StateWithVars TypedWithVarsADT
convert_type_exprs_in_adts decls (Type.ADT name variants) = Type.ADT name <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> convert_type_expr decls ty) fields
        convert_variant decls (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM (convert_type_expr decls) fields

convert_type_exprs_in_type_synonyms :: UntypedDeclArena -> UntypedTypeSynonym -> StateWithVars TypedWithVarsTypeSynonym
convert_type_exprs_in_type_synonyms decls (Type.TypeSynonym name expansion) = Type.TypeSynonym name <$> convert_type_expr decls expansion

convert_type_expr :: UntypedDeclArena -> TypeExpr -> StateWithVars TypeWithVars
convert_type_expr decls (HIR.TypeExpr'Identifier sp iden) =
    case iden of -- TODO: make poison type variable
        Just i -> case Arena.get decls i of
            HIR.Decl'Module _ _ _ _ -> lift (Compiler.tell_error $ NotAType sp "a module") >> Type.Type'Variable <$> new_type_variable (TypeExpr sp)
            HIR.Decl'Type ty -> pure $ void_var_to_key ty
        Nothing -> Type.Type'Variable <$> new_type_variable (TypeExpr sp)
    where
        -- basically useless function for converting Type Void to Type TypeVarKey
        void_var_to_key (Type.Type'ADT k) = Type.Type'ADT k
        void_var_to_key (Type.Type'Synonym k) = Type.Type'Synonym k
        void_var_to_key Type.Type'Int = Type.Type'Int
        void_var_to_key Type.Type'Float = Type.Type'Float
        void_var_to_key Type.Type'Char = Type.Type'Char
        void_var_to_key Type.Type'String = Type.Type'String
        void_var_to_key Type.Type'Bool = Type.Type'Bool
        void_var_to_key (Type.Type'Function a r) = Type.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (Type.Type'Tuple a b) = Type.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (Type.Type'Variable void) = absurd void

convert_type_expr decls (HIR.TypeExpr'Tuple a b) = Type.Type'Tuple <$> convert_type_expr decls a <*> convert_type_expr decls b
convert_type_expr _ (HIR.TypeExpr'Poison sp) = Type.Type'Variable <$> new_type_variable (TypeExpr sp)

assign_type_variable_to_bound_value :: UntypedBoundValue -> StateWithVars TypedWithVarsBoundValue
assign_type_variable_to_bound_value (HIR.BoundValue () def_span) = HIR.BoundValue <$> (Type.Type'Variable <$> new_type_variable (BoundValue def_span)) <*> pure def_span

collect_constraints :: UntypedDeclArena -> TypedWithVarsBoundValueArena -> UntypedDecl -> WriterT [Constraint] StateWithVars TypedWithVarsDecl
collect_constraints _ _ (HIR.Decl'Type ty) = pure $ HIR.Decl'Type ty
collect_constraints decls bna (HIR.Decl'Module nc bindings adts type_synonyms) = HIR.Decl'Module nc <$> mapM collect_for_binding bindings <*> pure adts <*> pure type_synonyms
    where
        collect_for_binding (HIR.Binding pat eq_sp expr) =
            collect_for_pat pat >>= \ pat ->
            collect_for_expr expr >>= \ expr ->
            tell [Eq InAssignment eq_sp (loc_pat_type pat) (loc_expr_type expr)] >>
            pure (HIR.Binding pat eq_sp expr)

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

        loc_pat_type pat = Located (HIR.pattern_span pat) (HIR.pattern_type pat)
        loc_expr_type expr = Located (HIR.expr_span expr) (HIR.expr_type expr)

        collect_for_pat (HIR.Pattern'Identifier () sp bn) =
            let (HIR.BoundValue ty _) = Arena.get bna bn
            in pure (HIR.Pattern'Identifier ty sp bn)

        collect_for_pat (HIR.Pattern'Wildcard () sp) =
            lift (Type.Type'Variable <$> new_type_variable (WildcardPattern sp)) >>= \ ty ->
            pure (HIR.Pattern'Wildcard ty sp)

        collect_for_pat (HIR.Pattern'Tuple () sp l r) =
            collect_for_pat l >>= \ l ->
            collect_for_pat r >>= \ r ->
            pure (HIR.Pattern'Tuple (Type.Type'Tuple (HIR.pattern_type l) (HIR.pattern_type r)) sp l r)

        collect_for_pat (HIR.Pattern'Named () sp at_sp bnk subpat) =
            collect_for_pat subpat >>= \ subpat ->
            let (HIR.BoundValue bn_ty _) = Arena.get bna (unlocate bnk)
            in tell [Eq InNamedPattern at_sp (Located (just_span bnk) bn_ty) (loc_pat_type subpat)] >>
            pure (HIR.Pattern'Named bn_ty sp at_sp bnk subpat)

        collect_for_pat (HIR.Pattern'Poison () sp) = HIR.Pattern'Poison <$> (Type.Type'Variable <$> lift (new_type_variable $ PoisonPattern sp)) <*> pure sp

        collect_for_expr (HIR.Expr'Identifier () sp bn) =
            (case unlocate bn of
                Just bn -> let (HIR.BoundValue ty _) = Arena.get bna bn in pure ty
                Nothing -> Type.Type'Variable <$> lift (new_type_variable (UnresolvedIdenExpr sp))) >>= \ ty ->

            pure (HIR.Expr'Identifier ty sp bn)

        collect_for_expr (HIR.Expr'Char () sp c) = pure (HIR.Expr'Char Type.Type'Char sp c)
        collect_for_expr (HIR.Expr'String () sp t) = pure (HIR.Expr'String Type.Type'String sp t)
        collect_for_expr (HIR.Expr'Int () sp i) = pure (HIR.Expr'Int Type.Type'Int sp i)
        collect_for_expr (HIR.Expr'Float () sp r) = pure (HIR.Expr'Float Type.Type'Float sp r)
        collect_for_expr (HIR.Expr'Bool () sp b) = pure (HIR.Expr'Bool Type.Type'Bool sp b)

        collect_for_expr (HIR.Expr'Tuple () sp l r) = collect_for_expr l >>= \ l -> collect_for_expr r >>= \ r -> pure (HIR.Expr'Tuple (Type.Type'Tuple (HIR.expr_type l) (HIR.expr_type r)) sp l r)

        collect_for_expr (HIR.Expr'Lambda () sp param body) =
            collect_for_pat param >>= \ param ->
            collect_for_expr body >>= \ body ->
            pure (HIR.Expr'Lambda (Type.Type'Function (HIR.pattern_type param) (HIR.expr_type body)) sp param body)

        collect_for_expr (HIR.Expr'Let () sp bindings result) =
            mapM collect_for_binding bindings >>= \ bindings ->
            collect_for_expr result >>= \ result ->
            pure (HIR.Expr'Let (HIR.expr_type result) sp bindings result)

        collect_for_expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void

        collect_for_expr (HIR.Expr'Call () sp callee arg) =
            collect_for_expr callee >>= \ callee ->
            collect_for_expr arg >>= \ arg ->
            lift (new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

            tell [Expect InCallExpr (loc_expr_type callee) (Type.Type'Function (HIR.expr_type arg) (Type.Type'Variable res_ty_var))] >>

            pure (HIR.Expr'Call (Type.Type'Variable res_ty_var) sp callee arg)

        collect_for_expr (HIR.Expr'If () sp if_sp cond true false) =
            collect_for_expr cond >>= \ cond ->
            collect_for_expr true >>= \ true ->
            collect_for_expr false >>= \ false ->

            tell
                [ Expect InIfCondition (loc_expr_type cond) Type.Type'Bool
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
                Nothing -> Type.Type'Variable <$> lift (new_type_variable (CaseExpr sp))) >>= \ result_ty ->

            pure (HIR.Expr'Case result_ty sp case_tok_sp testing arms)

        collect_for_expr (HIR.Expr'Poison () sp) = HIR.Expr'Poison <$> (Type.Type'Variable <$> lift (new_type_variable $ PoisonExpr sp)) <*> pure sp

        collect_for_expr (HIR.Expr'TypeAnnotation () sp annotation e) =
            lift (convert_type_expr decls annotation) >>= \ annotation ->
            collect_for_expr e >>= \ e ->
            tell [Expect InTypeAnnotation (loc_expr_type e) annotation] >> -- TODO: use annotation span
            pure (HIR.Expr'TypeAnnotation annotation sp annotation e)

solve_constraints :: TypedWithVarsADTArena -> TypedWithVarsTypeSynonymArena -> [Constraint] -> StateWithVars ()
solve_constraints adts type_synonyms = mapM_ solve
    where
        -- TODO: figure out how to gracefully handle errors because the type variables become ambiguous if they cant be unified
        solve :: Constraint -> StateWithVars ()
        solve (Eq in_what sp a b) =
            runExceptT (unify (unlocate a) (unlocate b)) >>= \case
                Right () -> pure ()

                Left (Left (a_part, b_part)) -> -- mismatch error
                    get >>= \ vars ->
                    lift (Compiler.tell_error $ EqError { eq_error_adts = adts, eq_error_type_synonyms = type_synonyms, eq_error_vars = vars, eq_error_in_what = in_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part }) >>
                    pure ()

                Left (Right (var, ty)) -> -- occurs check failure
                    get >>= \ vars ->
                    lift (Compiler.tell_error $ OccursCheckError adts type_synonyms vars sp var ty) >>
                    pure ()

        solve (Expect in_what got expect) =
            runExceptT (unify (unlocate got) expect) >>= \case
                Right () -> pure ()

                Left (Left (got_part, expect_part)) -> -- mismatch error
                    get >>= \ vars ->
                    lift (Compiler.tell_error $ ExpectError { expect_error_adts = adts, expect_error_type_synonyms = type_synonyms, expect_error_vars = vars, expect_error_in_what = in_what, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part }) >>
                    pure ()

                Left (Right (var, ty)) -> -- occurs check failure
                    get >>= \ vars ->
                    lift (Compiler.tell_error $ OccursCheckError adts type_synonyms vars (just_span got) var ty) >>
                    pure ()

        unify :: TypeWithVars -> TypeWithVars -> ExceptT (Either (TypeWithVars, TypeWithVars) (TypeVarKey, TypeWithVars)) StateWithVars ()
        unify a@(Type.Type'ADT a_adt_key) b@(Type.Type'ADT b_adt_key)
            | a_adt_key == b_adt_key = pure ()
            | otherwise = ExceptT (pure $ Left $ Left (a, b))

        unify (Type.Type'Synonym a_syn_key) b =
            case Arena.get type_synonyms a_syn_key of
                Type.TypeSynonym _ a_expansion -> unify a_expansion b

        unify a (Type.Type'Synonym b_syn_key) =
            case Arena.get type_synonyms b_syn_key of
                Type.TypeSynonym _ b_expansion -> unify a b_expansion

        unify (Type.Type'Variable a) b = unify_var a b False
        unify a (Type.Type'Variable b) = unify_var b a True
        unify Type.Type'Int Type.Type'Int = pure ()
        unify Type.Type'Float Type.Type'Float = pure ()
        unify Type.Type'Char Type.Type'Char = pure ()
        unify Type.Type'String Type.Type'String = pure ()
        unify Type.Type'Bool Type.Type'Bool = pure ()
        unify (Type.Type'Function a1 r1) (Type.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
        unify (Type.Type'Tuple a1 b1) (Type.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
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
                    Type.Type'Variable other_var ->
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
                Type.Type'Variable other_v ->
                    if v == other_v
                        then pure True
                        else
                            Arena.get <$> get <*> pure other_v >>= \case
                                TypeVar _ (Substituted other_sub) -> occurs_check v other_sub
                                TypeVar _ Fresh -> pure False

                Type.Type'ADT adt_key ->
                    case Arena.get adts adt_key of
                        Type.ADT _ _ -> pure False -- TODO: check type arguemnts when those are added

                Type.Type'Synonym syn_key ->
                    case Arena.get type_synonyms syn_key of
                        Type.TypeSynonym _ other_expansion -> occurs_check v other_expansion

                Type.Type'Int -> pure False
                Type.Type'Float -> pure False
                Type.Type'Char -> pure False
                Type.Type'String -> pure False
                Type.Type'Bool -> pure False
                Type.Type'Function a r -> (||) <$> occurs_check v a <*> occurs_check v r
                Type.Type'Tuple a b -> (||) <$> occurs_check v a <*> occurs_check v b

convert_vars :: TypeVarArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeVarKey)
convert_vars vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    let (res, errs) = (runWriter $ mfix (\ vars_converted -> Arena.transformM (runMaybeT . convert_var vars_converted) vars))
    in Compiler.tell_errors errs >> pure res
    where
        -- this usage of mfix does not play nicely with the IO in the Compiler monad so this must be done in the Writer monad
        r _ Type.Type'Int = pure Type.Type'Int
        r _ Type.Type'Float = pure Type.Type'Float
        r _ Type.Type'Char = pure Type.Type'Char
        r _ Type.Type'String = pure Type.Type'String
        r _ Type.Type'Bool = pure Type.Type'Bool
        r _ (Type.Type'ADT a) = pure $ Type.Type'ADT a
        r _ (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r vars_converted (Type.Type'Function arg res) = Type.Type'Function <$> r vars_converted arg <*> r vars_converted res
        r vars_converted (Type.Type'Tuple a b) = Type.Type'Tuple <$> r vars_converted a <*> r vars_converted b
        r vars_converted (Type.Type'Variable v) = MaybeT $ pure $ Arena.get vars_converted v

        convert_var vars_converted (TypeVar _ (Substituted s)) = r vars_converted s
        convert_var _ (TypeVar for_what Fresh) = lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)

remove_vars_from_decl :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsDecl -> TypedDecl
remove_vars_from_decl vars (HIR.Decl'Module nc bindings adts type_synonyms) = HIR.Decl'Module nc (map (remove_vars_from_binding vars) bindings) adts type_synonyms
remove_vars_from_decl _ (HIR.Decl'Type ty) = HIR.Decl'Type ty

remove_vars_from_bound_value :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBoundValue -> TypedBoundValue
remove_vars_from_bound_value vars (HIR.BoundValue ty sp) = HIR.BoundValue (remove_vars vars ty) sp

remove_vars_from_adt :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsADT -> TypedADT
remove_vars_from_adt vars (Type.ADT name variants) = Type.ADT name (map remove_from_variant variants)
    where
        remove_from_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name (map (\ (name, ty) -> (name, remove_vars vars ty)) fields)
        remove_from_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name (map (remove_vars vars) fields)

remove_vars_from_type_synonym :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsTypeSynonym -> TypedTypeSynonym
remove_vars_from_type_synonym vars (Type.TypeSynonym name expansion) = Type.TypeSynonym name (remove_vars vars expansion)

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
        remove_from_expr (HIR.Expr'Let ty sp bindings result) = HIR.Expr'Let (remove_vars vars ty) sp (map (remove_vars_from_binding vars) bindings) (remove_from_expr result)
        remove_from_expr (HIR.Expr'BinaryOps void _ _ _ _) = absurd void
        remove_from_expr (HIR.Expr'Call ty sp callee arg) = HIR.Expr'Call (remove_vars vars ty) sp (remove_from_expr callee) (remove_from_expr arg)
        remove_from_expr (HIR.Expr'If ty sp if_sp cond true false) = HIR.Expr'If (remove_vars vars ty) sp if_sp (remove_from_expr cond) (remove_from_expr true) (remove_from_expr false)
        remove_from_expr (HIR.Expr'Case ty sp case_sp testing arms) = HIR.Expr'Case (remove_vars vars ty) sp case_sp (remove_from_expr testing) (map (\ (p, e) -> (remove_from_pat p, remove_from_expr e)) arms)
        remove_from_expr (HIR.Expr'Poison ty sp) = HIR.Expr'Poison (remove_vars vars ty) sp
        remove_from_expr (HIR.Expr'TypeAnnotation ty sp annotation e) = HIR.Expr'TypeAnnotation (remove_vars vars ty) sp (remove_vars vars annotation) (remove_from_expr e)

remove_vars :: Arena.Arena (Maybe Type) TypeVarKey -> TypeWithVars -> Maybe Type
remove_vars vars = r
    where
        r Type.Type'Int = pure Type.Type'Int
        r Type.Type'Float = pure Type.Type'Float
        r Type.Type'Char = pure Type.Type'Char
        r Type.Type'String = pure Type.Type'String
        r Type.Type'Bool = pure Type.Type'Bool
        r (Type.Type'ADT a) = pure $ Type.Type'ADT a
        r (Type.Type'Synonym s) = pure $ Type.Type'Synonym s
        r (Type.Type'Function arg res) = Type.Type'Function <$> r arg <*> r res
        r (Type.Type'Tuple a b) = Type.Type'Tuple <$> r a <*> r b
        r (Type.Type'Variable v) = Arena.get vars v
