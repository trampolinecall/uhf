{-# LANGUAGE RecordWildCards #-}
module UHF.Type
    ( typecheck
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Location (Span, Located (Located, unlocate, just_span))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes
import qualified UHF.Diagnostic.Sections.Messages as Messages

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)

newtype TypeVarKey = TypeVarKey Int deriving (Show, Eq)
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i
type TypeVarArena = Arena.Arena TypeVar TypeVarKey
data TypeVar = TypeVar TypeVarForWhat TypeVarState
data TypeVarForWhat
    = BoundValue Span
    | UnresolvedIdenExpr Span
    | CallExpr Span
    | CaseExpr Span
    | PoisonExpr Span
    | PoisonPattern Span
    | TypeExpr Span
data TypeVarState = Fresh | Substituted TypeWithVars

type_var_for_what_sp :: TypeVarForWhat -> Span
type_var_for_what_sp (BoundValue sp) = sp
type_var_for_what_sp (UnresolvedIdenExpr sp) = sp
type_var_for_what_sp (CallExpr sp) = sp
type_var_for_what_sp (CaseExpr sp) = sp
type_var_for_what_sp (PoisonExpr sp) = sp
type_var_for_what_sp (PoisonPattern sp) = sp
type_var_for_what_sp (TypeExpr sp) = sp

type_var_for_what_name :: TypeVarForWhat -> Text
type_var_for_what_name (BoundValue _) = "binding"
type_var_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
type_var_for_what_name (CallExpr _) = "call expression"
type_var_for_what_name (CaseExpr _) = "case expression"
type_var_for_what_name (PoisonExpr _) = "expression"
type_var_for_what_name (PoisonPattern _) = "pattern"
type_var_for_what_name (TypeExpr _) = "type expression"

type TypeWithVars = IR.Type TypeVarKey
type Type = IR.Type Void

type UntypedNominalType = IR.NominalType TypeExpr
type UntypedBinding = IR.Binding (Maybe IR.BoundValueKey) TypeExpr () Void
type UntypedExpr = IR.Expr (Maybe IR.BoundValueKey) TypeExpr () Void
type UntypedPattern = IR.Pattern (Maybe IR.BoundValueKey) ()
type UntypedBoundValue = IR.BoundValue ()

type UntypedBindingArena = Arena.Arena UntypedBinding IR.BindingKey
type UntypedNominalTypeArena = Arena.Arena UntypedNominalType IR.NominalTypeKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue IR.BoundValueKey

type TypedWithVarsNominalType = IR.NominalType TypeWithVars
type TypedWithVarsBinding = IR.Binding (Maybe IR.BoundValueKey) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = IR.Expr (Maybe IR.BoundValueKey) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = IR.Pattern (Maybe IR.BoundValueKey) TypeWithVars
type TypedWithVarsBoundValue = IR.BoundValue TypeWithVars

type TypedWithVarsBindingArena = Arena.Arena TypedWithVarsBinding IR.BindingKey
type TypedWithVarsNominalTypeArena = Arena.Arena TypedWithVarsNominalType IR.NominalTypeKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue IR.BoundValueKey

type TypedNominalType = IR.NominalType (Maybe Type)
type TypedBinding = IR.Binding (Maybe IR.BoundValueKey) (Maybe Type) (Maybe Type) Void
type TypedExpr = IR.Expr (Maybe IR.BoundValueKey) (Maybe Type) (Maybe Type) Void
type TypedPattern = IR.Pattern (Maybe IR.BoundValueKey) (Maybe Type)
type TypedBoundValue = IR.BoundValue (Maybe Type)

type TypedBindingArena = Arena.Arena TypedBinding IR.BindingKey
type TypedNominalTypeArena = Arena.Arena TypedNominalType IR.NominalTypeKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue IR.BoundValueKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data EqInWhat = InAssignment | InNamedPattern | InIfBranches | InCasePatterns | InCaseArms
data ExpectInWhat = InTypeAnnotation | InCallExpr | InIfCondition
data Constraint
    = Eq EqInWhat Span (Located TypeWithVars) (Located TypeWithVars)
    | Expect ExpectInWhat (Located TypeWithVars) TypeWithVars

data Error
    = EqError
        { eq_error_nominal_types :: TypedWithVarsNominalTypeArena
        , eq_error_vars :: TypeVarArena
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithVars
        , eq_error_b_whole :: Located TypeWithVars
        , eq_error_a_part :: TypeWithVars
        , eq_error_b_part :: TypeWithVars
        }
    | ExpectError
        { expect_error_nominal_types :: TypedWithVarsNominalTypeArena
        , expect_error_vars :: TypeVarArena
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithVars
        , expect_error_expect_whole :: TypeWithVars
        , expect_error_got_part :: TypeWithVars
        , expect_error_expect_part :: TypeWithVars
        }

    | OccursCheckError TypedWithVarsNominalTypeArena TypeVarArena Span TypeVarKey TypeWithVars

    | AmbiguousType TypeVarForWhat

instance Diagnostic.IsError Error where
    to_error (EqError nominal_types vars in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InCasePatterns -> "'case' expression patterns"
                InCaseArms -> "'case' expression arms"

        in Diagnostic.Error Diagnostic.Codes.type_mismatch $
            Diagnostic.DiagnosticContents
                (Just span)
                ("conflicting types in " <> what <> ": '" <> print_type False nominal_types vars a_part <> "' vs '" <> print_type False nominal_types vars b_part <> "'")
                [ just_span a_whole `Messages.note` convert_str (print_type False nominal_types vars $ unlocate a_whole)
                , just_span b_whole `Messages.note` convert_str (print_type False nominal_types vars $ unlocate b_whole)
                ]
                []

    to_error (ExpectError nominal_types vars in_what got_whole expect_whole got_part expect_part) =
        let what = case in_what of
                InTypeAnnotation -> "type annotation"
                InIfCondition -> "'if' condition"
                InCallExpr -> "call expression"

            sp = just_span got_whole

        in Diagnostic.Error Diagnostic.Codes.type_mismatch $ -- TODO: change code?
            Diagnostic.DiagnosticContents
                (Just sp)
                (convert_str $ "conflicting types in " <> what <> ": '" <> print_type False nominal_types vars expect_part <> "' vs '" <> print_type False nominal_types vars got_part <> "'")
                [ sp `Messages.note` convert_str ("expected '" <> print_type False nominal_types vars expect_whole <> "', got '" <> print_type False nominal_types vars (unlocate got_whole) <> "'") ]
                []

    to_error (OccursCheckError nominal_types vars span var_key ty) =
        let var_as_type = IR.Type'Variable var_key
            (TypeVar var_for_what _) = Arena.get vars var_key

            var_sp = type_var_for_what_sp var_for_what
            var_name = type_var_for_what_name var_for_what
            var_printed = print_type True nominal_types vars var_as_type

        in Diagnostic.Error Diagnostic.Codes.occurs_check $
            Diagnostic.DiagnosticContents
                (Just span)
                ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> "' = '" <> print_type True nominal_types vars ty <> "'")
                [ var_sp `Messages.note` (convert_str $ "where " <> var_printed <> " is the type of this " <> var_name)]
                []

    to_error (AmbiguousType for_what) =
        let sp = type_var_for_what_sp for_what
            name = type_var_for_what_name for_what
        in Diagnostic.Error Diagnostic.Codes.ambiguous_type $
            Diagnostic.DiagnosticContents
                (Just sp) -- TODO
                ("ambiguous type: could not infer the type of this " <> name) -- TODO: better message
                []
                []

print_type :: Bool -> TypedWithVarsNominalTypeArena -> TypeVarArena -> TypeWithVars -> Text
-- TODO: construct an ast and print it
print_type _ nominals _ (IR.Type'Nominal key) =
    case Arena.get nominals key of
        IR.NominalType'Data name _ -> name
        IR.NominalType'Synonym name _ -> name
print_type _ _ _ (IR.Type'Int) = "int"
print_type _ _ _ (IR.Type'Float) = "float"
print_type _ _ _ (IR.Type'Char) = "char"
print_type _ _ _ (IR.Type'String) = "string"
print_type _ _ _ (IR.Type'Bool) = "bool"
print_type vars_show_index nominals vars (IR.Type'Function a r) = print_type vars_show_index nominals vars a <> " -> " <> print_type vars_show_index nominals vars r -- TODO: parentheses and grouping
print_type vars_show_index nominals vars (IR.Type'Tuple a b) = "(" <> print_type vars_show_index nominals vars a <> ", " <> print_type vars_show_index nominals vars b <> ")"
print_type vars_show_index nominals vars (IR.Type'Variable var) =
    case Arena.get vars var of
        TypeVar _ Fresh
            | vars_show_index -> "<unknown " <> show (Arena.unmake_key var) <> ">"
            | otherwise -> "_"
        TypeVar _ (Substituted other) -> print_type vars_show_index nominals vars other

type StateWithVars = StateT TypeVarArena (Writer [Error])

new_type_variable :: TypeVarForWhat -> StateWithVars TypeVarKey
new_type_variable for_what =
    state $ \ type_vars ->
        Arena.put (TypeVar for_what Fresh) type_vars

-- also does type inference
typecheck :: (DeclArena, UntypedNominalTypeArena, UntypedBindingArena, UntypedBoundValueArena) -> Writer [Error] (DeclArena, TypedNominalTypeArena, TypedBindingArena, TypedBoundValueArena)
typecheck (decls, nominal_types, bindings, bound_values) =
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

convert_type_exprs_in_nominal_types :: DeclArena -> UntypedNominalType -> StateWithVars TypedWithVarsNominalType
convert_type_exprs_in_nominal_types decls (IR.NominalType'Data name variants) = IR.NominalType'Data name <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> convert_type_expr decls ty) fields
        convert_variant decls (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> mapM (convert_type_expr decls) fields

convert_type_exprs_in_nominal_types decls (IR.NominalType'Synonym name expansion) = IR.NominalType'Synonym name <$> convert_type_expr decls expansion

convert_type_expr :: DeclArena -> TypeExpr -> StateWithVars TypeWithVars
convert_type_expr decls (IR.TypeExpr'Identifier sp iden) = case iden of -- TODO: make poison type variable
    Just i -> case Arena.get decls i of
        IR.Decl'Module _ -> IR.Type'Variable <$> new_type_variable (TypeExpr sp) -- TODO: report error for this
        IR.Decl'Type ty -> pure $ void_var_to_key ty
    Nothing -> IR.Type'Variable <$> new_type_variable (TypeExpr sp)
    where
        -- basically useless function for converting Type Void to Type(TypeVarKey
        void_var_to_key (IR.Type'Nominal k) = IR.Type'Nominal k
        void_var_to_key (IR.Type'Int) = IR.Type'Int
        void_var_to_key (IR.Type'Float) = IR.Type'Float
        void_var_to_key (IR.Type'Char) = IR.Type'Char
        void_var_to_key (IR.Type'String) = IR.Type'String
        void_var_to_key (IR.Type'Bool) = IR.Type'Bool
        void_var_to_key (IR.Type'Function a r) = IR.Type'Function (void_var_to_key a) (void_var_to_key r)
        void_var_to_key (IR.Type'Tuple a b) = IR.Type'Tuple (void_var_to_key a) (void_var_to_key b)
        void_var_to_key (IR.Type'Variable void) = absurd void

convert_type_expr decls (IR.TypeExpr'Tuple a b) = IR.Type'Tuple <$> convert_type_expr decls a <*> convert_type_expr decls b
convert_type_expr _ (IR.TypeExpr'Poison sp) = IR.Type'Variable <$> new_type_variable (TypeExpr sp)

assign_type_variable_to_bound_value :: UntypedBoundValue -> StateWithVars TypedWithVarsBoundValue
assign_type_variable_to_bound_value (IR.BoundValue () def_span) = IR.BoundValue <$> (IR.Type'Variable <$> new_type_variable (BoundValue def_span)) <*> pure def_span

collect_constraints :: DeclArena -> TypedWithVarsBoundValueArena -> UntypedBinding -> WriterT [Constraint] StateWithVars TypedWithVarsBinding
collect_constraints decls bna (IR.Binding pat eq_sp expr) =
    collect_for_pat pat >>= \ pat ->
    collect_for_expr expr >>= \ expr ->
    tell [Eq InAssignment eq_sp (loc_pat_type pat) (loc_expr_type expr)] >>
    pure (IR.Binding pat eq_sp expr)
    where
        loc_pat_type pat = Located (IR.pattern_span pat) (IR.pattern_type pat)
        loc_expr_type expr = Located (IR.expr_span expr) (IR.expr_type expr)

        collect_for_pat (IR.Pattern'Identifier () sp bn) =
            let (IR.BoundValue ty _) = Arena.get bna bn
            in pure (IR.Pattern'Identifier ty sp bn)

        collect_for_pat (IR.Pattern'Tuple () sp l r) =
            collect_for_pat l >>= \ l ->
            collect_for_pat r >>= \ r ->
            pure (IR.Pattern'Tuple (IR.Type'Tuple (IR.pattern_type l) (IR.pattern_type r)) sp l r)

        collect_for_pat (IR.Pattern'Named () sp at_sp bnk subpat) =
            collect_for_pat subpat >>= \ subpat ->
            let (IR.BoundValue bn_ty _) = Arena.get bna (unlocate bnk)
            in tell [Eq InNamedPattern at_sp (Located (just_span bnk) bn_ty) (loc_pat_type subpat)] >>
            pure (IR.Pattern'Named bn_ty sp at_sp bnk subpat)

        collect_for_pat (IR.Pattern'Poison () sp) = IR.Pattern'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonPattern sp)) <*> pure sp

        collect_for_expr (IR.Expr'Identifier () sp bn) =
            (case bn of
                Just bn -> let (IR.BoundValue ty _) = Arena.get bna bn in pure ty
                Nothing -> IR.Type'Variable <$> lift (new_type_variable (UnresolvedIdenExpr sp))) >>= \ ty ->

            pure (IR.Expr'Identifier ty sp bn)

        collect_for_expr (IR.Expr'Char () sp c) = pure (IR.Expr'Char IR.Type'Char sp c)
        collect_for_expr (IR.Expr'String () sp t) = pure (IR.Expr'String IR.Type'String sp t)
        collect_for_expr (IR.Expr'Int () sp i) = pure (IR.Expr'Int IR.Type'Int sp i)
        collect_for_expr (IR.Expr'Float () sp r) = pure (IR.Expr'Float IR.Type'Float sp r)
        collect_for_expr (IR.Expr'Bool () sp b) = pure (IR.Expr'Bool IR.Type'Bool sp b)

        collect_for_expr (IR.Expr'Tuple () sp l r) = collect_for_expr l >>= \ l -> collect_for_expr r >>= \ r -> pure (IR.Expr'Tuple (IR.Type'Tuple (IR.expr_type l) (IR.expr_type r)) sp l r)

        collect_for_expr (IR.Expr'Lambda () sp param body) =
            collect_for_pat param >>= \ param ->
            collect_for_expr body >>= \ body ->
            pure (IR.Expr'Lambda (IR.Type'Function (IR.pattern_type param) (IR.expr_type body)) sp param body)

        collect_for_expr (IR.Expr'Let () sp result) =
            collect_for_expr result >>= \ result ->
            pure (IR.Expr'Let (IR.expr_type result) sp result)
        collect_for_expr (IR.Expr'LetRec () sp result) =
            collect_for_expr result >>= \ result ->
            pure (IR.Expr'LetRec (IR.expr_type result) sp result)

        collect_for_expr (IR.Expr'BinaryOps void _ _ _ _) = absurd void

        collect_for_expr (IR.Expr'Call () sp callee arg) =
            collect_for_expr callee >>= \ callee ->
            collect_for_expr arg >>= \ arg ->
            lift (new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

            tell [Expect InCallExpr (loc_expr_type callee) (IR.Type'Function (IR.expr_type arg) (IR.Type'Variable res_ty_var))] >>

            pure (IR.Expr'Call (IR.Type'Variable res_ty_var) sp callee arg)

        collect_for_expr (IR.Expr'If () sp if_sp cond true false) =
            collect_for_expr cond >>= \ cond ->
            collect_for_expr true >>= \ true ->
            collect_for_expr false >>= \ false ->

            tell
                [ Expect InIfCondition (loc_expr_type cond) IR.Type'Bool
                , Eq InIfBranches if_sp (loc_expr_type true) (loc_expr_type false)
                ] >>

            pure (IR.Expr'If (IR.expr_type true) sp if_sp cond true false)

        collect_for_expr (IR.Expr'Case () sp case_tok_sp testing arms) =
            collect_for_expr testing >>= \ testing ->
            mapM (\ (p, e) -> (,) <$> collect_for_pat p <*> collect_for_expr e) arms >>= \ arms ->

            -- first expr matches all pattern types
            tell (map (\ (arm_pat, _) -> Eq InCasePatterns case_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms) >>
            -- all arm types are the same
            tell (zipWith (\ (_, arm_result_1) (_, arm_result_2) -> Eq InCaseArms case_tok_sp (loc_expr_type arm_result_1) (loc_expr_type arm_result_2)) arms (drop 1 arms)) >>

            (case headMay arms of
                Just (_, first_arm_result) -> pure $ IR.expr_type first_arm_result
                Nothing -> IR.Type'Variable <$> lift (new_type_variable (CaseExpr sp))) >>= \ result_ty ->

            pure (IR.Expr'Case result_ty sp case_tok_sp testing arms)

        collect_for_expr (IR.Expr'Poison () sp) = IR.Expr'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonExpr sp)) <*> pure sp

        collect_for_expr (IR.Expr'TypeAnnotation () sp annotation e) =
            lift (convert_type_expr decls annotation) >>= \ annotation ->
            collect_for_expr e >>= \ e ->
            tell [Expect InTypeAnnotation (loc_expr_type e) annotation] >> -- TODO: use annotation span
            pure (IR.Expr'TypeAnnotation annotation sp annotation e)

solve_constraints :: TypedWithVarsNominalTypeArena -> [Constraint] -> StateWithVars ()
solve_constraints nominal_types = mapM_ solve
    where
        -- TODO: gracefully figure out how to handle errors because the type variables become ambiguous if they cant be unified
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
        unify a@(IR.Type'Nominal a_nominal_key) b@(IR.Type'Nominal b_nominal_key) =
            -- TODO: fix bug with unifying this: synonyms will not be expanded if for example a is nominal pointing to a synonym b is not a nominal
            case (Arena.get nominal_types a_nominal_key, Arena.get nominal_types b_nominal_key) of
                (IR.NominalType'Synonym _ a_expansion, _) -> unify a_expansion b
                (_, IR.NominalType'Synonym _ b_expansion) -> unify a b_expansion
                (IR.NominalType'Data _ _, IR.NominalType'Data _ _) -> if a_nominal_key == b_nominal_key
                    then pure ()
                    else ExceptT (pure $ Left $ Left (a, b))

        unify (IR.Type'Variable a) b = unify_var a b False
        unify a (IR.Type'Variable b) = unify_var b a True
        unify (IR.Type'Int) (IR.Type'Int) = pure ()
        unify (IR.Type'Float) (IR.Type'Float) = pure ()
        unify (IR.Type'Char) (IR.Type'Char) = pure ()
        unify (IR.Type'String) (IR.Type'String) = pure ()
        unify (IR.Type'Bool) (IR.Type'Bool) = pure ()
        unify (IR.Type'Function a1 r1) (IR.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
        unify (IR.Type'Tuple a1 b1) (IR.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
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
                    IR.Type'Variable other_var ->
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
                IR.Type'Variable other_v ->
                    if v == other_v
                        then pure True
                        else
                            Arena.get <$> get <*> pure other_v >>= \case
                                TypeVar _ (Substituted other_sub) -> occurs_check v other_sub
                                TypeVar _ Fresh -> pure False

                IR.Type'Nominal nominal_key ->
                    case Arena.get nominal_types nominal_key of
                        IR.NominalType'Synonym _ other_expansion -> occurs_check v other_expansion
                        IR.NominalType'Data _ _ -> pure False -- TODO: check type arguemnts when those are added

                IR.Type'Int -> pure False
                IR.Type'Float -> pure False
                IR.Type'Char -> pure False
                IR.Type'String -> pure False
                IR.Type'Bool -> pure False
                IR.Type'Function a r -> (||) <$> occurs_check v a <*> occurs_check v r
                IR.Type'Tuple a b -> (||) <$> occurs_check v a <*> occurs_check v b

convert_vars :: TypeVarArena -> Writer [Error] (Arena.Arena (Maybe Type) TypeVarKey)
convert_vars vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    mfix (\ vars_converted -> Arena.transformM (runMaybeT . convert_var vars_converted) vars)
    where
        r _ (IR.Type'Int) = pure $ IR.Type'Int
        r _ (IR.Type'Float) = pure $ IR.Type'Float
        r _ (IR.Type'Char) = pure $ IR.Type'Char
        r _ (IR.Type'String) = pure $ IR.Type'String
        r _ (IR.Type'Bool) = pure $ IR.Type'Bool
        r _ (IR.Type'Nominal n) = pure $ IR.Type'Nominal n
        r vars_converted (IR.Type'Function arg res) = IR.Type'Function <$> r vars_converted arg <*> r vars_converted res
        r vars_converted (IR.Type'Tuple a b) = IR.Type'Tuple <$> r vars_converted a <*> r vars_converted b
        r vars_converted (IR.Type'Variable v) = MaybeT $ pure $ Arena.get vars_converted v

        convert_var vars_converted (TypeVar _ (Substituted s)) = r vars_converted s
        convert_var _ (TypeVar for_what Fresh) = lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)

remove_vars_from_bound_value :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBoundValue -> TypedBoundValue
remove_vars_from_bound_value vars (IR.BoundValue ty sp) = IR.BoundValue (remove_vars vars ty) sp

remove_vars_from_nominal_type :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsNominalType -> TypedNominalType
remove_vars_from_nominal_type vars (IR.NominalType'Data name variants) = IR.NominalType'Data name (map remove_from_variant variants)
    where
        remove_from_variant (IR.DataVariant'Named name fields) = IR.DataVariant'Named name (map (\ (name, ty) -> (name, remove_vars vars ty)) fields)
        remove_from_variant (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name (map (remove_vars vars) fields)
remove_vars_from_nominal_type vars (IR.NominalType'Synonym name expansion) = IR.NominalType'Synonym name (remove_vars vars expansion)

remove_vars_from_binding :: Arena.Arena (Maybe Type) TypeVarKey -> TypedWithVarsBinding -> TypedBinding
remove_vars_from_binding vars (IR.Binding pat eq_sp expr) = IR.Binding (remove_from_pat pat) eq_sp (remove_from_expr expr)
    where
        remove_from_pat (IR.Pattern'Identifier ty sp bn) = IR.Pattern'Identifier (remove_vars vars ty) sp bn
        remove_from_pat (IR.Pattern'Tuple ty sp l r) = IR.Pattern'Tuple (remove_vars vars ty) sp (remove_from_pat l) (remove_from_pat r)
        remove_from_pat (IR.Pattern'Named ty sp at_sp bnk subpat) = IR.Pattern'Named (remove_vars vars ty) sp at_sp bnk (remove_from_pat subpat)
        remove_from_pat (IR.Pattern'Poison ty sp) = IR.Pattern'Poison (remove_vars vars ty) sp

        remove_from_expr (IR.Expr'Identifier ty sp bn) = IR.Expr'Identifier (remove_vars vars ty) sp bn
        remove_from_expr (IR.Expr'Char ty sp c) = IR.Expr'Char (remove_vars vars ty) sp c
        remove_from_expr (IR.Expr'String ty sp t) = IR.Expr'String (remove_vars vars ty) sp t
        remove_from_expr (IR.Expr'Int ty sp i) = IR.Expr'Int (remove_vars vars ty) sp i
        remove_from_expr (IR.Expr'Float ty sp r) = IR.Expr'Float (remove_vars vars ty) sp r
        remove_from_expr (IR.Expr'Bool ty sp b) = IR.Expr'Bool (remove_vars vars ty) sp b
        remove_from_expr (IR.Expr'Tuple ty sp l r) = IR.Expr'Tuple (remove_vars vars ty) sp (remove_from_expr l) (remove_from_expr r)
        remove_from_expr (IR.Expr'Lambda ty sp param body) = IR.Expr'Lambda (remove_vars vars ty) sp (remove_from_pat param) (remove_from_expr body)
        remove_from_expr (IR.Expr'Let ty sp result) = IR.Expr'Let (remove_vars vars ty) sp (remove_from_expr result)
        remove_from_expr (IR.Expr'LetRec ty sp result) = IR.Expr'LetRec (remove_vars vars ty) sp (remove_from_expr result)
        remove_from_expr (IR.Expr'BinaryOps void _ _ _ _) = absurd void
        remove_from_expr (IR.Expr'Call ty sp callee arg) = IR.Expr'Call (remove_vars vars ty) sp (remove_from_expr callee) (remove_from_expr arg)
        remove_from_expr (IR.Expr'If ty sp if_sp cond true false) = IR.Expr'If (remove_vars vars ty) sp if_sp (remove_from_expr cond) (remove_from_expr true) (remove_from_expr false)
        remove_from_expr (IR.Expr'Case ty sp case_sp testing arms) = IR.Expr'Case (remove_vars vars ty) sp case_sp (remove_from_expr testing) (map (\ (p, e) -> (remove_from_pat p, remove_from_expr e)) arms)
        remove_from_expr (IR.Expr'Poison ty sp) = IR.Expr'Poison (remove_vars vars ty) sp
        remove_from_expr (IR.Expr'TypeAnnotation ty sp annotation e) = IR.Expr'TypeAnnotation (remove_vars vars ty) sp (remove_vars vars annotation) (remove_from_expr e)

remove_vars :: Arena.Arena (Maybe Type) TypeVarKey -> TypeWithVars -> Maybe Type
remove_vars vars ty = r ty
    where
        r (IR.Type'Int) = pure $ IR.Type'Int
        r (IR.Type'Float) = pure $ IR.Type'Float
        r (IR.Type'Char) = pure $ IR.Type'Char
        r (IR.Type'String) = pure $ IR.Type'String
        r (IR.Type'Bool) = pure $ IR.Type'Bool
        r (IR.Type'Nominal n) = pure $ IR.Type'Nominal n
        r (IR.Type'Function arg res) = IR.Type'Function <$> r arg <*> r res
        r (IR.Type'Tuple a b) = IR.Type'Tuple <$> r a <*> r b
        r (IR.Type'Variable v) = Arena.get vars v
