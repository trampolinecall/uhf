{-# LANGUAGE RecordWildCards #-}
module UHF.Type
    ( typecheck
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Location (Span, Located)

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)

newtype TypeVarKey = TypeVarKey Int
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i
type TypeVarArena = Arena.Arena TypeVar TypeVarKey
data TypeVar = TypeVar TypeVarForWhat TypeVarState
data TypeVarForWhat
    = BoundName Span
    | UnresolvedIdenExpr Span
    | CallExpr Span
    | CaseExpr Span
    | PoisonExpr Span
    | PoisonPattern Span
    | TypeExpr Span
data TypeVarState = Fresh | Substituted TypeWithVars

type TypeWithVars = IR.Type TypeVarKey
type Type = IR.Type Void

type UntypedNominalType = IR.NominalType TypeExpr
type UntypedBinding = IR.Binding (Maybe IR.BoundNameKey) TypeExpr ()
type UntypedExpr = IR.Expr (Maybe IR.BoundNameKey) TypeExpr ()
type UntypedPattern = IR.Pattern (Maybe IR.BoundNameKey) ()
type UntypedBoundName = IR.BoundName ()

type UntypedBindingArena = Arena.Arena UntypedBinding IR.BindingKey
type UntypedNominalTypeArena = Arena.Arena UntypedNominalType IR.NominalTypeKey
type UntypedBoundNameArena = Arena.Arena UntypedBoundName IR.BoundNameKey

type TypedWithVarsNominalType = IR.NominalType TypeWithVars
type TypedWithVarsBinding = IR.Binding (Maybe IR.BoundNameKey) TypeWithVars TypeWithVars
type TypedWithVarsExpr = IR.Expr (Maybe IR.BoundNameKey) TypeWithVars TypeWithVars
type TypedWithVarsPattern = IR.Pattern (Maybe IR.BoundNameKey) TypeWithVars
type TypedWithVarsBoundName = IR.BoundName TypeWithVars

type TypedWithVarsBindingArena = Arena.Arena TypedWithVarsBinding IR.BindingKey
type TypedWithVarsNominalTypeArena = Arena.Arena TypedWithVarsNominalType IR.NominalTypeKey
type TypedWithVarsBoundNameArena = Arena.Arena TypedWithVarsBoundName IR.BoundNameKey

type TypedNominalType = IR.NominalType Type
type TypedBinding = IR.Binding (Maybe IR.BoundNameKey) Type Type
type TypedExpr = IR.Expr (Maybe IR.BoundNameKey) Type Type
type TypedPattern = IR.Pattern (Maybe IR.BoundNameKey) Type
type TypedBoundName = IR.BoundName Type

type TypedBindingArena = Arena.Arena TypedBinding IR.BindingKey
type TypedNominalTypeArena = Arena.Arena TypedNominalType IR.NominalTypeKey
type TypedBoundNameArena = Arena.Arena TypedBoundName IR.BoundNameKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data Constraint = Eq TypeWithVars TypeWithVars

data Error
    = EqError
        { eq_error_a_whole :: TypeWithVars
        , eq_error_b_whole :: TypeWithVars
        , eq_error_a_part :: TypeWithVars
        , eq_error_b_part :: TypeWithVars
        } -- TODO: add spans
instance Diagnostic.IsError Error where
    to_error (EqError {..}) =
        Diagnostic.Error Diagnostic.Codes.type_mismatch $ todo
            -- Diagnostic.DiagnosticContents
                -- (Just sp)
                -- [Underlines.underlines [sp `Underlines.primary` []]]

type StateWithVars = StateT TypeVarArena (Writer [Error])

new_type_variable :: TypeVarForWhat -> StateWithVars TypeVarKey
new_type_variable for_what =
    state $ \ type_vars ->
        Arena.put (TypeVar for_what Fresh) type_vars

-- also does type inference
typecheck :: (DeclArena, UntypedNominalTypeArena, UntypedBindingArena, UntypedBoundNameArena) -> Writer [Error] (DeclArena, TypedNominalTypeArena, TypedBindingArena, TypedBoundNameArena)
typecheck (decls, nominal_types, bindings, bound_names) =
    runStateT
        (
            Arena.transformM (convert_type_exprs_in_nominal_types decls) nominal_types >>= \ nominal_types ->
            Arena.transformM assign_type_variable_to_bound_name bound_names >>= \ bound_names ->
            runWriterT (Arena.transformM (collect_constraints decls bound_names) bindings) >>= \ (bindings, constraints) ->
            solve_constraints constraints >>
            pure (nominal_types, bound_names, bindings)
        )
        Arena.new >>= \ ((nominal_types, bound_names, bindings), vars) ->

    Arena.transformM (apply_type_vars_to_nominal_type vars) nominal_types >>= \ nominal_types ->
    Arena.transformM (apply_type_vars_to_bound_name vars) bound_names >>= \ bound_names ->
    Arena.transformM (apply_type_vars_to_binding vars) bindings >>= \ bindings ->

    pure (decls, nominal_types, bindings, bound_names)

convert_type_exprs_in_nominal_types :: DeclArena -> UntypedNominalType -> StateWithVars TypedWithVarsNominalType
convert_type_exprs_in_nominal_types decls (IR.NominalType'Data variants) = IR.NominalType'Data <$> mapM (convert_variant decls) variants
    where
        convert_variant decls (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> convert_type_expr decls ty) fields
        convert_variant decls (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> mapM (convert_type_expr decls) fields

convert_type_exprs_in_nominal_types decls (IR.NominalType'Synonym expansion) = IR.NominalType'Synonym <$> convert_type_expr decls expansion

convert_type_expr :: DeclArena -> TypeExpr -> StateWithVars TypeWithVars
convert_type_expr decls (IR.TypeExpr'Identifier iden) = case iden of
    Just i -> case Arena.get decls i of
        IR.Decl'Module _ -> IR.Type'Variable <$> new_type_variable (TypeExpr todo) -- TODO: report error for this
        IR.Decl'Type ty -> pure $ IR.Type'Nominal ty
    Nothing -> IR.Type'Variable <$> new_type_variable (TypeExpr todo)
convert_type_expr decls (IR.TypeExpr'Tuple items) = IR.Type'Tuple <$> todo <*> todo -- mapM (convert_type_expr decls) items

assign_type_variable_to_bound_name :: UntypedBoundName -> StateWithVars TypedWithVarsBoundName
assign_type_variable_to_bound_name (IR.BoundName ()) = IR.BoundName <$> (IR.Type'Variable <$> new_type_variable (BoundName todo))

collect_constraints :: DeclArena -> TypedWithVarsBoundNameArena -> UntypedBinding -> WriterT [Constraint] StateWithVars TypedWithVarsBinding
collect_constraints decls bna (IR.Binding pat expr) =
    collect_for_pat pat >>= \ pat ->
    collect_for_expr expr >>= \ expr ->
    tell [Eq (IR.pattern_type pat) (IR.expr_type expr)] >>
    pure (IR.Binding pat expr)
    where
        collect_for_pat (IR.Pattern'Identifier () bn) =
            let (IR.BoundName ty) = Arena.get bna bn
            in pure (IR.Pattern'Identifier ty bn)

        collect_for_pat (IR.Pattern'Tuple () l r) =
            collect_for_pat l >>= \ l ->
            collect_for_pat r >>= \ r ->
            pure (IR.Pattern'Tuple (IR.Type'Tuple (IR.pattern_type l) (IR.pattern_type r)) l r)

        collect_for_pat (IR.Pattern'Named () bnk subpat) =
            collect_for_pat subpat >>= \ subpat ->
            let (IR.BoundName bn_ty) = Arena.get bna bnk
            in tell [Eq bn_ty (IR.pattern_type subpat)] >>
            pure (IR.Pattern'Named bn_ty bnk subpat)

        collect_for_pat (IR.Pattern'Poison () sp) = IR.Pattern'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonPattern sp)) <*> pure sp

        collect_for_expr (IR.Expr'Identifier () sp bn) =
            (case bn of
                Just bn -> let (IR.BoundName ty) = Arena.get bna bn in pure ty
                Nothing -> IR.Type'Variable <$> lift (new_type_variable (UnresolvedIdenExpr sp))) >>= \ ty ->

            pure (IR.Expr'Identifier ty sp bn)

        collect_for_expr (IR.Expr'Char () c) = pure (IR.Expr'Char IR.Type'Char c)
        collect_for_expr (IR.Expr'String () t) = pure (IR.Expr'String IR.Type'String t)
        collect_for_expr (IR.Expr'Int () i) = pure (IR.Expr'Int IR.Type'Int i)
        collect_for_expr (IR.Expr'Float () r) = pure (IR.Expr'Float IR.Type'Float r)
        collect_for_expr (IR.Expr'Bool () b) = pure (IR.Expr'Bool IR.Type'Bool b)

        collect_for_expr (IR.Expr'Tuple () l r) = collect_for_expr l >>= \ l -> collect_for_expr r >>= \ r -> pure (IR.Expr'Tuple (IR.Type'Tuple (IR.expr_type l) (IR.expr_type r)) l r)

        collect_for_expr (IR.Expr'Lambda () param body) =
            collect_for_pat param >>= \ param ->
            collect_for_expr body >>= \ body ->
            pure (IR.Expr'Lambda (IR.Type'Function (IR.pattern_type param) (IR.expr_type body)) param body)

        collect_for_expr (IR.Expr'Let () result) =
            collect_for_expr result >>= \ result ->
            pure (IR.Expr'Let (IR.expr_type result) result)
        collect_for_expr (IR.Expr'LetRec () result) =
            collect_for_expr result >>= \ result ->
            pure (IR.Expr'LetRec (IR.expr_type result) result)

        collect_for_expr (IR.Expr'BinaryOps () first ops) = todo -- TODO: group these before this stage so that this does not exist

        collect_for_expr (IR.Expr'Call () sp callee arg) =
            collect_for_expr callee >>= \ callee ->
            collect_for_expr arg >>= \ arg ->
            lift (new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

            tell [Eq (IR.expr_type callee) (IR.Type'Function (IR.expr_type arg) (IR.Type'Variable res_ty_var))] >>

            pure (IR.Expr'Call (IR.Type'Variable res_ty_var) sp callee arg)

        collect_for_expr (IR.Expr'If () cond true false) =
            collect_for_expr cond >>= \ cond ->
            collect_for_expr true >>= \ true ->
            collect_for_expr false >>= \ false ->

            tell
                [ Eq (IR.expr_type cond) (IR.Type'Bool)
                , Eq (IR.expr_type true) (IR.expr_type false)
                ] >>

            pure (IR.Expr'If (IR.expr_type true) cond true false)

        collect_for_expr (IR.Expr'Case () sp testing arms) =
            collect_for_expr testing >>= \ testing ->
            mapM (\ (p, e) -> (,) <$> collect_for_pat p <*> collect_for_expr e) arms >>= \ arms ->
            IR.Type'Variable <$> lift (new_type_variable (CaseExpr sp)) >>= \ result_ty ->

            -- first expr matches all pattern types
            -- all arm types are the same
            tell (map (\ (arm_pat, _) -> Eq (IR.pattern_type arm_pat) (IR.expr_type testing)) arms) >>
            tell (map (\ (_, arm_result) -> Eq (IR.expr_type arm_result) result_ty) arms) >>

            pure (IR.Expr'Case result_ty sp testing arms)

        collect_for_expr (IR.Expr'Poison () sp) = IR.Expr'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonExpr sp)) <*> pure sp

        collect_for_expr (IR.Expr'TypeAnnotation () annotation e) =
            lift (convert_type_expr decls annotation) >>= \ annotation ->
            collect_for_expr e >>= \ e ->
            tell [Eq annotation (IR.expr_type e)] >>
            pure (IR.Expr'TypeAnnotation annotation annotation e)

solve_constraints :: [Constraint] -> StateWithVars ()
solve_constraints = mapM_ solve
    where
        -- TODO: gracefully figure out how to handle errors because the type variables become ambiguous if they cant be unified
        solve :: Constraint -> StateWithVars ()
        solve (Eq a b) =
            runExceptT (unify a b) >>= \case
                Right () -> pure ()
                Left (a_part, b_part) -> lift (tell [EqError { eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part }]) >> pure ()

        unify :: TypeWithVars -> TypeWithVars -> ExceptT (TypeWithVars, TypeWithVars) StateWithVars ()
        unify (IR.Type'Nominal a) (IR.Type'Nominal b) = todo
        unify (IR.Type'Variable a) b = unify_var a b False
        unify a (IR.Type'Variable b) = unify_var b a True
        unify (IR.Type'Int) (IR.Type'Int) = pure ()
        unify (IR.Type'Float) (IR.Type'Float) = pure ()
        unify (IR.Type'Char) (IR.Type'Char) = pure ()
        unify (IR.Type'String) (IR.Type'String) = pure ()
        unify (IR.Type'Bool) (IR.Type'Bool) = pure ()
        unify (IR.Type'Function a1 r1) (IR.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
        unify (IR.Type'Tuple a1 b1) (IR.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2

        unify_var :: TypeVarKey -> TypeWithVars -> Bool -> ExceptT (TypeWithVars, TypeWithVars) StateWithVars ()
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
                            TypeVar _ Fresh -> lift (set_type_var_state var (Substituted other)) -- TODO: check that these are not the same variable

                    -- if the other type is a type and not a variable
                    _ -> occurs_check var other >> lift (set_type_var_state var (Substituted other))

        set_type_var_state :: TypeVarKey -> TypeVarState -> StateWithVars ()
        set_type_var_state var new_state = modify (\ ty_arena -> Arena.modify ty_arena var (\ (TypeVar for _) -> TypeVar for new_state))

        occurs_check :: TypeVarKey -> TypeWithVars -> ExceptT (TypeWithVars, TypeWithVars) StateWithVars ()
        -- does the variable v occur anywhere in the type ty?
        occurs_check v ty = pure () -- TODO

apply_type_vars_to_bound_name :: TypeVarArena -> TypedWithVarsBoundName -> Writer [Error] TypedBoundName
apply_type_vars_to_bound_name = todo

apply_type_vars_to_nominal_type :: TypeVarArena -> TypedWithVarsNominalType -> Writer [Error] TypedNominalType
apply_type_vars_to_nominal_type = todo

apply_type_vars_to_binding :: TypeVarArena -> TypedWithVarsBinding -> Writer [Error] TypedBinding
apply_type_vars_to_binding = todo

