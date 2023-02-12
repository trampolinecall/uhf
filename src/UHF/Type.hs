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
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)

newtype TypeVarKey = TypeVarKey Int deriving (Show, Eq)
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

type TypedNominalType = IR.NominalType (Maybe Type)
type TypedBinding = IR.Binding (Maybe IR.BoundNameKey) (Maybe Type) (Maybe Type)
type TypedExpr = IR.Expr (Maybe IR.BoundNameKey) (Maybe Type) (Maybe Type)
type TypedPattern = IR.Pattern (Maybe IR.BoundNameKey) (Maybe Type)
type TypedBoundName = IR.BoundName (Maybe Type)

type TypedBindingArena = Arena.Arena TypedBinding IR.BindingKey
type TypedNominalTypeArena = Arena.Arena TypedNominalType IR.NominalTypeKey
type TypedBoundNameArena = Arena.Arena TypedBoundName IR.BoundNameKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data EqForWhat = Assignment | NamedPattern | IfBranches | CasePatterns | CaseArms
data Constraint
    = Eq EqForWhat Span (Located TypeWithVars) (Located TypeWithVars)
    | Expect (Located TypeWithVars) TypeWithVars

data Error
    = EqError
        { eq_error_nominal_types :: TypedWithVarsNominalTypeArena
        , eq_error_vars :: TypeVarArena
        , eq_error_for_what :: EqForWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithVars
        , eq_error_b_whole :: Located TypeWithVars
        , eq_error_a_part :: TypeWithVars
        , eq_error_b_part :: TypeWithVars
        }
    | ExpectError
        { expect_error_nominal_types :: TypedWithVarsNominalTypeArena
        , expect_error_vars :: TypeVarArena
        , expect_error_got_whole :: Located TypeWithVars
        , expect_error_expect_whole :: TypeWithVars
        , expect_error_got_part :: TypeWithVars
        , expect_error_expect_part :: TypeWithVars
        }

    | AmbiguousType TypeVarForWhat

instance Diagnostic.IsError Error where
    to_error (EqError nominal_types vars for_what span a_whole b_whole a_part b_part) =
        let what = case for_what of
                Assignment -> "assignment"
                NamedPattern -> "named pattern"
                IfBranches -> "'if' expression"
                CasePatterns -> "'case' expression patterns"
                CaseArms -> "'case' expression arms"

        in Diagnostic.Error Diagnostic.Codes.type_mismatch $
            Diagnostic.DiagnosticContents
                (Just span)
                [Underlines.underlines
                    [ span `Underlines.primary` [Underlines.error $ convert_str $ "conflicting types in " <> what <> ": '" <> print_type nominal_types vars a_part <> "' vs '" <> print_type nominal_types vars b_part <> "'"]
                    , just_span a_whole `Underlines.secondary` [Underlines.note $ convert_str $ print_type nominal_types vars $ unlocate a_whole]
                    , just_span b_whole `Underlines.secondary` [Underlines.note $ convert_str $ print_type nominal_types vars $ unlocate b_whole]
                    ]
                ]

    to_error (ExpectError {expect_error_nominal_types = nominal_types, expect_error_vars = vars, ..}) =
        Diagnostic.Error Diagnostic.Codes.type_mismatch $ -- TODO, also TODO change code?
            Diagnostic.DiagnosticContents
                (Just (just_span expect_error_got_whole))
                [Underlines.underlines -- TODO
                    [ just_span expect_error_got_whole `Underlines.primary` [Underlines.error $ convert_str $ print_type nominal_types vars $ unlocate expect_error_got_whole]
                    , just_span expect_error_got_whole `Underlines.primary` [Underlines.error $ convert_str $ print_type nominal_types vars expect_error_expect_whole]
                    , just_span expect_error_got_whole `Underlines.primary` [Underlines.error $ convert_str $ print_type nominal_types vars expect_error_got_part]
                    , just_span expect_error_got_whole `Underlines.primary` [Underlines.error $ convert_str $ print_type nominal_types vars expect_error_expect_part]
                    ]
                ]

    to_error (AmbiguousType for_what) =
        let (sp, name) = case for_what of
                BoundName sp -> (sp, "binding")
                UnresolvedIdenExpr sp -> (sp, "identifier expression") -- TODO: make sure this doesnt happen
                CallExpr sp -> (sp, "call expression")
                CaseExpr sp -> (sp, "case expression")
                PoisonExpr sp -> (sp, "expression")
                PoisonPattern sp -> (sp, "pattern")
                TypeExpr sp -> (sp, "type expression")
        in Diagnostic.Error Diagnostic.Codes.ambiguous_type $
            Diagnostic.DiagnosticContents
                (Just sp)
                [Underlines.underlines -- TODO
                    [ sp `Underlines.primary` [Underlines.error $ "ambiguous type: could not infer the type of this " <> name]
                    ]
                ]

print_type :: TypedWithVarsNominalTypeArena -> TypeVarArena -> TypeWithVars -> Text
-- TODO: construct an ast and print it
print_type nominals _ (IR.Type'Nominal key) =
    case Arena.get nominals key of
        IR.NominalType'Data name _ -> name
        IR.NominalType'Synonym name _ -> name
print_type _ _ (IR.Type'Int) = "int"
print_type _ _ (IR.Type'Float) = "float"
print_type _ _ (IR.Type'Char) = "char"
print_type _ _ (IR.Type'String) = "string"
print_type _ _ (IR.Type'Bool) = "bool"
print_type nominals vars (IR.Type'Function a r) = print_type nominals vars a <> " -> " <> print_type nominals vars r -- TODO: parentheses and grouping
print_type nominals vars (IR.Type'Tuple a b) = "(" <> print_type nominals vars a <> ", " <> print_type nominals vars b <> ")"
print_type nominals vars (IR.Type'Variable var) = case Arena.get vars var of
    TypeVar _ Fresh -> "_"
    TypeVar _ (Substituted other) -> print_type nominals vars other

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
            solve_constraints nominal_types constraints >>
            pure (nominal_types, bound_names, bindings)
        )
        Arena.new >>= \ ((nominal_types, bound_names, bindings), vars) ->

    Arena.transformM (remove_vars_from_nominal_type vars) nominal_types >>= \ nominal_types ->
    Arena.transformM (remove_vars_from_bound_name vars) bound_names >>= \ bound_names ->
    Arena.transformM (remove_vars_from_binding vars) bindings >>= \ bindings ->

    pure (decls, nominal_types, bindings, bound_names)

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
        IR.Decl'Type ty -> pure $ IR.Type'Nominal ty
    Nothing -> IR.Type'Variable <$> new_type_variable (TypeExpr sp)
convert_type_expr decls (IR.TypeExpr'Tuple a b) = IR.Type'Tuple <$> convert_type_expr decls a <*> convert_type_expr decls b
convert_type_expr _ (IR.TypeExpr'Poison sp) = IR.Type'Variable <$> new_type_variable (TypeExpr sp)

assign_type_variable_to_bound_name :: UntypedBoundName -> StateWithVars TypedWithVarsBoundName
assign_type_variable_to_bound_name (IR.BoundName () def_span) = IR.BoundName <$> (IR.Type'Variable <$> new_type_variable (BoundName def_span)) <*> pure def_span

collect_constraints :: DeclArena -> TypedWithVarsBoundNameArena -> UntypedBinding -> WriterT [Constraint] StateWithVars TypedWithVarsBinding
collect_constraints decls bna (IR.Binding pat eq_sp expr) =
    collect_for_pat pat >>= \ pat ->
    collect_for_expr expr >>= \ expr ->
    tell [Eq Assignment eq_sp (loc_pat_type pat) (loc_expr_type expr)] >>
    pure (IR.Binding pat eq_sp expr)
    where
        loc_pat_type pat = Located (IR.pattern_span pat) (IR.pattern_type pat)
        loc_expr_type expr = Located (IR.expr_span expr) (IR.expr_type expr)

        collect_for_pat (IR.Pattern'Identifier () sp bn) =
            let (IR.BoundName ty _) = Arena.get bna bn
            in pure (IR.Pattern'Identifier ty sp bn)

        collect_for_pat (IR.Pattern'Tuple () sp l r) =
            collect_for_pat l >>= \ l ->
            collect_for_pat r >>= \ r ->
            pure (IR.Pattern'Tuple (IR.Type'Tuple (IR.pattern_type l) (IR.pattern_type r)) sp l r)

        collect_for_pat (IR.Pattern'Named () sp at_sp bnk subpat) =
            collect_for_pat subpat >>= \ subpat ->
            let (IR.BoundName bn_ty _) = Arena.get bna (unlocate bnk)
            in tell [Eq NamedPattern at_sp (Located (just_span bnk) bn_ty) (loc_pat_type subpat)] >>
            pure (IR.Pattern'Named bn_ty sp at_sp bnk subpat)

        collect_for_pat (IR.Pattern'Poison () sp) = IR.Pattern'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonPattern sp)) <*> pure sp

        collect_for_expr (IR.Expr'Identifier () sp bn) =
            (case bn of
                Just bn -> let (IR.BoundName ty _) = Arena.get bna bn in pure ty
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

        collect_for_expr (IR.Expr'BinaryOps () _ _ _) = todo -- TODO: group these before this stage so that this does not exist

        collect_for_expr (IR.Expr'Call () sp callee arg) =
            collect_for_expr callee >>= \ callee ->
            collect_for_expr arg >>= \ arg ->
            lift (new_type_variable (CallExpr sp)) >>= \ res_ty_var ->

            tell [Expect (loc_expr_type callee) (IR.Type'Function (IR.expr_type arg) (IR.Type'Variable res_ty_var))] >>

            pure (IR.Expr'Call (IR.Type'Variable res_ty_var) sp callee arg)

        collect_for_expr (IR.Expr'If () sp if_sp cond true false) =
            collect_for_expr cond >>= \ cond ->
            collect_for_expr true >>= \ true ->
            collect_for_expr false >>= \ false ->

            tell
                [ Expect (loc_expr_type cond) IR.Type'Bool
                , Eq IfBranches if_sp (loc_expr_type true) (loc_expr_type false)
                ] >>

            pure (IR.Expr'If (IR.expr_type true) sp if_sp cond true false)

        collect_for_expr (IR.Expr'Case () sp case_tok_sp testing arms) =
            collect_for_expr testing >>= \ testing ->
            mapM (\ (p, e) -> (,) <$> collect_for_pat p <*> collect_for_expr e) arms >>= \ arms ->
            IR.Type'Variable <$> lift (new_type_variable (CaseExpr sp)) >>= \ result_ty ->

            -- first expr matches all pattern types
            tell (map (\ (arm_pat, _) -> Eq CasePatterns case_tok_sp (loc_pat_type arm_pat) (loc_expr_type testing)) arms) >>
            -- all arm types are the same TODO: find a better way to do this (probably iterate in pairs)
            tell (map (\ (_, arm_result) -> Eq CaseArms case_tok_sp (loc_expr_type arm_result) (Located sp result_ty)) arms) >>

            pure (IR.Expr'Case result_ty sp case_tok_sp testing arms)

        collect_for_expr (IR.Expr'Poison () sp) = IR.Expr'Poison <$> (IR.Type'Variable <$> lift (new_type_variable $ PoisonExpr sp)) <*> pure sp

        collect_for_expr (IR.Expr'TypeAnnotation () sp annotation e) =
            lift (convert_type_expr decls annotation) >>= \ annotation ->
            collect_for_expr e >>= \ e ->
            tell [Expect (loc_expr_type e) annotation] >> -- TODO: use annotation span
            pure (IR.Expr'TypeAnnotation annotation sp annotation e)

solve_constraints :: TypedWithVarsNominalTypeArena -> [Constraint] -> StateWithVars ()
solve_constraints nominal_types = mapM_ solve
    where
        -- TODO: gracefully figure out how to handle errors because the type variables become ambiguous if they cant be unified
        solve :: Constraint -> StateWithVars ()
        solve (Eq for_what sp a b) =
            runExceptT (unify (unlocate a) (unlocate b)) >>= \case
                Right () -> pure ()
                Left (a_part, b_part) ->
                    get >>= \ vars ->
                    lift (tell [EqError { eq_error_nominal_types = nominal_types, eq_error_vars = vars, eq_error_for_what = for_what, eq_error_span = sp, eq_error_a_whole = a, eq_error_b_whole = b, eq_error_a_part = a_part, eq_error_b_part = b_part }]) >> pure ()

        solve (Expect got expect) =
            runExceptT (unify (unlocate got) expect) >>= \case
                Right () -> pure ()
                Left (got_part, expect_part) ->
                    get >>= \ vars ->
                    lift (tell [ExpectError { expect_error_nominal_types = nominal_types, expect_error_vars = vars, expect_error_got_whole = got, expect_error_expect_whole = expect, expect_error_got_part = got_part, expect_error_expect_part = expect_part }]) >> pure ()

        unify :: TypeWithVars -> TypeWithVars -> ExceptT (TypeWithVars, TypeWithVars) StateWithVars ()
        unify a@(IR.Type'Nominal a_nominal_idx) b@(IR.Type'Nominal b_nominal_idx) =
            case (Arena.get nominal_types a_nominal_idx, Arena.get nominal_types b_nominal_idx) of
                (IR.NominalType'Synonym _ a_expansion, _) -> unify a_expansion b
                (_, IR.NominalType'Synonym _ b_expansion) -> unify a b_expansion
                (IR.NominalType'Data _ _, IR.NominalType'Data _ _) -> if a_nominal_idx == b_nominal_idx
                    then pure ()
                    else ExceptT (pure $ Left (a, b))

        unify (IR.Type'Variable a) b = unify_var a b False
        unify a (IR.Type'Variable b) = unify_var b a True
        unify (IR.Type'Int) (IR.Type'Int) = pure ()
        unify (IR.Type'Float) (IR.Type'Float) = pure ()
        unify (IR.Type'Char) (IR.Type'Char) = pure ()
        unify (IR.Type'String) (IR.Type'String) = pure ()
        unify (IR.Type'Bool) (IR.Type'Bool) = pure ()
        unify (IR.Type'Function a1 r1) (IR.Type'Function a2 r2) = unify a1 a2 >> unify r1 r2
        unify (IR.Type'Tuple a1 b1) (IR.Type'Tuple a2 b2) = unify a1 a2 >> unify b1 b2
        unify a b = ExceptT (pure $ Left (a, b))

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
                            TypeVar _ Fresh ->
                                if var /= other_var
                                    then lift (set_type_var_state var (Substituted other))
                                    else pure ()

                    -- if the other type is a type and not a variable
                    _ -> occurs_check var other >> lift (set_type_var_state var (Substituted other))

        set_type_var_state :: TypeVarKey -> TypeVarState -> StateWithVars ()
        set_type_var_state var new_state = modify (\ ty_arena -> Arena.modify ty_arena var (\ (TypeVar for _) -> TypeVar for new_state))

        occurs_check :: TypeVarKey -> TypeWithVars -> ExceptT (TypeWithVars, TypeWithVars) StateWithVars ()
        -- does the variable v occur anywhere in the type ty?
        occurs_check v ty = pure () -- TODO

remove_vars_from_bound_name :: TypeVarArena -> TypedWithVarsBoundName -> Writer [Error] TypedBoundName
remove_vars_from_bound_name vars (IR.BoundName ty sp) = IR.BoundName <$> remove_vars vars ty <*> pure sp

remove_vars_from_nominal_type :: TypeVarArena -> TypedWithVarsNominalType -> Writer [Error] TypedNominalType
remove_vars_from_nominal_type vars (IR.NominalType'Data name variants) = IR.NominalType'Data name <$> mapM remove_from_variant variants
    where
        remove_from_variant (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> remove_vars vars ty) fields
        remove_from_variant (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> mapM (remove_vars vars) fields
remove_vars_from_nominal_type vars (IR.NominalType'Synonym name expansion) = IR.NominalType'Synonym name <$> remove_vars vars expansion

remove_vars_from_binding :: TypeVarArena -> TypedWithVarsBinding -> Writer [Error] TypedBinding
remove_vars_from_binding vars (IR.Binding pat eq_sp expr) = IR.Binding <$> remove_from_pat pat <*> pure eq_sp <*> remove_from_expr expr
    where
        remove_from_pat (IR.Pattern'Identifier ty sp bn) = remove_vars vars ty >>= \ ty -> pure (IR.Pattern'Identifier ty sp bn)
        remove_from_pat (IR.Pattern'Tuple ty sp l r) = remove_vars vars ty >>= \ ty -> IR.Pattern'Tuple ty sp <$> remove_from_pat l <*> remove_from_pat r
        remove_from_pat (IR.Pattern'Named ty sp at_sp bnk subpat) = remove_vars vars ty >>= \ ty -> IR.Pattern'Named ty sp at_sp bnk <$> remove_from_pat subpat
        remove_from_pat (IR.Pattern'Poison ty sp) = remove_vars vars ty >>= \ ty -> pure (IR.Pattern'Poison ty sp)

        remove_from_expr (IR.Expr'Identifier ty sp bn) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Identifier ty sp bn)
        remove_from_expr (IR.Expr'Char ty sp c) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Char ty sp c)
        remove_from_expr (IR.Expr'String ty sp t) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'String ty sp t)
        remove_from_expr (IR.Expr'Int ty sp i) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Int ty sp i)
        remove_from_expr (IR.Expr'Float ty sp r) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Float ty sp r)
        remove_from_expr (IR.Expr'Bool ty sp b) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Bool ty sp b)
        remove_from_expr (IR.Expr'Tuple ty sp l r) = remove_vars vars ty >>= \ ty -> IR.Expr'Tuple ty sp <$> remove_from_expr l <*> remove_from_expr r
        remove_from_expr (IR.Expr'Lambda ty sp param body) = remove_vars vars ty >>= \ ty -> IR.Expr'Lambda ty sp <$> remove_from_pat param <*> remove_from_expr body
        remove_from_expr (IR.Expr'Let ty sp result) = remove_vars vars ty >>= \ ty -> IR.Expr'Let ty sp <$> remove_from_expr result
        remove_from_expr (IR.Expr'LetRec ty sp result) = remove_vars vars ty >>= \ ty -> IR.Expr'LetRec ty sp <$> remove_from_expr result
        remove_from_expr (IR.Expr'BinaryOps _ _ _ _) = todo
        remove_from_expr (IR.Expr'Call ty sp callee arg) = remove_vars vars ty >>= \ ty -> IR.Expr'Call ty sp <$> remove_from_expr callee <*> remove_from_expr arg
        remove_from_expr (IR.Expr'If ty sp if_sp cond true false) = remove_vars vars ty >>= \ ty -> IR.Expr'If ty sp if_sp <$> remove_from_expr cond <*> remove_from_expr true <*> remove_from_expr false
        remove_from_expr (IR.Expr'Case ty sp case_sp testing arms) = remove_vars vars ty >>= \ ty -> IR.Expr'Case ty sp case_sp <$> remove_from_expr testing <*> mapM (\ (p, e) -> (,) <$> remove_from_pat p <*> remove_from_expr e) arms
        remove_from_expr (IR.Expr'Poison ty sp) = remove_vars vars ty >>= \ ty -> pure (IR.Expr'Poison ty sp)
        remove_from_expr (IR.Expr'TypeAnnotation ty sp annotation e) = remove_vars vars ty >>= \ ty -> remove_vars vars annotation >>= \ annotation -> IR.Expr'TypeAnnotation ty sp annotation <$> remove_from_expr e

remove_vars :: TypeVarArena -> TypeWithVars -> Writer [Error] (Maybe Type)
remove_vars vars ty = runMaybeT $ r ty
    -- TODO: cache in order to not repeat errors
    where
        r (IR.Type'Int) = pure $ IR.Type'Int
        r (IR.Type'Float) = pure $ IR.Type'Float
        r (IR.Type'Char) = pure $ IR.Type'Char
        r (IR.Type'String) = pure $ IR.Type'String
        r (IR.Type'Bool) = pure $ IR.Type'Bool
        r (IR.Type'Nominal n) = pure $ IR.Type'Nominal n
        r (IR.Type'Function arg res) = IR.Type'Function <$> r arg <*> r res
        r (IR.Type'Tuple a b) = IR.Type'Tuple <$> r a <*> r b
        r (IR.Type'Variable v) =
            case Arena.get vars v of
                TypeVar _ (Substituted s) -> r s
                TypeVar for_what Fresh ->
                    lift (tell [AmbiguousType for_what]) >> MaybeT (pure Nothing)
