module UHF.Type
    ( typecheck
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Location (Span)

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)

newtype TypeVarKey = TypeVarKey Int
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i
type TypeVarArena = Arena.Arena TypeVar TypeVarKey
data TypeVar -- = TypeVar TypeVarForWhat TypeVarStatus
    = BoundValue IR.BoundNameKey
    | Expr Span
    | Pattern Span
    | TypeExpr Span
    deriving Show

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

data Constraint

data Error
instance Diagnostic.IsError Error where

type StateWithVars = StateT TypeVarArena (Writer [Error])

new_type_variable :: TypeVar -> StateWithVars TypeVarKey
new_type_variable type_var =
    state $ \ type_vars ->
        Arena.put type_var type_vars

-- also does type inference
typecheck :: (DeclArena, UntypedNominalTypeArena, UntypedBindingArena, UntypedBoundNameArena) -> Writer [Error] (DeclArena, TypedNominalTypeArena, TypedBindingArena, TypedBoundNameArena)
typecheck (decls, nominal_types, bindings, bound_names) =

    runStateT
        (
            Arena.transformM (convert_type_exprs_in_nominal_types decls) nominal_types >>= \ nominal_types ->
            Arena.transformM assign_type_variable_to_bound_name bound_names >>= \ bound_names ->
            runWriterT (Arena.transformM collect_constraints bindings) >>= \ (bindings, constraints) ->
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
convert_type_expr decls (IR.TypeExpr'Tuple items) = IR.Type'Tuple <$> mapM (convert_type_expr decls) items

assign_type_variable_to_bound_name :: UntypedBoundName -> StateWithVars TypedWithVarsBoundName
assign_type_variable_to_bound_name (IR.BoundName ()) = todo

collect_constraints :: UntypedBinding -> WriterT [Constraint] StateWithVars TypedWithVarsBinding
collect_constraints = todo

solve_constraints :: [Constraint] -> StateWithVars ()
solve_constraints = todo

apply_type_vars_to_bound_name :: TypeVarArena -> TypedWithVarsBoundName -> Writer [Error] TypedBoundName
apply_type_vars_to_bound_name = todo

apply_type_vars_to_nominal_type :: TypeVarArena -> TypedWithVarsNominalType -> Writer [Error] TypedNominalType
apply_type_vars_to_nominal_type = todo

apply_type_vars_to_binding :: TypeVarArena -> TypedWithVarsBinding -> Writer [Error] TypedBinding
apply_type_vars_to_binding = todo

