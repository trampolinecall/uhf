module UHF.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Located (Located (..))

import qualified UHF.Type.Var as Var

type TypeExpr = IR.TypeExpr (Maybe IR.DeclKey)
type TypeWithVars = IR.Type Var.TypeVarKey
type Type = IR.Type Void

type UntypedNominalType = IR.NominalType TypeExpr
type UntypedBinding = IR.Binding (Located (Maybe IR.BoundValueKey)) TypeExpr () Void
type UntypedExpr = IR.Expr (Located (Maybe IR.BoundValueKey)) TypeExpr () Void
type UntypedPattern = IR.Pattern (Located (Maybe IR.BoundValueKey)) ()
type UntypedBoundValue = IR.BoundValue ()

type UntypedBindingArena = Arena.Arena UntypedBinding IR.BindingKey
type UntypedNominalTypeArena = Arena.Arena UntypedNominalType IR.NominalTypeKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue IR.BoundValueKey

type TypedWithVarsNominalType = IR.NominalType TypeWithVars
type TypedWithVarsBinding = IR.Binding (Located (Maybe IR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = IR.Expr (Located (Maybe IR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = IR.Pattern (Located (Maybe IR.BoundValueKey)) TypeWithVars
type TypedWithVarsBoundValue = IR.BoundValue TypeWithVars

type TypedWithVarsBindingArena = Arena.Arena TypedWithVarsBinding IR.BindingKey
type TypedWithVarsNominalTypeArena = Arena.Arena TypedWithVarsNominalType IR.NominalTypeKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue IR.BoundValueKey

type TypedNominalType = IR.NominalType (Maybe Type)
type TypedBinding = IR.Binding (Located (Maybe IR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedExpr = IR.Expr (Located (Maybe IR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedPattern = IR.Pattern (Located (Maybe IR.BoundValueKey)) (Maybe Type)
type TypedBoundValue = IR.BoundValue (Maybe Type)

type TypedBindingArena = Arena.Arena TypedBinding IR.BindingKey
type TypedNominalTypeArena = Arena.Arena TypedNominalType IR.NominalTypeKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue IR.BoundValueKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

