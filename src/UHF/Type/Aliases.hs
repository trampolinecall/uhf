module UHF.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.HIR as HIR

import UHF.IO.Located (Located (..))

import qualified UHF.Type.Var as Var

type TypeExpr = HIR.TypeExpr (Maybe HIR.DeclKey)
type TypeWithVars = HIR.Type Var.TypeVarKey
type Type = HIR.Type Void

type UntypedDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) TypeExpr () Void
type UntypedNominalType = HIR.NominalType TypeExpr
type UntypedBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) TypeExpr () Void
type UntypedExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) TypeExpr () Void
type UntypedPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) ()
type UntypedBoundValue = HIR.BoundValue ()

type UntypedNominalTypeArena = Arena.Arena UntypedNominalType HIR.NominalTypeKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue HIR.BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl HIR.DeclKey

type TypedWithVarsDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsNominalType = HIR.NominalType TypeWithVars
type TypedWithVarsBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) TypeWithVars
type TypedWithVarsBoundValue = HIR.BoundValue TypeWithVars

type TypedWithVarsNominalTypeArena = Arena.Arena TypedWithVarsNominalType HIR.NominalTypeKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue HIR.BoundValueKey
type TypedWithVarsDeclArena = Arena.Arena TypedWithVarsDecl HIR.DeclKey

type TypedDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedNominalType = HIR.NominalType (Maybe Type)
type TypedBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) (Maybe Type)
type TypedBoundValue = HIR.BoundValue (Maybe Type)

type TypedNominalTypeArena = Arena.Arena TypedNominalType HIR.NominalTypeKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue HIR.BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl HIR.DeclKey
