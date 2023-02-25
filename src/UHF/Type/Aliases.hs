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
type UntypedADT = HIR.ADT TypeExpr
type UntypedTypeSynonym = HIR.TypeSynonym TypeExpr
type UntypedBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) TypeExpr () Void
type UntypedExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) TypeExpr () Void
type UntypedPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) ()
type UntypedBoundValue = HIR.BoundValue ()

type UntypedADTArena = Arena.Arena UntypedADT HIR.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym HIR.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue HIR.BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl HIR.DeclKey

type TypedWithVarsDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsADT = HIR.ADT TypeWithVars
type TypedWithVarsTypeSynonym = HIR.TypeSynonym TypeWithVars
type TypedWithVarsBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) TypeWithVars
type TypedWithVarsBoundValue = HIR.BoundValue TypeWithVars

type TypedWithVarsADTArena = Arena.Arena TypedWithVarsADT HIR.ADTKey
type TypedWithVarsTypeSynonymArena = Arena.Arena TypedWithVarsTypeSynonym HIR.TypeSynonymKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue HIR.BoundValueKey
type TypedWithVarsDeclArena = Arena.Arena TypedWithVarsDecl HIR.DeclKey

type TypedDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedADT = HIR.ADT (Maybe Type)
type TypedTypeSynonym = HIR.TypeSynonym (Maybe Type)
type TypedBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) (Maybe Type)
type TypedBoundValue = HIR.BoundValue (Maybe Type)

type TypedADTArena = Arena.Arena TypedADT HIR.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym HIR.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue HIR.BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl HIR.DeclKey
