module UHF.Phases.Middle.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

import UHF.IO.Located (Located (..))

import qualified UHF.Phases.Middle.Type.Var as Var

type TypeExpr = HIR.TypeExpr (Maybe DeclKey)
type TypeWithVars = Type.Type Var.TypeVarKey
type Type = Type.Type Void

type UntypedHIR = HIR.HIR (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedDecl = HIR.Decl (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedADT = HIR.ADT TypeExpr
type UntypedTypeSynonym = HIR.TypeSynonym TypeExpr
type UntypedBinding = HIR.Binding (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedExpr = HIR.Expr (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedPattern = HIR.Pattern (Located (Maybe BoundValueKey)) ()
type UntypedBoundValue = HIR.BoundValue ()

type UntypedADTArena = Arena.Arena UntypedADT ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym HIR.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl DeclKey

type TypedWithVarsHIR = HIR.HIR (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsDecl = HIR.Decl (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsADT = HIR.ADT TypeWithVars
type TypedWithVarsTypeSynonym = HIR.TypeSynonym TypeWithVars
type TypedWithVarsBinding = HIR.Binding (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = HIR.Expr (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = HIR.Pattern (Located (Maybe BoundValueKey)) TypeWithVars
type TypedWithVarsBoundValue = HIR.BoundValue TypeWithVars

type TypedWithVarsADTArena = Arena.Arena TypedWithVarsADT ADTKey
type TypedWithVarsTypeSynonymArena = Arena.Arena TypedWithVarsTypeSynonym HIR.TypeSynonymKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue BoundValueKey
type TypedWithVarsDeclArena = Arena.Arena TypedWithVarsDecl DeclKey

type TypedHIR = HIR.HIR (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedDecl = HIR.Decl (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedADT = HIR.ADT (Maybe Type)
type TypedTypeSynonym = HIR.TypeSynonym (Maybe Type)
type TypedBinding = HIR.Binding (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedExpr = HIR.Expr (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedPattern = HIR.Pattern (Located (Maybe BoundValueKey)) (Maybe Type)
type TypedBoundValue = HIR.BoundValue (Maybe Type)

type TypedADTArena = Arena.Arena TypedADT ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym HIR.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl DeclKey
