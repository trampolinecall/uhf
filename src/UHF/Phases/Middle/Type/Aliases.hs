module UHF.Phases.Middle.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

import UHF.IO.Located (Located (..))

import qualified UHF.Phases.Middle.Type.Var as Var

type TypeExpr = SIR.TypeExpr (Maybe DeclKey)
type TypeWithVars = Type.Type Var.TypeVarKey
type Type = Type.Type Void

type UntypedSIR = SIR.SIR (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedDecl = SIR.Decl (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedADT = Type.ADT TypeExpr
type UntypedTypeSynonym = Type.TypeSynonym TypeExpr
type UntypedBinding = SIR.Binding (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedExpr = SIR.Expr (Located (Maybe BoundValueKey)) TypeExpr () Void
type UntypedPattern = SIR.Pattern (Located (Maybe BoundValueKey)) ()
type UntypedBoundValue = SIR.BoundValue ()

type UntypedADTArena = Arena.Arena UntypedADT ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl DeclKey

type TypedWithVarsSIR = SIR.SIR (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsDecl = SIR.Decl (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsADT = Type.ADT TypeWithVars
type TypedWithVarsTypeSynonym = Type.TypeSynonym TypeWithVars
type TypedWithVarsBinding = SIR.Binding (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsExpr = SIR.Expr (Located (Maybe BoundValueKey)) TypeWithVars TypeWithVars Void
type TypedWithVarsPattern = SIR.Pattern (Located (Maybe BoundValueKey)) TypeWithVars
type TypedWithVarsBoundValue = SIR.BoundValue TypeWithVars

type TypedWithVarsADTArena = Arena.Arena TypedWithVarsADT ADTKey
type TypedWithVarsTypeSynonymArena = Arena.Arena TypedWithVarsTypeSynonym Type.TypeSynonymKey
type TypedWithVarsBoundValueArena = Arena.Arena TypedWithVarsBoundValue BoundValueKey
type TypedWithVarsDeclArena = Arena.Arena TypedWithVarsDecl DeclKey

type TypedSIR = SIR.SIR (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedDecl = SIR.Decl (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedADT = Type.ADT (Maybe Type)
type TypedTypeSynonym = Type.TypeSynonym (Maybe Type)
type TypedBinding = SIR.Binding (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedExpr = SIR.Expr (Located (Maybe BoundValueKey)) (Maybe Type) (Maybe Type) Void
type TypedPattern = SIR.Pattern (Located (Maybe BoundValueKey)) (Maybe Type)
type TypedBoundValue = SIR.BoundValue (Maybe Type)

type TypedADTArena = Arena.Arena TypedADT ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl DeclKey
