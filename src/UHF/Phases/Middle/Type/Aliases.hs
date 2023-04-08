module UHF.Phases.Middle.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

import UHF.IO.Located (Located (..))

import qualified UHF.Phases.Middle.Type.Unknown as Unknown

type TypeExpr = SIR.TypeExpr (Maybe DeclKey)
type TypeWithUnk = Type.Type Unknown.TypeUnknownKey
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

type TypedWithUnkSIR = SIR.SIR (Located (Maybe BoundValueKey)) TypeWithUnk TypeWithUnk Void
type TypedWithUnkDecl = SIR.Decl (Located (Maybe BoundValueKey)) TypeWithUnk TypeWithUnk Void
type TypedWithUnkADT = Type.ADT TypeWithUnk
type TypedWithUnkTypeSynonym = Type.TypeSynonym TypeWithUnk
type TypedWithUnkBinding = SIR.Binding (Located (Maybe BoundValueKey)) TypeWithUnk TypeWithUnk Void
type TypedWithUnkExpr = SIR.Expr (Located (Maybe BoundValueKey)) TypeWithUnk TypeWithUnk Void
type TypedWithUnkPattern = SIR.Pattern (Located (Maybe BoundValueKey)) TypeWithUnk
type TypedWithUnkBoundValue = SIR.BoundValue TypeWithUnk

type TypedWithUnkADTArena = Arena.Arena TypedWithUnkADT ADTKey
type TypedWithUnkTypeSynonymArena = Arena.Arena TypedWithUnkTypeSynonym Type.TypeSynonymKey
type TypedWithUnkBoundValueArena = Arena.Arena TypedWithUnkBoundValue BoundValueKey
type TypedWithUnkDeclArena = Arena.Arena TypedWithUnkDecl DeclKey

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
