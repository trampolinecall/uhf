module UHF.Phases.Middle.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))

import qualified UHF.Phases.Middle.Type.Unknown as Unknown

type TypeWithUnk = Type.Type Unknown.TypeUnknownKey
type Type = Type.Type Void

type VIden = Located (Maybe SIR.BoundValueKey)
type DIden = Maybe SIR.DeclKey

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type UntypedSIR = SIR.SIR DIden VIden () Void
type UntypedDecl = SIR.Decl DIden VIden () Void
type UntypedADT = Type.ADT UntypedTypeExpr
type UntypedTypeSynonym = Type.TypeSynonym UntypedTypeExpr
type UntypedBinding = SIR.Binding DIden VIden () Void
type UntypedExpr = SIR.Expr DIden VIden () Void
type UntypedPattern = SIR.Pattern ()
type UntypedBoundValue = SIR.BoundValue ()
type UntypedTypeExpr = SIR.TypeExpr (Maybe SIR.DeclKey) ()

type UntypedADTArena = Arena.Arena UntypedADT Type.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue SIR.BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl SIR.DeclKey

type TypedWithUnkSIR = SIR.SIR DIden VIden TypeWithUnk Void
type TypedWithUnkDecl = SIR.Decl DIden VIden TypeWithUnk Void
type TypedWithUnkADT = Type.ADT TypedWithUnkTypeExpr
type TypedWithUnkTypeSynonym = Type.TypeSynonym TypedWithUnkTypeExpr
type TypedWithUnkBinding = SIR.Binding DIden VIden TypeWithUnk Void
type TypedWithUnkExpr = SIR.Expr DIden VIden TypeWithUnk Void
type TypedWithUnkPattern = SIR.Pattern TypeWithUnk
type TypedWithUnkBoundValue = SIR.BoundValue TypeWithUnk
type TypedWithUnkTypeExpr = SIR.TypeExpr (Maybe SIR.DeclKey) (Type.Type Unknown.TypeUnknownKey)

type TypedWithUnkADTArena = Arena.Arena TypedWithUnkADT Type.ADTKey
type TypedWithUnkTypeSynonymArena = Arena.Arena TypedWithUnkTypeSynonym Type.TypeSynonymKey
type TypedWithUnkBoundValueArena = Arena.Arena TypedWithUnkBoundValue SIR.BoundValueKey
type TypedWithUnkDeclArena = Arena.Arena TypedWithUnkDecl SIR.DeclKey

type TypedSIR = SIR.SIR DIden VIden (Maybe Type) Void
type TypedDecl = SIR.Decl DIden VIden (Maybe Type) Void
type TypedADT = Type.ADT TypedTypeExpr
type TypedTypeSynonym = Type.TypeSynonym TypedTypeExpr
type TypedBinding = SIR.Binding DIden VIden (Maybe Type) Void
type TypedExpr = SIR.Expr DIden VIden (Maybe Type) Void
type TypedPattern = SIR.Pattern (Maybe Type)
type TypedBoundValue = SIR.BoundValue (Maybe Type)
type TypedTypeExpr = SIR.TypeExpr (Maybe SIR.DeclKey) (Maybe Type)

type TypedADTArena = Arena.Arena TypedADT Type.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue SIR.BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl SIR.DeclKey
