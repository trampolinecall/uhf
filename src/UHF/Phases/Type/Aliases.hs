module UHF.Phases.Type.Aliases where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Located (Located (..))

import qualified UHF.Phases.Type.Unknown as Unknown

type TypeWithUnk = Type.Type Unknown.TypeUnknownKey
type Type = Type.Type Void

type DIden = Maybe SIR.DeclKey
type VIden = Located (Maybe SIR.BoundValueKey)
type PIden = Maybe Type.ADTVariantIndex

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type Untyped = (DIden, VIden, PIden, Type, (), Void)

type UntypedSIR = SIR.SIR Untyped
type UntypedDecl = SIR.Decl
type UntypedModule = SIR.Module Untyped
type UntypedADT = Type.ADT UntypedTypeExpr
type UntypedTypeSynonym = Type.TypeSynonym UntypedTypeExpr
type UntypedBinding = SIR.Binding Untyped
type UntypedExpr = SIR.Expr Untyped
type UntypedPattern = SIR.Pattern Untyped
type UntypedBoundValue = SIR.BoundValue Untyped
type UntypedTypeExpr = SIR.TypeExpr Untyped

type UntypedADTArena = Arena.Arena UntypedADT Type.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue SIR.BoundValueKey
type UntypedDeclArena = Arena.Arena UntypedDecl SIR.DeclKey
type UntypedModuleArena = Arena.Arena UntypedModule SIR.ModuleKey

type TypedWithUnk = (DIden, VIden, PIden, Type, TypeWithUnk, Void) -- TODO: make this use TypeWithUnk as the TypeExprTypeInfo instead of Type

type TypedWithUnkSIR = SIR.SIR TypedWithUnk
type TypedWithUnkDecl = SIR.Decl
type TypedWithUnkModule = SIR.Module TypedWithUnk
type TypedWithUnkADT = Type.ADT TypedWithUnkTypeExpr
type TypedWithUnkTypeSynonym = Type.TypeSynonym TypedWithUnkTypeExpr
type TypedWithUnkBinding = SIR.Binding TypedWithUnk
type TypedWithUnkExpr = SIR.Expr TypedWithUnk
type TypedWithUnkPattern = SIR.Pattern TypedWithUnk
type TypedWithUnkBoundValue = SIR.BoundValue TypedWithUnk
type TypedWithUnkTypeExpr = SIR.TypeExpr TypedWithUnk

type TypedWithUnkADTArena = Arena.Arena TypedWithUnkADT Type.ADTKey
type TypedWithUnkTypeSynonymArena = Arena.Arena TypedWithUnkTypeSynonym Type.TypeSynonymKey
type TypedWithUnkBoundValueArena = Arena.Arena TypedWithUnkBoundValue SIR.BoundValueKey
type TypedWithUnkDeclArena = Arena.Arena TypedWithUnkDecl SIR.DeclKey
type TypedWithUnkModuleArena = Arena.Arena TypedWithUnkModule SIR.ModuleKey

type Typed = (DIden, VIden, PIden, Type, Maybe Type, Void)

type TypedSIR = SIR.SIR Typed
type TypedDecl = SIR.Decl
type TypedModule = SIR.Module Typed
type TypedADT = Type.ADT TypedTypeExpr
type TypedTypeSynonym = Type.TypeSynonym TypedTypeExpr
type TypedBinding = SIR.Binding Typed
type TypedExpr = SIR.Expr Typed
type TypedPattern = SIR.Pattern Typed
type TypedBoundValue = SIR.BoundValue Typed
type TypedTypeExpr = SIR.TypeExpr Typed

type TypedADTArena = Arena.Arena TypedADT Type.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue SIR.BoundValueKey
type TypedDeclArena = Arena.Arena TypedDecl SIR.DeclKey
type TypedModuleArena = Arena.Arena TypedModule SIR.ModuleKey
