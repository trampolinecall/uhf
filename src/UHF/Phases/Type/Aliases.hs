module UHF.Phases.Type.Aliases where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.Type.Unknown as Unknown

type TypeWithUnk = Type.Type Unknown.TypeUnknownKey
type Type = Type.Type Void

type DIden = Maybe SIR.Decl
type VIden = Maybe SIR.BoundValueKey
type PIden = Maybe Type.ADTVariantIndex

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type Untyped = (DIden, DIden, Maybe Type, VIden, VIden, PIden, PIden, (), Void)

type UntypedSIR = SIR.SIR Untyped
type UntypedModule = SIR.Module Untyped
type UntypedADT = Type.ADT (UntypedTypeExpr, Maybe Type)
type UntypedTypeSynonym = Type.TypeSynonym (UntypedTypeExpr, Maybe Type)
type UntypedBinding = SIR.Binding Untyped
type UntypedExpr = SIR.Expr Untyped
type UntypedPattern = SIR.Pattern Untyped
type UntypedBoundValue = SIR.BoundValue Untyped
type UntypedTypeExpr = SIR.TypeExpr Untyped

type UntypedADTArena = Arena.Arena UntypedADT Type.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedBoundValueArena = Arena.Arena UntypedBoundValue SIR.BoundValueKey
type UntypedModuleArena = Arena.Arena UntypedModule SIR.ModuleKey

type TypedWithUnk = (DIden, DIden, TypeWithUnk, VIden, VIden, PIden, PIden, TypeWithUnk, Void)

type TypedWithUnkSIR = SIR.SIR TypedWithUnk
type TypedWithUnkModule = SIR.Module TypedWithUnk
type TypedWithUnkADT = Type.ADT (TypedWithUnkTypeExpr, TypeWithUnk)
type TypedWithUnkTypeSynonym = Type.TypeSynonym (TypedWithUnkTypeExpr, TypeWithUnk)
type TypedWithUnkBinding = SIR.Binding TypedWithUnk
type TypedWithUnkExpr = SIR.Expr TypedWithUnk
type TypedWithUnkPattern = SIR.Pattern TypedWithUnk
type TypedWithUnkBoundValue = SIR.BoundValue TypedWithUnk
type TypedWithUnkTypeExpr = SIR.TypeExpr TypedWithUnk

type TypedWithUnkADTArena = Arena.Arena TypedWithUnkADT Type.ADTKey
type TypedWithUnkTypeSynonymArena = Arena.Arena TypedWithUnkTypeSynonym Type.TypeSynonymKey
type TypedWithUnkBoundValueArena = Arena.Arena TypedWithUnkBoundValue SIR.BoundValueKey
type TypedWithUnkModuleArena = Arena.Arena TypedWithUnkModule SIR.ModuleKey

type Typed = (DIden, DIden, Maybe Type, VIden, VIden, PIden, PIden, Maybe Type, Void)

type TypedSIR = SIR.SIR Typed
type TypedModule = SIR.Module Typed
type TypedADT = Type.ADT (TypedTypeExpr, Maybe Type)
type TypedTypeSynonym = Type.TypeSynonym (TypedTypeExpr, Maybe Type)
type TypedBinding = SIR.Binding Typed
type TypedExpr = SIR.Expr Typed
type TypedPattern = SIR.Pattern Typed
type TypedBoundValue = SIR.BoundValue Typed
type TypedTypeExpr = SIR.TypeExpr Typed

type TypedADTArena = Arena.Arena TypedADT Type.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedBoundValueArena = Arena.Arena TypedBoundValue SIR.BoundValueKey
type TypedModuleArena = Arena.Arena TypedModule SIR.ModuleKey
