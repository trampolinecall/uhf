module UHF.Phases.SolveTypes.Aliases where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena

type TypeWithInferVars = TypeWithInferVar.Type
type Type = Type.Type

type VIden = Maybe SIR.BoundValue
type PIden = Maybe Type.ADT.VariantIndex

type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type Untyped = (UntypedDIden, UntypedDIden, TypeWithInferVars, VIden, VIden, PIden, PIden, (), Void)

type UntypedDIden = Maybe (SIR.Decl TypeWithInferVars)

type UntypedSIR = SIR.SIR Untyped
type UntypedModule = SIR.Module Untyped
type UntypedADT = SIR.ADT Untyped
type UntypedTypeSynonym = SIR.TypeSynonym Untyped
type UntypedBinding = SIR.Binding Untyped
type UntypedExpr = SIR.Expr Untyped
type UntypedPattern = SIR.Pattern Untyped
type UntypedVariable = SIR.Variable Untyped
type UntypedTypeExpr = SIR.TypeExpr Untyped

type UntypedADTArena = Arena.Arena UntypedADT Type.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedVariableArena = Arena.Arena UntypedVariable SIR.VariableKey
type UntypedModuleArena = Arena.Arena UntypedModule SIR.ModuleKey

type TypedWithInferVars = (TypedWithInferVarsDIden, TypedWithInferVarsDIden, TypeWithInferVars, VIden, VIden, PIden, PIden, TypeWithInferVars, Void)

type TypedWithInferVarsDIden = Maybe (SIR.Decl TypeWithInferVars)

type TypedWithInferVarsSIR = SIR.SIR TypedWithInferVars
type TypedWithInferVarsModule = SIR.Module TypedWithInferVars
type TypedWithInferVarsADT = SIR.ADT TypedWithInferVars
type TypedWithInferVarsTypeSynonym = SIR.TypeSynonym TypedWithInferVars
type TypedWithInferVarsBinding = SIR.Binding TypedWithInferVars
type TypedWithInferVarsExpr = SIR.Expr TypedWithInferVars
type TypedWithInferVarsPattern = SIR.Pattern TypedWithInferVars
type TypedWithInferVarsVariable = SIR.Variable TypedWithInferVars
type TypedWithInferVarsTypeExpr = SIR.TypeExpr TypedWithInferVars

type TypedWithInferVarsADTArena = Arena.Arena TypedWithInferVarsADT Type.ADTKey
type TypedWithInferVarsTypeSynonymArena = Arena.Arena TypedWithInferVarsTypeSynonym Type.TypeSynonymKey
type TypedWithInferVarsVariableArena = Arena.Arena TypedWithInferVarsVariable SIR.VariableKey
type TypedWithInferVarsModuleArena = Arena.Arena TypedWithInferVarsModule SIR.ModuleKey

type Typed = (TypedDIden, TypedDIden, Maybe Type, VIden, VIden, PIden, PIden, Maybe Type, Void)

type TypedDIden = Maybe (SIR.Decl Type)

type TypedSIR = SIR.SIR Typed
type TypedModule = SIR.Module Typed
type TypedADT = SIR.ADT Typed
type TypedTypeSynonym = SIR.TypeSynonym Typed
type TypedBinding = SIR.Binding Typed
type TypedExpr = SIR.Expr Typed
type TypedPattern = SIR.Pattern Typed
type TypedVariable = SIR.Variable Typed
type TypedTypeExpr = SIR.TypeExpr Typed

type TypedADTArena = Arena.Arena TypedADT Type.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedVariableArena = Arena.Arena TypedVariable SIR.VariableKey
type TypedModuleArena = Arena.Arena TypedModule SIR.ModuleKey
