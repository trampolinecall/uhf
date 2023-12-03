module UHF.Phases.SolveTypes.Aliases where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.SolveTypes.Solver.InferVar as InferVar

type TypeWithInferVars = Type.Type InferVar.TypeInferVarKey
type Type = Type.Type Void

type DIden = Maybe SIR.Decl
type VIden = Maybe SIR.VariableKey
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
type UntypedVariable = SIR.Variable Untyped
type UntypedTypeExpr = SIR.TypeExpr Untyped

type UntypedADTArena = Arena.Arena UntypedADT Type.ADTKey
type UntypedTypeSynonymArena = Arena.Arena UntypedTypeSynonym Type.TypeSynonymKey
type UntypedVariableArena = Arena.Arena UntypedVariable SIR.VariableKey
type UntypedModuleArena = Arena.Arena UntypedModule SIR.ModuleKey

type TypedWithInferVars = (DIden, DIden, TypeWithInferVars, VIden, VIden, PIden, PIden, TypeWithInferVars, Void)

type TypedWithInferVarsSIR = SIR.SIR TypedWithInferVars
type TypedWithInferVarsModule = SIR.Module TypedWithInferVars
type TypedWithInferVarsADT = Type.ADT (TypedWithInferVarsTypeExpr, TypeWithInferVars)
type TypedWithInferVarsTypeSynonym = Type.TypeSynonym (TypedWithInferVarsTypeExpr, TypeWithInferVars)
type TypedWithInferVarsBinding = SIR.Binding TypedWithInferVars
type TypedWithInferVarsExpr = SIR.Expr TypedWithInferVars
type TypedWithInferVarsPattern = SIR.Pattern TypedWithInferVars
type TypedWithInferVarsVariable = SIR.Variable TypedWithInferVars
type TypedWithInferVarsTypeExpr = SIR.TypeExpr TypedWithInferVars

type TypedWithInferVarsADTArena = Arena.Arena TypedWithInferVarsADT Type.ADTKey
type TypedWithInferVarsTypeSynonymArena = Arena.Arena TypedWithInferVarsTypeSynonym Type.TypeSynonymKey
type TypedWithInferVarsVariableArena = Arena.Arena TypedWithInferVarsVariable SIR.VariableKey
type TypedWithInferVarsModuleArena = Arena.Arena TypedWithInferVarsModule SIR.ModuleKey

type Typed = (DIden, DIden, Maybe Type, VIden, VIden, PIden, PIden, Maybe Type, Void)

type TypedSIR = SIR.SIR Typed
type TypedModule = SIR.Module Typed
type TypedADT = Type.ADT (TypedTypeExpr, Maybe Type)
type TypedTypeSynonym = Type.TypeSynonym (TypedTypeExpr, Maybe Type)
type TypedBinding = SIR.Binding Typed
type TypedExpr = SIR.Expr Typed
type TypedPattern = SIR.Pattern Typed
type TypedVariable = SIR.Variable Typed
type TypedTypeExpr = SIR.TypeExpr Typed

type TypedADTArena = Arena.Arena TypedADT Type.ADTKey
type TypedTypeSynonymArena = Arena.Arena TypedTypeSynonym Type.TypeSynonymKey
type TypedVariableArena = Arena.Arena TypedVariable SIR.VariableKey
type TypedModuleArena = Arena.Arena TypedModule SIR.ModuleKey
