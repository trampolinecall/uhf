module UHF.Phases.Middle.Type (typecheck) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Error
import UHF.Phases.Middle.Type.StateWithUnk

import qualified UHF.Phases.Middle.Type.ConvertTypeExpr as ConvertTypeExpr
import qualified UHF.Phases.Middle.Type.CollectConstraints as CollectConstraints
import qualified UHF.Phases.Middle.Type.SolveConstraints as SolveConstraints
import qualified UHF.Phases.Middle.Type.RemoveUnknowns as RemoveUnknowns

-- also does type inference
typecheck :: UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
typecheck (SIR.SIR decls adts type_synonyms bound_values mod) =
    runStateT
        (
            Arena.transformM (ConvertTypeExpr.adt decls) adts >>= \ adts ->
            Arena.transformM (ConvertTypeExpr.type_synonym decls) type_synonyms >>= \ type_synonyms ->
            Arena.transformM assign_type_variable_to_bound_value bound_values >>= \ bound_values ->
            runWriterT (Arena.transformM (CollectConstraints.collect decls bound_values) decls) >>= \ (decls, constraints) ->
            SolveConstraints.solve adts type_synonyms constraints >>
            pure (decls, adts, type_synonyms, bound_values)
        )
        Arena.new >>= \ ((decls, adts, type_synonyms, bound_values), vars) ->

    RemoveUnknowns.remove vars decls adts type_synonyms bound_values >>= \ (decls, adts, type_synonyms, bound_values) ->
    pure (SIR.SIR decls adts type_synonyms bound_values mod)

assign_type_variable_to_bound_value :: UntypedBoundValue -> StateWithUnk TypedWithUnkBoundValue
assign_type_variable_to_bound_value (SIR.BoundValue id () def_span) = SIR.BoundValue id <$> (Type.Type'Unknown <$> new_type_variable (BoundValue def_span)) <*> pure def_span
