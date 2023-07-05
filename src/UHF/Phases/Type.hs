module UHF.Phases.Type (typecheck) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.SIR as SIR

import qualified UHF.Compiler as Compiler

import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Error

import qualified UHF.Phases.Type.AddTypes as AddTypes
import qualified UHF.Phases.Type.SolveConstraints as SolveConstraints
import qualified UHF.Phases.Type.RemoveUnknowns as RemoveUnknowns

-- also does type inference
typecheck :: UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
typecheck (SIR.SIR decls adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runWriterT (AddTypes.add adts type_synonyms bound_values decls) >>= \ ((adts, type_synonyms, bound_values, decls), constraints) ->
            SolveConstraints.solve adts type_synonyms type_vars constraints >>
            pure (decls, adts, type_synonyms, bound_values)
        )
        Arena.new >>= \ ((decls, adts, type_synonyms, bound_values), vars) ->

    RemoveUnknowns.remove vars decls adts type_synonyms bound_values >>= \ (decls, adts, type_synonyms, bound_values) ->
    pure (SIR.SIR decls adts type_synonyms type_vars bound_values mod)
