module UHF.Phases.Type (typecheck) where

import UHF.Prelude

import UHF.Phases.Type.Aliases
import UHF.Phases.Type.Error
import qualified UHF.Util.Arena as Arena
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Phases.Type.AddTypes as AddTypes
import qualified UHF.Phases.Type.RemoveUnknowns as RemoveUnknowns
import qualified UHF.Phases.Type.SolveConstraints as SolveConstraints

-- also does type inference
typecheck :: UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
typecheck (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) = -- TODO: do not destructure ir?
    runStateT
        (
            runWriterT (AddTypes.add mods adts type_synonyms bound_values decls) >>= \ ((mods, adts, type_synonyms, bound_values, decls), constraints) ->
            SolveConstraints.solve adts type_synonyms type_vars constraints >>
            pure (decls, mods, adts, type_synonyms, bound_values)
        )
        Arena.new >>= \ ((decls, mods, adts, type_synonyms, bound_values), vars) ->

    RemoveUnknowns.remove vars mods adts type_synonyms bound_values >>= \ (mods, adts, type_synonyms, bound_values) ->
    pure (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod)
