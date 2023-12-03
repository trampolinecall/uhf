module UHF.Phases.SolveTypes (solve) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Error
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Phases.SolveTypes.AddTypes as AddTypes
import qualified UHF.Phases.SolveTypes.RemoveInferVars as RemoveInferVars
import qualified UHF.Phases.SolveTypes.Solver as Solver

-- this does both type inference and type checking
solve :: Solver.InferVarArena -> [Solver.Constraint] -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
solve infer_var_arena nr_constraints (SIR.SIR mods adts type_synonyms type_vars variables mod) = -- TODO: do not destructure ir?
    runStateT
        (
            runWriterT (AddTypes.add mods adts type_synonyms variables) >>= \ ((mods, adts, type_synonyms, variables), constraints) ->
            Solver.solve adts type_synonyms type_vars (nr_constraints ++ constraints) >>
            pure (mods, adts, type_synonyms, variables)
        )
        infer_var_arena >>= \ ((mods, adts, type_synonyms, variables), vars) ->

    RemoveInferVars.remove vars mods adts type_synonyms variables >>= \ (mods, adts, type_synonyms, variables) ->
    pure (SIR.SIR mods adts type_synonyms type_vars variables mod)
