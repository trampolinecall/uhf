module UHF.Phases.SolveTypes (solve) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Error
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Phases.SolveTypes.AddTypes as AddTypes
import qualified UHF.Phases.SolveTypes.RemoveInferVars as RemoveInferVars
import qualified UHF.Parts.TypeSolver as TypeSolver

-- this does both type inference and type checking
solve :: TypeSolver.SolverState -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
solve nr_solver_state (SIR.SIR mods adts type_synonyms quant_vars variables mod) = -- TODO: do not destructure ir?
    TypeSolver.run_solve_monad_with
        (
            runWriterT (AddTypes.add mods adts type_synonyms quant_vars variables) >>= \ ((mods, adts, type_synonyms, variables), constraints) ->
            mapM_ (TypeSolver.solve_constraint adts type_synonyms quant_vars) constraints >>
            pure (mods, adts, type_synonyms, variables)
        )
        nr_solver_state >>= \ ((mods, adts, type_synonyms, variables), TypeSolver.SolverState _ infer_vars) ->

    RemoveInferVars.remove infer_vars mods adts type_synonyms variables >>= \ (mods, adts, type_synonyms, variables) ->
    pure (SIR.SIR mods adts type_synonyms quant_vars variables mod)
