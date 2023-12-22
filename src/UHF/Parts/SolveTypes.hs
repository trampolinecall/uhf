module UHF.Parts.SolveTypes (solve) where

import UHF.Prelude

import UHF.Parts.SolveTypes.Aliases
import UHF.Parts.SolveTypes.Error
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Parts.SolveTypes.AddTypes as AddTypes
import qualified UHF.Parts.SolveTypes.RemoveInferVars as RemoveInferVars
import qualified UHF.Util.Arena as Arena

-- this does both type inference and type checking
solve :: TypeSolver.SolverState -> UntypedSIR -> Compiler.WithDiagnostics Error Void TypedSIR
solve nr_solver_state (SIR.SIR mods adts type_synonyms quant_vars variables mod) = -- TODO: do not destructure ir?
    TypeSolver.run_solve_monad_with
        (
            runWriterT (AddTypes.add mods adts type_synonyms quant_vars variables) >>= \ ((mods, adts, type_synonyms, variables), constraints) ->
            let get_type_synonym ts_key = pure $ Arena.get type_synonyms ts_key
            in
            mapM_ (TypeSolver.solve_constraint adts type_synonyms get_type_synonym quant_vars) constraints >>
            TypeSolver.solve_constraint_backlog adts type_synonyms get_type_synonym quant_vars >>
            pure (mods, adts, type_synonyms, variables)
        )
        nr_solver_state >>= \ ((mods, adts, type_synonyms, variables), TypeSolver.SolverState _ infer_vars) ->

    RemoveInferVars.remove infer_vars mods adts type_synonyms variables >>= \ (mods, adts, type_synonyms, variables) ->
    pure (SIR.SIR mods adts type_synonyms quant_vars variables mod)
