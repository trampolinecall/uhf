module UHF.Phases.SolveTypes.StateWithInferVars (StateWithInferVars, new_type_unknown) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Error
import UHF.Phases.SolveTypes.Solver.InferVar
import qualified UHF.Compiler as Compiler
import qualified UHF.Util.Arena as Arena

type StateWithInferVars = StateT InferVarArena (Compiler.WithDiagnostics Error Void)

new_type_unknown :: InferVarForWhat -> StateWithInferVars InferVarKey
new_type_unknown for_what =
    state $ \ type_vars ->
        Arena.put (InferVar for_what Fresh) type_vars

