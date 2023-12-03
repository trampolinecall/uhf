module UHF.Phases.SolveTypes.StateWithInferVars (StateWithInferVars, new_type_unknown) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Error
import UHF.Phases.SolveTypes.Solver.InferVar
import qualified UHF.Compiler as Compiler
import qualified UHF.Util.Arena as Arena

type StateWithInferVars = StateT TypeInferVarArena (Compiler.WithDiagnostics Error Void)

new_type_unknown :: TypeInferVarForWhat -> StateWithInferVars TypeInferVarKey
new_type_unknown for_what =
    state $ \ type_vars ->
        Arena.put (TypeInferVar for_what TFresh) type_vars

