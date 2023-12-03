module UHF.Phases.SolveTypes.StateWithUnk (StateWithUnk, new_type_unknown) where

import UHF.Prelude

import UHF.Phases.SolveTypes.Error
import UHF.Phases.SolveTypes.Solver.Unknown
import qualified UHF.Compiler as Compiler
import qualified UHF.Util.Arena as Arena

type StateWithUnk = StateT TypeUnknownArena (Compiler.WithDiagnostics Error Void)

new_type_unknown :: TypeUnknownForWhat -> StateWithUnk TypeUnknownKey
new_type_unknown for_what =
    state $ \ type_vars ->
        Arena.put (TypeUnknown for_what Fresh) type_vars

