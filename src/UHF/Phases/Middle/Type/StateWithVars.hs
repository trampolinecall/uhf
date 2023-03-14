module UHF.Phases.Middle.Type.StateWithVars (StateWithVars, new_type_variable) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Error

type StateWithVars = StateT TypeVarArena (Compiler.WithDiagnostics Error Void)

new_type_variable :: TypeVarForWhat -> StateWithVars TypeVarKey
new_type_variable for_what =
    state $ \ type_vars ->
        Arena.put (TypeVar for_what Fresh) type_vars

