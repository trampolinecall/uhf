module UHF.Phases.Type.StateWithUnk (StateWithUnk, new_type_unknown) where

import UHF.Util.Prelude

import UHF.Phases.Type.Error
import UHF.Phases.Type.Unknown
import qualified Arena
import qualified UHF.Compiler as Compiler

type StateWithUnk = StateT TypeUnknownArena (Compiler.WithDiagnostics Error Void)

new_type_unknown :: TypeUnknownForWhat -> StateWithUnk TypeUnknownKey
new_type_unknown for_what =
    state $ \ type_vars ->
        Arena.put (TypeUnknown for_what Fresh) type_vars

