module UHF.Phases.Middle.Type.StateWithUnk (StateWithUnk, new_type_unknown) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Error

type StateWithUnk = StateT TypeUnknownArena (Compiler.WithDiagnostics Error Void)

new_type_unknown :: TypeUnknownForWhat -> StateWithUnk TypeUnknownKey
new_type_unknown for_what =
    state $ \ type_vars ->
        Arena.put (TypeUnknown for_what Fresh) type_vars
