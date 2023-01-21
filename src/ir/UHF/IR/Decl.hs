module UHF.IR.Decl where

import UHF.Util.Prelude

import qualified UHF.IR.Value as Value

import qualified Data.Map as Map

data Decl identifier = Decl'Module (Module identifier)

data Module identifier = Module (Map.Map Text (Decl identifier)) (Map.Map Text (Value.Value identifier))
