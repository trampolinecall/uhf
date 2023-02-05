module UHF.IR.Decl where

import UHF.Util.Prelude

import qualified UHF.IR.Value as Value

import qualified Data.Map as Map

data Decl value_identifier = Decl'Module (Module value_identifier) deriving Show

data Module value_identifier = Module (Map.Map Text (Decl value_identifier)) (Map.Map Text (Value.Value value_identifier)) deriving Show
