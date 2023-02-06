module UHF.IR.Decl where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IR.Value as Value

import qualified Data.Map as Map

newtype Key = Key Int deriving Show
instance Arena.Key Key where
    make_key = Key
    unmake_key (Key i) = i

data Decl = Decl'Module Module deriving Show

data Module = Module (Map.Map Text Key) (Map.Map Text Value.Key) deriving Show
