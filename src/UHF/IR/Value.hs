module UHF.IR.Value where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IR.Expr as Expr

data Key = Key Int deriving Show
instance Arena.Key Key where
    make_key = Key
    unmake_key (Key i) = i

type ValueArena value_identifier = Arena.Arena (Value value_identifier) Key

data Value identifier = Value (Expr.Expr identifier) deriving Show
