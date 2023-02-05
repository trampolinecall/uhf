module UHF.IR.Value where

import UHF.Util.Prelude

import qualified UHF.IR.Expr as Expr

data Value identifier = Value (Expr.Expr identifier) deriving Show
newtype ResolvedValue = ResolvedValue (Value ResolvedValue) deriving Show
