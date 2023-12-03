module UHF.Data.IR.Type.QuantVar (QuantVarKey, QuantVar (..)) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)

newtype QuantVar = QuantVar (Located Text) deriving Show -- TODO: put id
