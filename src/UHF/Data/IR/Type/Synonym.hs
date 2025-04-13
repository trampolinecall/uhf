module UHF.Data.IR.Type.Synonym (TypeSynonymKey, TypeSynonym (..)) where

import UHF.Prelude

import UHF.Data.IR.Keys

data TypeSynonym ty = TypeSynonym Text ty deriving Show
