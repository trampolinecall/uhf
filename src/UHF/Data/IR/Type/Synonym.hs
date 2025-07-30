module UHF.Data.IR.Type.Synonym (TypeSynonymKey, TypeSynonym (..)) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.ID as ID

data TypeSynonym ty = TypeSynonym ID.DeclID (Located Text) ty deriving Show
