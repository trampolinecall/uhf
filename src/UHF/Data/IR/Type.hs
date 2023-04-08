module UHF.Data.IR.Type
    ( Type (..)

    , ADTKey
    , ADT (..)
    , ADTVariant (..)

    , TypeSynonymKey
    , TypeSynonym (..)

    , TypeVarKey
    , Var(..)
    ) where

import UHF.Util.Prelude

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.ID as ID

data Type unk
    = Type'ADT ADTKey
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function (Type unk) (Type unk)
    | Type'Tuple (Type unk) (Type unk)
    | Type'Unknown unk
    | Type'Variable TypeVarKey
    | Type'Forall [TypeVarKey] (Type unk)
    deriving Show

data ADT ty = ADT ID.DeclID Text [ADTVariant ty] deriving Show
data ADTVariant ty
    = ADTVariant'Named Text [(Text, ty)]
    | ADTVariant'Anon Text [ty]
    deriving Show

data TypeSynonym ty = TypeSynonym ID.DeclID Text ty deriving Show

data Var = Var Text deriving Show
