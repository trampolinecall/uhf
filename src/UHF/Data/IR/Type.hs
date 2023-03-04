module UHF.Data.IR.Type
    ( Type (..)
    , ADTKey
    , ADT (..)
    , ADTVariant (..)
    , TypeSynonymKey
    , TypeSynonym (..)
    ) where

import UHF.Util.Prelude

import UHF.Data.IR.Keys

data Type var
    = Type'ADT ADTKey
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function (Type var) (Type var)
    | Type'Tuple (Type var) (Type var)
    | Type'Variable var
    deriving Show

data ADT ty = ADT Text [ADTVariant ty] deriving Show
data ADTVariant ty
    = ADTVariant'Named Text [(Text, ty)]
    | ADTVariant'Anon Text [ty]
    deriving Show

data TypeSynonym ty = TypeSynonym Text ty deriving Show

