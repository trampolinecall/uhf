module UHF.Data.IR.Type
    ( Type(..)
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

