module UHF.Data.IR.Type
    ( Type (..)
    , Kind (..)

    , ADT (..)
    , ADTKey

    , TypeSynonym (..)
    , TypeSynonymKey

    , QuantVar (..)
    , QuantVarKey
    ) where

import UHF.Prelude

import UHF.Data.IR.Keys

import UHF.Data.IR.Type.ADT
import UHF.Data.IR.Type.Synonym
import UHF.Data.IR.Type.QuantVar

data Type
    = Type'ADT ADTKey [Type]
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function Type Type
    | Type'Tuple Type Type
    | Type'QuantVar QuantVarKey
    | Type'Forall (NonEmpty QuantVarKey) Type
    | Type'Kind Kind
    deriving Show

data Kind
    = Kind'Type
    | Kind'Arrow Type Type
    | Kind'Kind
    deriving Show
