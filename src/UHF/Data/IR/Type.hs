module UHF.Data.IR.Type
    ( Type (..)

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

-- TODO: remove infer_var from this
data Type infer_var
    = Type'ADT ADTKey [Type infer_var]
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function (Type infer_var) (Type infer_var)
    | Type'Tuple (Type infer_var) (Type infer_var)
    | Type'InferVar infer_var
    | Type'QuantVar QuantVarKey
    | Type'Forall (NonEmpty QuantVarKey) (Type infer_var)
    deriving Show


