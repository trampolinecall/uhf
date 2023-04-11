module UHF.Data.IR.Type
    ( Type (..)

    , ADTKey
    , ADT (..)
    , ADTVariant (..)
    , ADTVariantIndex (..)
    , get_adt_variant
    , variant_name
    , variant_field_types

    , TypeSynonymKey
    , TypeSynonym (..)

    , TypeVarKey
    , Var(..)
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.ID as ID

import qualified Data.List as List

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
    | Type'Forall (NonEmpty TypeVarKey) (Type unk)
    deriving Show

data ADT ty = ADT ID.DeclID Text [ADTVariant ty] deriving Show
data ADTVariant ty
    = ADTVariant'Named Text [(Text, ty)]
    | ADTVariant'Anon Text [ty]
    deriving Show
data ADTVariantIndex = ADTVariantIndex ADTKey Int deriving Show

variant_name :: ADTVariant ty -> Text
variant_name (ADTVariant'Anon name _) = name
variant_name (ADTVariant'Named name _) = name
variant_field_types :: ADTVariant ty -> [ty]
variant_field_types (ADTVariant'Anon _ tys) = tys
variant_field_types (ADTVariant'Named _ tys) = map snd tys

-- technically can error, but every ADTVariantIndex constructed should be a valid variant, so hopefully if everything is functioning correctly, this should never fail
get_adt_variant :: Arena.Arena (ADT ty) ADTKey -> ADTVariantIndex  -> ADTVariant ty
get_adt_variant adts (ADTVariantIndex key i) =
    let (ADT _ _ variants) = Arena.get adts key
    in variants List.!! i

data TypeSynonym ty = TypeSynonym ID.DeclID Text ty deriving Show

newtype Var = Var Text deriving Show -- TODO: put id
