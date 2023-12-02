module UHF.Data.IR.Type
    ( Type (..)

    , ADTKey
    , ADT (..)
    , ADTVariant (..)
    , ADTVariantIndex (..)
    , ADTFieldIndex (..)
    , get_adt_variant
    , get_adt_field_type
    , get_adt_field_id
    , adt_variant_idxs
    , variant_name
    , variant_id
    , variant_field_ids
    , variant_field_types

    , TypeSynonymKey
    , TypeSynonym (..)

    , TypeVarKey
    , Var(..)
    ) where

import UHF.Util.Prelude

import qualified Data.List as List

import UHF.Data.IR.Keys
import UHF.IO.Located (Located)
import qualified Arena
import qualified UHF.Data.IR.ID as ID

data Type unk
    = Type'ADT ADTKey [Type unk]
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

data ADT ty = ADT ID.DeclID (Located Text) [TypeVarKey] [ADTVariant ty] deriving Show
data ADTVariant ty
    = ADTVariant'Named (Located Text) ID.ADTVariantID [(ID.ADTFieldID, Text, ty)]
    | ADTVariant'Anon (Located Text) ID.ADTVariantID [(ID.ADTFieldID, ty)]
    deriving Show
data ADTVariantIndex = ADTVariantIndex ADTKey Int deriving (Show, Eq)
data ADTFieldIndex = ADTFieldIndex ADTVariantIndex Int deriving (Show, Eq)

adt_variant_idxs :: Arena.Arena (ADT ty) ADTKey -> ADTKey -> [ADTVariantIndex]
adt_variant_idxs arena key =
    let (ADT _ _ _ variants) = Arena.get arena key
    in map (ADTVariantIndex key) [0 .. length variants - 1]

variant_name :: ADTVariant ty -> Located Text
variant_name (ADTVariant'Anon name _ _) = name
variant_name (ADTVariant'Named name _ _) = name
variant_id :: ADTVariant ty -> ID.ADTVariantID
variant_id (ADTVariant'Anon _ id _) = id
variant_id (ADTVariant'Named _ id _) = id
variant_field_ids :: ADTVariant ty -> [ID.ADTFieldID]
variant_field_ids (ADTVariant'Anon _ _ fields) = map fst fields
variant_field_ids (ADTVariant'Named _ _ fields) = fields & map (\ (id, _, _) -> id)
variant_field_types :: ADTVariant ty -> [ty]
variant_field_types (ADTVariant'Anon _ _ tys) = map snd tys
variant_field_types (ADTVariant'Named _ _ tys) = tys & map (\ (_, _, ty) -> ty)

-- technically can error, but every ADTVariantIndex constructed should be a valid variant, so hopefully if everything is functioning correctly, this should never fail
get_adt_variant :: Arena.Arena (ADT ty) ADTKey -> ADTVariantIndex -> ADTVariant ty
get_adt_variant adts (ADTVariantIndex key i) =
    let (ADT _ _ _ variants) = Arena.get adts key
    in variants List.!! i

-- hope that field index is legal
get_adt_field_type :: Arena.Arena (ADT ty) ADTKey -> ADTFieldIndex -> ty
get_adt_field_type adts (ADTFieldIndex variant i) =
    case get_adt_variant adts variant of
        ADTVariant'Named _ _ fields -> fields List.!! i & \ (_, _, ty) -> ty
        ADTVariant'Anon _ _ fields -> snd $ fields List.!! i
get_adt_field_id :: Arena.Arena (ADT ty) ADTKey -> ADTFieldIndex -> ID.ADTFieldID
get_adt_field_id adts (ADTFieldIndex variant i) =
    case get_adt_variant adts variant of
        ADTVariant'Named _ _ fields -> fields List.!! i & \ (id, _, _) -> id
        ADTVariant'Anon _ _ fields -> fst $ fields List.!! i

data TypeSynonym ty = TypeSynonym ID.DeclID (Located Text) ty deriving Show

newtype Var = Var (Located Text) deriving Show -- TODO: put id
