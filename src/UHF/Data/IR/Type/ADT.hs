module UHF.Data.IR.Type.ADT
    ( ADTKey
    , ADT (..)
    , Variant (..)
    , VariantIndex (..)
    , FieldIndex (..)
    , get_variant
    , get_field_type
    , get_field_id
    , variant_idxs
    , variant_name
    , variant_id
    , variant_field_ids
    , variant_field_types
    ) where

import UHF.Prelude

import qualified Data.List as List

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Util.Arena as Arena

-- TODO: don't expose variant index construtor, field index constructor
data ADT ty = ADT ID.DeclID (Located Text) [QuantVarKey] [Variant ty] deriving Show
data Variant ty
    = Variant'Named (Located Text) ID.ADTVariantID [(ID.ADTFieldID, Text, ty)]
    | Variant'Anon (Located Text) ID.ADTVariantID [(ID.ADTFieldID, ty)]
    deriving Show
data VariantIndex = VariantIndex ADTKey Int deriving (Show, Eq, Ord)
data FieldIndex = FieldIndex VariantIndex Int deriving (Show, Eq, Ord)

variant_idxs :: Arena.Arena (ADT ty) ADTKey -> ADTKey -> [VariantIndex]
variant_idxs arena key =
    let (ADT _ _ _ variants) = Arena.get arena key
    in map (VariantIndex key) [0 .. length variants - 1]

variant_name :: Variant ty -> Located Text
variant_name (Variant'Anon name _ _) = name
variant_name (Variant'Named name _ _) = name
variant_id :: Variant ty -> ID.ADTVariantID
variant_id (Variant'Anon _ id _) = id
variant_id (Variant'Named _ id _) = id
variant_field_ids :: Variant ty -> [ID.ADTFieldID]
variant_field_ids (Variant'Anon _ _ fields) = map fst fields
variant_field_ids (Variant'Named _ _ fields) = fields & map (\ (id, _, _) -> id)
variant_field_types :: Variant ty -> [ty]
variant_field_types (Variant'Anon _ _ tys) = map snd tys
variant_field_types (Variant'Named _ _ tys) = tys & map (\ (_, _, ty) -> ty)

-- technically can error, but every VariantIndex constructed should be a valid variant, so hopefully if everything is functioning correctly, this should never fail
get_variant :: Arena.Arena (ADT ty) ADTKey -> VariantIndex -> Variant ty
get_variant adts (VariantIndex key i) =
    let (ADT _ _ _ variants) = Arena.get adts key
    in variants List.!! i

-- hope that field index is legal
get_field_type :: Arena.Arena (ADT ty) ADTKey -> FieldIndex -> ty
get_field_type adts (FieldIndex variant i) =
    case get_variant adts variant of
        Variant'Named _ _ fields -> fields List.!! i & \ (_, _, ty) -> ty
        Variant'Anon _ _ fields -> snd $ fields List.!! i
get_field_id :: Arena.Arena (ADT ty) ADTKey -> FieldIndex -> ID.ADTFieldID
get_field_id adts (FieldIndex variant i) =
    case get_variant adts variant of
        Variant'Named _ _ fields -> fields List.!! i & \ (id, _, _) -> id
        Variant'Anon _ _ fields -> fst $ fields List.!! i
