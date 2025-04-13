module UHF.Data.IR.Type.ADT
    ( ADTKey
    , ADT (..)
    , Variant (..)
    , VariantIndex (..)
    , FieldIndex (..)
    , get_variant
    , get_field_type
    -- TODO: REMOVE?
    -- , get_field_id
    , variant_idxs
    , variant_field_idxs
    , variant_name
    -- TODO: REMOVE?
    -- , variant_id
    -- , variant_field_ids
    , variant_field_types
    ) where

import UHF.Prelude

import qualified Data.List as List

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

data DoNotConstruct = DoNotConstruct deriving (Show, Eq, Ord)

data ADT ty = ADT Text [QuantVarKey] [Variant ty] deriving Show
data Variant ty
    = Variant'Named Text [(Text, ty)]
    | Variant'Anon Text [ty]
    deriving Show
data VariantIndex = VariantIndex DoNotConstruct ADTKey Int deriving (Show, Eq, Ord)
data FieldIndex = FieldIndex DoNotConstruct VariantIndex Int deriving (Show, Eq, Ord)

variant_idxs :: Arena.Arena (ADT ty) ADTKey -> ADTKey -> [VariantIndex]
variant_idxs arena key =
    let (ADT _ _ variants) = Arena.get arena key
    in map (VariantIndex DoNotConstruct key) [0 .. length variants - 1]
variant_field_idxs :: Arena.Arena (ADT ty) ADTKey -> VariantIndex -> [FieldIndex]
variant_field_idxs arena v_idx =
    let variant = get_variant arena v_idx
    in case variant of
        (Variant'Anon _ fields) -> fields & zipWith (\ i _ -> FieldIndex DoNotConstruct v_idx i) [0..]
        (Variant'Named _ fields) -> fields & zipWith (\ i _ -> FieldIndex DoNotConstruct v_idx i) [0..]

variant_name :: Variant ty -> Text
variant_name (Variant'Anon name _) = name
variant_name (Variant'Named name _) = name
-- TODO: REMOVE?
-- variant_id :: Variant ty -> ID.ADTVariantID
-- variant_id (Variant'Anon _ id _) = id
-- variant_id (Variant'Named _ id _) = id
-- variant_field_ids :: Variant ty -> [ID.ADTFieldID]
-- variant_field_ids (Variant'Anon _ _ fields) = map fst fields
-- variant_field_ids (Variant'Named _ _ fields) = fields & map (\ (id, _, _) -> id)
variant_field_types :: Variant ty -> [ty]
variant_field_types (Variant'Anon _ tys) = tys
variant_field_types (Variant'Named _ tys) = map snd tys

-- technically is partial, but because VariantIndexes cannot be constructed outside of this module and this module is careful to only construct them to valid variants, this should hopefully never error in practice
get_variant :: Arena.Arena (ADT ty) ADTKey -> VariantIndex -> Variant ty
get_variant adts (VariantIndex _ key i) =
    let (ADT _ _ variants) = Arena.get adts key
    in variants List.!! i

-- same note about partial but should not be as above
get_field_type :: Arena.Arena (ADT ty) ADTKey -> FieldIndex -> ty
get_field_type adts (FieldIndex _ variant i) =
    case get_variant adts variant of
        Variant'Named _ fields -> snd $ fields List.!! i
        Variant'Anon _ fields -> fields List.!! i
-- TODO: REMOVE?
-- get_field_id :: Arena.Arena (ADT ty) ADTKey -> FieldIndex -> ID.ADTFieldID
-- get_field_id adts (FieldIndex _ variant i) =
--     case get_variant adts variant of
--         Variant'Named _ fields -> fields List.!! i & \ (id, _, _) -> id
--         Variant'Anon _ fields -> fst $ fields List.!! i
