module UHF.Data.SIR.ADT.Utils
    ( get_variant
    , get_field_type
    , get_field_id
    , variant_idxs
    , variant_field_idxs
    , variant_name
    , variant_id
    , variant_field_ids
    , variant_field_types
    ) where

import UHF.Prelude

import qualified Data.List as List

import qualified UHF.Data.IR.ID as ID
import UHF.Data.IR.Keys
import qualified UHF.Data.SIR as SIR
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

variant_idxs :: Arena.Arena (SIR.ADT stage) ADTKey -> ADTKey -> [VariantIndex]
variant_idxs arena key =
    let (SIR.ADT _ _ _ variants) = Arena.get arena key
    in map (VariantIndex DoNotConstruct key) [0 .. length variants - 1]
variant_field_idxs :: Arena.Arena (SIR.ADT stage) ADTKey -> VariantIndex -> [FieldIndex]
variant_field_idxs arena v_idx =
    let variant = get_variant arena v_idx
    in case variant of
        (SIR.ADTVariant'Anon _ _ fields) -> fields & zipWith (\i _ -> FieldIndex DoNotConstruct v_idx i) [0 ..]
        (SIR.ADTVariant'Named _ _ fields) -> fields & zipWith (\i _ -> FieldIndex DoNotConstruct v_idx i) [0 ..]

variant_name :: SIR.ADTVariant stage -> Located Text
variant_name (SIR.ADTVariant'Anon name _ _) = name
variant_name (SIR.ADTVariant'Named name _ _) = name
variant_id :: SIR.ADTVariant stage -> ID.ADTVariantID
variant_id (SIR.ADTVariant'Anon _ id _) = id
variant_id (SIR.ADTVariant'Named _ id _) = id
variant_field_ids :: SIR.ADTVariant stage -> [ID.ADTFieldID]
variant_field_ids (SIR.ADTVariant'Anon _ _ fields) = map (\(id, _, _) -> id) fields
variant_field_ids (SIR.ADTVariant'Named _ _ fields) = map (\(id, _, _, _) -> id) fields
variant_field_types :: SIR.ADTVariant stage -> [SIR.TypeExprEvaledAsTypeKey stage]
variant_field_types (SIR.ADTVariant'Anon _ _ tys) = map (\(_, _, ty) -> ty) tys
variant_field_types (SIR.ADTVariant'Named _ _ tys) = map (\(_, _, _, ty) -> ty) tys

-- technically is partial, but because VariantIndexes cannot be constructed outside of this module and this module is careful to only construct them to valid variants, this should hopefully never error in practice
get_variant :: Arena.Arena (SIR.ADT stage) ADTKey -> VariantIndex -> SIR.ADTVariant stage
get_variant adts (VariantIndex _ key i) =
    let (SIR.ADT _ _ _ variants) = Arena.get adts key
    in variants List.!! i

-- same note about partial but should not be as above
get_field_type :: Arena.Arena (SIR.ADT stage) ADTKey -> FieldIndex -> SIR.TypeExprEvaledAsTypeKey stage
get_field_type adts (FieldIndex _ variant i) =
    case get_variant adts variant of
        SIR.ADTVariant'Named _ _ fields -> fields List.!! i & \(_, _, _, ty) -> ty
        SIR.ADTVariant'Anon _ _ fields -> fields List.!! i & \(_, _, ty) -> ty
get_field_id :: Arena.Arena (SIR.ADT stage) ADTKey -> FieldIndex -> ID.ADTFieldID
get_field_id adts (FieldIndex _ variant i) =
    case get_variant adts variant of
        SIR.ADTVariant'Named _ _ fields -> fields List.!! i & \(id, _, _, _) -> id
        SIR.ADTVariant'Anon _ _ fields -> fields List.!! i & \(id, _, _) -> id
