{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UHF.Phases.ToRIR.PatternCheck
    ( CompletenessError (..)
    , NotUseful (..)
    , MatchValue (..)
    , check_complete
    , check_useful
    ) where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Text as Text

import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Diagnostic as Diagnostic

data CompletenessError stage = CompletenessError (Arena.Arena (Type.ADT Type) Type.ADTKey) Span [SIR.Pattern stage] [MatchValue]
data NotUseful stage = NotUseful (SIR.Pattern stage)

instance Diagnostic.ToError (CompletenessError stage) where
    to_error (CompletenessError adt_arena sp pats left_over) =
        Diagnostic.Error (Just sp) "incomplete patterns"
            ( (Just sp, Diagnostic.MsgError, Just $ "values not matched: " <> Text.intercalate ", " (map (show_match_value adt_arena) left_over)) :
                 map (\ pat -> (Just $ SIR.pattern_span pat, Diagnostic.MsgNote, Nothing)) pats
            )
            []

instance Diagnostic.ToWarning (NotUseful stage) where
    to_warning (NotUseful pat) = Diagnostic.Warning (Just $ SIR.pattern_span pat) "useless pattern" [] []

type Type = Maybe (Type.Type Void)
type CorrectStage s = (SIR.TypeInfo s ~ Type, SIR.PIdenResolved s ~ Maybe Type.ADTVariantIndex)

data MatchValue
    = Any
    | AnonADT Type.ADTVariantIndex [MatchValue]
    | Tuple MatchValue MatchValue

show_match_value :: Arena.Arena (Type.ADT Type) Type.ADTKey -> MatchValue -> Text
show_match_value _ Any = "_"
show_match_value adt_arena (AnonADT variant fields) =
    let refer_variant = Type.get_adt_variant adt_arena variant & Type.variant_id & ID.stringify
    in refer_variant <> "(" <> Text.intercalate ", " (map (show_match_value adt_arena) fields) <> ")"
show_match_value adt_arena (Tuple a b) = "(" <> show_match_value adt_arena a <> ", " <> show_match_value adt_arena b <> ")" -- TODO: print multi-element tuples better

-- TODO: clean this up
-- TODO: use actual type of expression being matched against, not types of patterns
check :: forall stage. CorrectStage stage => Arena.Arena (Type.ADT Type) Type.ADTKey -> Arena.Arena (Type.TypeSynonym Type) Type.TypeSynonymKey -> [SIR.Pattern stage] -> ([MatchValue], [(SIR.Pattern stage, [MatchValue])])
check adt_arena type_synonym_arena = mapAccumL check_one_pattern [Any]
    where
        check_one_pattern :: [MatchValue] -> SIR.Pattern stage -> ([MatchValue], (SIR.Pattern stage, [MatchValue]))
        check_one_pattern uncovered cur_pattern =
            let (still_uncovered, covered) = unzip $ map (check_against_one_uncovered_value cur_pattern) uncovered
            -- TODO: filter duplicates, also almost duplicates where _ and constructors are included (as of 2023-11-21, i am not sure what this todo is talking about)
            in (filter is_valid_match_value $ concat still_uncovered, (cur_pattern, filter is_valid_match_value $ concat covered))

        -- first list of tuple is all of the unmatched values
        -- second list of tuple is all of the matched values
        check_against_one_uncovered_value :: SIR.Pattern stage -> MatchValue -> ([MatchValue], [MatchValue])
        check_against_one_uncovered_value (SIR.Pattern'Identifier ty _ _) uncovered_value = check_wild ty uncovered_value
        check_against_one_uncovered_value (SIR.Pattern'Wildcard ty _) uncovered_value = check_wild ty uncovered_value

        check_against_one_uncovered_value (SIR.Pattern'Named _ _ _ _ subpat) uncovered_value = check_against_one_uncovered_value subpat uncovered_value

        check_against_one_uncovered_value (SIR.Pattern'Tuple _ _ field_a_pat field_b_pat) uncovered_value =
            case uncovered_value of
                -- the fields of '_ : (A, B)' are '[_ : A, _ : B]'
                Any -> go Any Any
                Tuple uncovered_a uncovered_b -> go uncovered_a uncovered_b

                AnonADT _ _ -> ([uncovered_value], []) -- is not an illegal state because things are allowed to pass to this stage even if they have type errors; just treat it like a poison pattern

            where
                make_into_tuple [a, b] = Tuple a b
                make_into_tuple _ = error "cannot make field combo of not 2 fields into a tuple"

                go uncovered_a uncovered_b =
                    let (uncovered_field_combos, covered_field_combos) = check_fields [field_a_pat, field_b_pat] [uncovered_a, uncovered_b]
                    in (map make_into_tuple uncovered_field_combos, map make_into_tuple covered_field_combos) -- make_into_tuple will not error because the field combos must be 2 fields long

        check_against_one_uncovered_value (SIR.Pattern'AnonADTVariant (Just ty) _ _ (Just pat_variant) _ pat_fields) uncovered_value =
            case uncovered_value of
                Any ->
                    let (still_uncovered, covered) = enumerate_adt_ctors_and_fields ty & map (uncurry go) & unzip
                    in (concat still_uncovered, concat covered)
                AnonADT uncovered_variant uncovered_fields -> go uncovered_variant uncovered_fields

                Tuple _ _ -> ([uncovered_value], []) -- like above: not illegal state; treat like a poison pattern

            where
                enumerate_adt_ctors_and_fields (Type.Type'ADT adt_key _) =
                    Type.adt_variant_idxs adt_arena adt_key &
                        map (\ variant_idx ->
                            let variant = Type.get_adt_variant adt_arena variant_idx
                                fields = Type.variant_field_types variant
                            in (variant_idx, map (const Any) fields)
                        )
                enumerate_adt_ctors_and_fields (Type.Type'Synonym ts_key) =
                    let (Type.TypeSynonym _ _ expansion) = Arena.get type_synonym_arena ts_key
                    in maybe [] enumerate_adt_ctors_and_fields expansion -- in the case that the type synonym had a type error and is Nothing, treat it like it has no constructors / like an uninhabited type
                enumerate_adt_ctors_and_fields Type.Type'Int = error_for_enumerate_adt_ctors_and_fields "Type'Int"
                enumerate_adt_ctors_and_fields Type.Type'Float = error_for_enumerate_adt_ctors_and_fields "Type'Float"
                enumerate_adt_ctors_and_fields Type.Type'Char = error_for_enumerate_adt_ctors_and_fields "Type'Char"
                enumerate_adt_ctors_and_fields Type.Type'String = error_for_enumerate_adt_ctors_and_fields "Type'String"
                enumerate_adt_ctors_and_fields Type.Type'Bool = error_for_enumerate_adt_ctors_and_fields "Type'Bool"
                enumerate_adt_ctors_and_fields (Type.Type'Function _ _) = error_for_enumerate_adt_ctors_and_fields "Type'Function"
                enumerate_adt_ctors_and_fields (Type.Type'Tuple _ _) = error_for_enumerate_adt_ctors_and_fields "Type'Tuple"
                enumerate_adt_ctors_and_fields (Type.Type'Unknown v) = absurd v
                enumerate_adt_ctors_and_fields (Type.Type'Variable _) = error_for_enumerate_adt_ctors_and_fields "Type'Variable"
                enumerate_adt_ctors_and_fields (Type.Type'Forall _ _) = error_for_enumerate_adt_ctors_and_fields "Type'Forall"

                error_for_enumerate_adt_ctors_and_fields ty = error $ "cannot enumerate adt constructors for " ++ ty

                go uncovered_variant uncovered_fields
                    | uncovered_variant == pat_variant =
                        let (uncovered_field_combos, covered_field_combos) = check_fields pat_fields uncovered_fields
                        in (map (AnonADT uncovered_variant) uncovered_field_combos, map (AnonADT uncovered_variant) covered_field_combos)
                    | otherwise = ([AnonADT uncovered_variant uncovered_fields], []) -- if the constructors do not match, the pattern will cover nothing
        check_against_one_uncovered_value (SIR.Pattern'AnonADTVariant _ _ _ _ _ _) uncovered_value = ([uncovered_value], []) -- a variant pattern with an unknown variant or an unknown type does not cover anything

        check_against_one_uncovered_value (SIR.Pattern'NamedADTVariant (Just ty) _ _ (Just variant) _ fields) uncovered_value = todo
        check_against_one_uncovered_value (SIR.Pattern'NamedADTVariant _ _ _ _ _ _) uncovered_value = ([uncovered_value], []) -- same as above: a variant pattern with an unknown variant or an unknown type does not cover anything

        check_against_one_uncovered_value (SIR.Pattern'Poison _ _) uncovered_value = ([uncovered_value], []) -- poison pattern behaves as if it doesnt cover anything

        check_wild _ uncovered_value = ([], [uncovered_value]) -- wildcard pattern always covers everything

        check_fields field_pats uncovered_value_fields = assert (length field_pats == length uncovered_value_fields) "must have the same number of field patterns and uncovered value fields" (uncovered_field_combos, covered_field_combos)
            where
                (fields_uncovered, fields_covered) = unzip $ zipWith check_against_one_uncovered_value field_pats uncovered_value_fields

                uncovered_field_combos =
                    -- go through every uncovered field and substitute that field into the original fields
                    -- for example:
                    -- if we start with the uncovered fields being [F1, F2] and we are checking those against the field patterns [P1, P2]
                    -- lets say that P1 doesn't cover [Q1] and P2 doesn't cover [Q2, R2]
                    -- we start with Q1, take the original field combo [F1, F2], and replace F1 with Q1
                    -- and then we move on to Q2, take the original field combo [F1, F2], and replace F2 with Q2
                    -- finally, we move on to R2, so we replace F2 in the original field combo with R2
                    -- the resulting field combos would be [Q1, F2], [F1, Q2], [F1, R2]
                    -- this works because for a field combo to be uncovered, it only needs 1 of its fields to not match
                    fields_uncovered
                        & zipWith
                            (\ field_i uncovered_possibilities ->
                                map
                                    (\ uncovered_possibility -> List.take field_i uncovered_value_fields ++ [uncovered_possibility] ++ List.drop (field_i + 1) uncovered_value_fields)
                                    uncovered_possibilities
                            )
                            [0..]
                        & concat

                covered_field_combos =
                    -- go through every possibly combination of covered fields (like a cartesian product)
                    -- for example
                    -- if we start with field patterns [P1, P2]
                    -- and P1 matches [V1, V2] and P2 matches [U1, U2, U3]
                    -- we take the cartesian product and the resulting combos are [V1, U1], [V1, U2], [V1, U3], [V2, U1], [V2, U2], [V2, U3]
                    -- this is because in order for a field combo to match the field patterns, all the fields must match the corresponding pattern
                    -- sequence can compute the cartesian product by taking advantage of the List >>=
                    sequence fields_covered

        is_valid_match_value mv = True -- TODO: not (has_uninhabited_values mv)

check_complete :: CorrectStage stage => Arena.Arena (Type.ADT Type) Type.ADTKey -> Arena.Arena (Type.TypeSynonym Type) Type.TypeSynonymKey -> Span -> [SIR.Pattern stage] -> Either (CompletenessError stage) ()
check_complete adt_arena type_synonym_arena err_sp patterns =
    let (left_over, _) = check adt_arena type_synonym_arena patterns
    in if null left_over
        then Right ()
        else Left $ CompletenessError adt_arena err_sp patterns left_over

check_useful :: CorrectStage stage => Arena.Arena (Type.ADT Type) Type.ADTKey -> Arena.Arena (Type.TypeSynonym Type) Type.TypeSynonymKey -> [SIR.Pattern stage] -> Either [NotUseful stage] ()
check_useful adt_arena type_synonym_arena patterns =
    let (_, patterns') = check adt_arena type_synonym_arena patterns
        warns = mapMaybe (\ (pat, covers) -> if null covers then Just (NotUseful pat) else Nothing) patterns'
    in if null warns
        then Right ()
        else Left warns
