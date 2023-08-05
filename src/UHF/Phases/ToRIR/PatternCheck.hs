module UHF.Phases.ToRIR.PatternCheck
    ( CompletenessError (..)
    , NotUsefulError (..)
    , MatchValue (..)
    , check_complete
    , check_useful
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.IO.Span (Span)

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.ID as ID

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.List as List
import qualified Data.Text as Text

data CompletenessError = CompletenessError (Arena.Arena (Type.ADT Type) Type.ADTKey) Span [Pattern] [MatchValue]
data NotUsefulError = NotUsefulError Pattern

instance Diagnostic.ToError CompletenessError where
    to_error (CompletenessError adt_arena sp pats left_over) =
        Diagnostic.Error Diagnostic.Codes.incomplete_patterns (Just sp) "incomplete patterns"
            ( (Just sp, Diagnostic.MsgNote, Just $ "values not matched: " <> Text.intercalate ", " (map (show_match_value adt_arena) left_over)) :
                 map (\ pat -> (Just $ SIR.pattern_span pat, Diagnostic.MsgNote, Nothing)) pats
            )
            []

instance Diagnostic.ToError NotUsefulError where
    to_error (NotUsefulError pat) = Diagnostic.Error Diagnostic.Codes.useless_pattern (Just $ SIR.pattern_span pat) "useless pattern" [] []

type Type = Maybe (Type.Type Void)
type Pattern = SIR.Pattern (Maybe Type.ADTVariantIndex) Type

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

-- TODO: this function assumes that all the pattern types are correct which might not always happen because things can pass the typechecking phase with invalid types that become Nothing; might cause internal error? - fix?
check :: Arena.Arena (Type.ADT Type) Type.ADTKey -> [Pattern] -> ([MatchValue], [(Pattern, [MatchValue])])
check adt_arena patterns = mapAccumL check_one_pattern [Any] patterns
    where
        check_one_pattern :: [MatchValue] -> Pattern -> ([MatchValue], (Pattern, [MatchValue]))
        check_one_pattern uncovered cur_pattern =
            let (still_uncovered, covered) = unzip $ map (check_against_one_uncovered_value cur_pattern) uncovered
            in (filter is_valid_match_value $ concat still_uncovered, (cur_pattern, filter is_valid_match_value $ concat covered))

        check_against_one_uncovered_value :: Pattern -> MatchValue -> ([MatchValue], [MatchValue])
        check_against_one_uncovered_value (SIR.Pattern'Identifier ty _ _) uncovered_value = check_wild ty uncovered_value
        check_against_one_uncovered_value (SIR.Pattern'Wildcard ty _) uncovered_value = check_wild ty uncovered_value

        check_against_one_uncovered_value (SIR.Pattern'Named _ _ _ _ subpat) uncovered_value = check_against_one_uncovered_value subpat uncovered_value

        check_against_one_uncovered_value (SIR.Pattern'Tuple _ _ field_a_pat field_b_pat) uncovered_value =
            case uncovered_value of
                -- the fields of '_ : (A, B)' are '[_ : A, _ : B]'
                Any -> go Any Any
                Tuple uncovered_a uncovered_b -> go uncovered_a uncovered_b

                AnonADT _ _ -> error "checking tuple pattern against adt uncovered value"
            where
                make_into_tuple [a, b] = Tuple a b
                make_into_tuple _ = error "cannot make field combo of not 2 fields into a tuple"

                go uncovered_a uncovered_b =
                    let (uncovered_field_combos, covered_field_combos) = check_fields [field_a_pat, field_b_pat] [uncovered_a, uncovered_b]
                    in (map make_into_tuple uncovered_field_combos, map make_into_tuple covered_field_combos)

        check_against_one_uncovered_value (SIR.Pattern'AnonADTVariant (Just ty) _ (Just pat_variant) _ pat_fields) uncovered_value =
            case uncovered_value of
                Any ->
                    let (still_uncovered, covered) = unzip $ enumerate_adt_ctors_and_fields ty & map (uncurry go)
                    in (concat still_uncovered, concat covered)
                AnonADT uncovered_variant uncovered_fields -> go uncovered_variant uncovered_fields

                Tuple _ _ -> error "checking anonymous adt pattern against tuple uncovered value"

            where
                enumerate_adt_ctors_and_fields (Type.Type'ADT adt_key _) =
                    Type.adt_variant_idxs adt_arena adt_key &
                        map (\ variant_idx ->
                            let variant = Type.get_adt_variant adt_arena variant_idx
                                fields = Type.variant_field_types variant
                            in (variant_idx, map (const Any) fields)
                        )
                enumerate_adt_ctors_and_fields _ = error "cannot enumerate adt variants of non-adt type"

                go uncovered_variant uncovered_fields
                    | uncovered_variant == pat_variant =
                        let (uncovered_field_combos, covered_field_combos) = check_fields pat_fields uncovered_fields
                        in (map (AnonADT uncovered_variant) uncovered_field_combos, map (AnonADT uncovered_variant) covered_field_combos)
                    | otherwise = ([AnonADT uncovered_variant uncovered_fields], []) -- if the constructors do not match, the pattern will cover nothing
        check_against_one_uncovered_value (SIR.Pattern'AnonADTVariant _ _ _ _ _) uncovered_value = ([uncovered_value], []) -- a variant pattern with an unknown variant or an unknown type does not cover anything

        check_against_one_uncovered_value (SIR.Pattern'NamedADTVariant (Just ty) _ (Just variant) _ fields) uncovered_value = todo
        check_against_one_uncovered_value (SIR.Pattern'NamedADTVariant _ _ _ _ _) uncovered_value = ([uncovered_value], []) -- same as above: a variant pattern with an unknown variant or an unknown type does not cover anything

        check_against_one_uncovered_value (SIR.Pattern'Poison _ _) uncovered_value = ([uncovered_value], []) -- poison pattern behaves as if it doesnt cover anything

        check_wild _ uncovered_value = ([], [uncovered_value]) -- wildcard pattern always covers everything

        check_fields field_pats uncovered_value_fields = (uncovered_field_combos, covered_field_combos)
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
                    -- this works because for a field combo to be uncovered, it only needs 1 of its field to not match
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

check_complete :: Arena.Arena (Type.ADT Type) Type.ADTKey -> Span -> [Pattern] -> Either CompletenessError ()
check_complete adt_arena err_sp patterns =
    let (left_over, _) = check adt_arena patterns
    in if null left_over
        then Right ()
        else Left $ CompletenessError adt_arena err_sp patterns left_over

check_useful :: Arena.Arena (Type.ADT Type) Type.ADTKey -> [Pattern] -> Either [NotUsefulError] ()
check_useful adt_arena patterns =
    let (_, patterns') = check adt_arena patterns
        errs = mapMaybe (\ (pat, covers) -> if null covers then Just (NotUsefulError pat) else Nothing) patterns'
    in if null errs
        then Right ()
        else Left errs
