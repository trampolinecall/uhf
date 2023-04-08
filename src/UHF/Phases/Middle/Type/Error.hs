module UHF.Phases.Middle.Type.Error (Error(..)) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP

import qualified UHF.PPUtils as PPUtils

import qualified Data.Map as Map

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import UHF.Phases.Middle.Type.Unknown
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Constraint

type VarNamer = State (Int, Map TypeUnknownKey Text)
run_var_namer :: VarNamer a -> (a, Map TypeUnknownKey Text)
run_var_namer s = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)
make_var_name_messages :: TypeUnknownArena -> Map TypeUnknownKey Text -> [Diagnostic.Message]
make_var_name_messages vars names =
    map
        (\ (key, name) ->
            let (TypeUnknown var_for_what _) = Arena.get vars key
                var_sp = type_var_for_what_sp var_for_what
                var_name = type_var_for_what_name var_for_what
             in var_sp `Diagnostic.msg_note_at` convert_str ("where '" <> name <> "' is the type of this " <> var_name))
        (Map.toList names)
name_var :: TypeUnknownKey -> VarNamer Text
name_var var = state $
    \ (cur_n, cache) ->
        case Map.lookup var cache of
            Just res -> (res, (cur_n, cache))
            Nothing -> let name = "T" <> show cur_n in (name, (cur_n + 1, Map.insert var name cache))
                {- TODO: make this work correctly
                -- print like a base-26 number system
                -- goes 'A', 'B', 'C', ..., 'Y', 'Z', 'AA', 'AB', 'AC', ..., 'AY', 'AZ', 'BA', 'BB', 'BC', ...
                let name = Text.pack $ map to_characters $ reverse $ make_name [] cur_n
                in (name, (cur_n + 1, Map.insert var name cache))
    where
        make_name acc n
            | n > 0 =
                make_name ((n `mod` 26) : acc) (n `div` 26)
            | otherwise = acc

        to_characters n = chr $ 65 + n
    -}

data Error
    = EqError
        { eq_error_adts :: TypedWithUnkADTArena
        , eq_error_type_synonyms :: TypedWithUnkTypeSynonymArena
        , eq_error_vars :: TypeUnknownArena
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithUnk
        , eq_error_b_whole :: Located TypeWithUnk
        , eq_error_a_part :: TypeWithUnk
        , eq_error_b_part :: TypeWithUnk
        }
    | ExpectError
        { expect_error_adts :: TypedWithUnkADTArena
        , expect_error_type_synonyms :: TypedWithUnkTypeSynonymArena
        , expect_error_vars :: TypeUnknownArena
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithUnk
        , expect_error_expect_whole :: TypeWithUnk
        , expect_error_got_part :: TypeWithUnk
        , expect_error_expect_part :: TypeWithUnk
        }

    | OccursCheckError TypedWithUnkADTArena TypedWithUnkTypeSynonymArena TypeUnknownArena Span TypeUnknownKey TypeWithUnk

    | AmbiguousType TypeUnknownForWhat

    | NotAType Span Text

instance Diagnostic.ToError Error where
    to_error (EqError adts type_synonyms vars in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InCasePatterns -> "'case' expression patterns"
                InCaseArms -> "'case' expression arms"

            ((a_part_printed, b_part_printed, a_whole_printed, b_whole_printed), var_names) =
                run_var_namer $ -- TODO: somehow make it so that this can be used without VarNamer
                    print_type False adts type_synonyms vars a_part >>= \ a_part_printed ->
                    print_type False adts type_synonyms vars b_part >>= \ b_part_printed ->
                    print_type False adts type_synonyms vars (unlocate a_whole) >>= \ a_whole_printed ->
                    print_type False adts type_synonyms vars (unlocate b_whole) >>= \ b_whole_printed ->
                    pure (PPUtils.exec_pp a_part_printed, PPUtils.exec_pp b_part_printed, PPUtils.exec_pp a_whole_printed, PPUtils.exec_pp b_whole_printed)
        in Diagnostic.Error Diagnostic.Codes.type_mismatch
            (Just span)
            ("conflicting types in " <> what <> ": '" <> a_part_printed <> "' vs '" <> b_part_printed <> "'")
            (just_span a_whole `Diagnostic.msg_note_at` convert_str a_whole_printed
                : just_span b_whole `Diagnostic.msg_note_at` convert_str b_whole_printed
                : make_var_name_messages vars var_names)
            []

    to_error (ExpectError adts type_synonyms vars in_what got_whole expect_whole got_part expect_part) =
        let what = case in_what of
                InTypeAnnotation -> "type annotation"
                InIfCondition -> "'if' condition"
                InCallExpr -> "call expression"

            sp = just_span got_whole

            ((expect_part_printed, got_part_printed, expect_whole_printed, got_whole_printed), var_names) =
                run_var_namer $
                    print_type False adts type_synonyms vars expect_part >>= \ expect_part_printed ->
                    print_type False adts type_synonyms vars got_part >>= \ got_part_printed ->
                    print_type False adts type_synonyms vars expect_whole >>= \ expect_whole_printed ->
                    print_type False adts type_synonyms vars (unlocate got_whole) >>= \ got_whole_printed ->
                    pure (PPUtils.exec_pp expect_part_printed, PPUtils.exec_pp got_part_printed, PPUtils.exec_pp expect_whole_printed, PPUtils.exec_pp got_whole_printed)

        in Diagnostic.Error Diagnostic.Codes.type_mismatch
            (Just sp)
            ("conflicting types in " <> what <> ": '" <> expect_part_printed <> "' vs '" <> got_part_printed <> "'")
            ((sp `Diagnostic.msg_note_at` convert_str ("expected '" <> expect_whole_printed <> "', got '" <> got_whole_printed <> "'")) : make_var_name_messages vars var_names)
            []

    to_error (OccursCheckError adts type_synonyms vars span var_key ty) =
        let var_as_type = Type.Type'Unknown var_key

            ((ty_printed, var_printed), var_names) =
                run_var_namer $
                    print_type True adts type_synonyms vars ty >>= \ ty_printed ->
                    print_type True adts type_synonyms vars var_as_type >>= \ var_printed ->
                    pure (PPUtils.exec_pp ty_printed, PPUtils.exec_pp var_printed)
        in Diagnostic.Error Diagnostic.Codes.occurs_check
            (Just span)
            ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> " = " <> ty_printed <> "'")
            (make_var_name_messages vars var_names)
            []

    to_error (AmbiguousType for_what) =
        let sp = type_var_for_what_sp for_what
            name = type_var_for_what_name for_what
        in Diagnostic.Error Diagnostic.Codes.ambiguous_type
                (Just sp)
                ("ambiguous type: could not infer the type of this " <> name)
                []
                []

    to_error (NotAType sp instead) = Diagnostic.Error Diagnostic.Codes.not_a_type (Just sp) ("not a type: got " <> instead) [] []

print_type :: Bool -> TypedWithUnkADTArena -> TypedWithUnkTypeSynonymArena -> TypeUnknownArena -> TypeWithUnk -> VarNamer (PPUtils.PP ()) -- TODO: since this already a monad, put the arenas and things into a reader monad?
print_type vars_show_index adts type_synonyms vars = Type.PP.refer_type_m show_var adts type_synonyms
    where
        show_var var =
            case Arena.get vars var of
                TypeUnknown _ Fresh
                    | vars_show_index -> PPUtils.write <$> name_var var
                    | otherwise -> pure $ PPUtils.write "_"
                TypeUnknown _ (Substituted other) -> print_type vars_show_index adts type_synonyms vars other

