module UHF.Phases.SolveTypes.Error (Error(..), ErrorTypeContext(..)) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.Unknown
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.PP as Type.PP
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

type UnkNamer = State (Int, Map TypeUnknownKey Text)
run_unk_namer :: UnkNamer a -> (a, Map TypeUnknownKey Text)
run_unk_namer s = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)
make_unk_name_messages :: TypeUnknownArena -> Map TypeUnknownKey Text -> [Diagnostic.Message]
make_unk_name_messages unks names =
    map
        (\ (key, name) ->
            let (TypeUnknown unk_for_what _) = Arena.get unks key
                var_sp = type_unk_for_what_sp unk_for_what
                var_name = type_unk_for_what_name unk_for_what
             in var_sp `Diagnostic.msg_note_at` convert_str ("where '" <> name <> "' is the type of this " <> var_name))
        (Map.toList names)
name_unk :: TypeUnknownKey -> UnkNamer Text
name_unk var = state $
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

data ErrorTypeContext = ErrorTypeContext TypedWithUnkADTArena TypedWithUnkTypeSynonymArena TypeVarArena TypeUnknownArena

data Error
    = EqError
        { eq_error_context :: ErrorTypeContext
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithUnk
        , eq_error_b_whole :: Located TypeWithUnk
        , eq_error_a_part :: TypeWithUnk
        , eq_error_b_part :: TypeWithUnk
        }
    | ExpectError
        { expect_error_context :: ErrorTypeContext
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithUnk
        , expect_error_expect_whole :: TypeWithUnk
        , expect_error_got_part :: TypeWithUnk
        , expect_error_expect_part :: TypeWithUnk
        }

    | OccursCheckError ErrorTypeContext Span TypeUnknownKey TypeWithUnk

    | AmbiguousType TypeUnknownForWhat

    | DoesNotTakeTypeArgument ErrorTypeContext Span TypeWithUnk
    | WrongTypeArgument ErrorTypeContext Span TypeWithUnk TypeWithUnk

instance Diagnostic.ToError Error where
    to_error (EqError context@(ErrorTypeContext _ _ _ unks) in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InMatchPatterns -> "'case' expression patterns"
                InMatchArms -> "'case' expression arms"

            ((a_part_printed, b_part_printed, a_whole_printed, b_whole_printed), var_names) =
                run_unk_namer $ -- TODO: somehow make it so that this can be used without UnkNamer
                    print_type False context a_part >>= \ a_part_printed ->
                    print_type False context b_part >>= \ b_part_printed ->
                    print_type False context (unlocate a_whole) >>= \ a_whole_printed ->
                    print_type False context (unlocate b_whole) >>= \ b_whole_printed ->
                    pure (PP.render a_part_printed, PP.render b_part_printed, PP.render a_whole_printed, PP.render b_whole_printed)
        in Diagnostic.Error
            (Just span)
            ("conflicting types in " <> what <> ": '" <> a_part_printed <> "' vs '" <> b_part_printed <> "'")
            (just_span a_whole `Diagnostic.msg_note_at` convert_str a_whole_printed
                : just_span b_whole `Diagnostic.msg_note_at` convert_str b_whole_printed
                : make_unk_name_messages unks var_names)
            []

    to_error (ExpectError context@(ErrorTypeContext _ _ _ unks) in_what got_whole expect_whole got_part expect_part) =
        let what = case in_what of
                InTypeAnnotation -> "type annotation"
                InIfCondition -> "'if' condition"
                InCallExpr -> "call expression"
                InTypeApplication -> "type application"
                InADTVariantPatternField -> "ADT variant pattern field"

            sp = just_span got_whole

            ((expect_part_printed, got_part_printed, expect_whole_printed, got_whole_printed), var_names) =
                run_unk_namer $
                    print_type False context expect_part >>= \ expect_part_printed ->
                    print_type False context got_part >>= \ got_part_printed ->
                    print_type False context expect_whole >>= \ expect_whole_printed ->
                    print_type False context (unlocate got_whole) >>= \ got_whole_printed ->
                    pure (PP.render expect_part_printed, PP.render got_part_printed, PP.render expect_whole_printed, PP.render got_whole_printed)

        in Diagnostic.Error
            (Just sp)
            ("conflicting types in " <> what <> ": '" <> expect_part_printed <> "' vs '" <> got_part_printed <> "'")
            ((sp `Diagnostic.msg_note_at` convert_str ("expected '" <> expect_whole_printed <> "', got '" <> got_whole_printed <> "'")) : make_unk_name_messages unks var_names)
            []

    to_error (OccursCheckError context@(ErrorTypeContext _ _ _ unks) span var_key ty) =
        let var_as_type = Type.Type'Unknown var_key

            ((ty_printed, var_printed), var_names) =
                run_unk_namer $
                    print_type True context ty >>= \ ty_printed ->
                    print_type True context var_as_type >>= \ var_printed ->
                    pure (PP.render ty_printed, PP.render var_printed)
        in Diagnostic.Error
            (Just span)
            ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> " = " <> ty_printed <> "'")
            (make_unk_name_messages unks var_names)
            []

    to_error (AmbiguousType for_what) =
        let sp = type_unk_for_what_sp for_what
            name = type_unk_for_what_name for_what
        in Diagnostic.Error
                (Just sp)
                ("ambiguous type: could not infer the type of this " <> name)
                []
                []

    to_error (DoesNotTakeTypeArgument context@(ErrorTypeContext _ _ _ unks) sp ty) =
        let (ty_printed, var_names) =
                run_unk_namer $
                    print_type False context ty >>= \ ty_printed ->
                    pure (PP.render ty_printed)
        in Diagnostic.Error
            (Just sp)
            -- TODO: type '' of kind does not take a type argument
            ("type '" <> ty_printed <> "' does not accept a type argument")
            (make_unk_name_messages unks var_names)
            []

    to_error (WrongTypeArgument context@(ErrorTypeContext _ _ _ unks) sp ty arg) =
        let ((ty_printed, arg_printed), var_names) =
                run_unk_namer $
                    print_type False context ty >>= \ ty_printed ->
                    print_type False context arg >>= \ arg_printed ->
                    pure (PP.render ty_printed, PP.render arg_printed)
        in Diagnostic.Error
            (Just sp)
            -- TODO: type '' of kind '' expects type argument of kind '', but got argument of kind ''
            ("type '" <> ty_printed <> "' does not accept a type argument '" <> arg_printed <> "'")
            (make_unk_name_messages unks var_names)
            []

print_type :: Bool -> ErrorTypeContext -> TypeWithUnk -> UnkNamer PP.Token -- TODO: since this already a monad, put the arenas and things into a reader monad?
print_type unks_show_index context@(ErrorTypeContext adts type_synonyms vars unks) = Type.PP.refer_type_m show_unk adts type_synonyms vars
    where
        show_unk unk =
            case Arena.get unks unk of
                TypeUnknown _ Fresh
                    | unks_show_index -> PP.String <$> name_unk unk
                    | otherwise -> pure "_"
                TypeUnknown _ (Substituted other) -> print_type unks_show_index context other

