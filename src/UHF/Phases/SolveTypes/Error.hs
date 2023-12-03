module UHF.Phases.SolveTypes.Error (Error(..), ErrorTypeContext(..)) where

import UHF.Prelude

import qualified Data.Map as Map

import UHF.Phases.SolveTypes.Aliases
import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.TypeWithInferVar
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP
import qualified UHF.Util.Arena as Arena

type InferVarNamer = State (Int, Map InferVarKey Text)
run_infer_var_namer :: InferVarNamer a -> (a, Map InferVarKey Text)
run_infer_var_namer s = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)
make_infer_var_name_messages :: InferVarArena -> Map InferVarKey Text -> [Diagnostic.Message]
make_infer_var_name_messages infer_vars names =
    map
        (\ (key, name) ->
            let (InferVar infer_var_for_what _) = Arena.get infer_vars key
                var_sp = infer_var_for_what_sp infer_var_for_what
                var_name = infer_var_for_what_name infer_var_for_what
             in var_sp `Diagnostic.msg_note_at` convert_str ("where '" <> name <> "' is the type of this " <> var_name))
        (Map.toList names)
name_infer_var :: InferVarKey -> InferVarNamer Text
name_infer_var var = state $
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

data ErrorTypeContext = ErrorTypeContext TypedWithInferVarsADTArena TypedWithInferVarsTypeSynonymArena QuantVarArena InferVarArena

data Error
    = EqError
        { eq_error_context :: ErrorTypeContext
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithInferVars
        , eq_error_b_whole :: Located TypeWithInferVars
        , eq_error_a_part :: TypeWithInferVars
        , eq_error_b_part :: TypeWithInferVars
        }
    | ExpectError
        { expect_error_context :: ErrorTypeContext
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithInferVars
        , expect_error_expect_whole :: TypeWithInferVars
        , expect_error_got_part :: TypeWithInferVars
        , expect_error_expect_part :: TypeWithInferVars
        }

    | OccursCheckError ErrorTypeContext Span InferVarKey TypeWithInferVars

    | AmbiguousType InferVarForWhat

    | DoesNotTakeTypeArgument ErrorTypeContext Span TypeWithInferVars
    | WrongTypeArgument ErrorTypeContext Span TypeWithInferVars TypeWithInferVars

instance Diagnostic.ToError Error where
    to_error (EqError context@(ErrorTypeContext _ _ _ infer_vars) in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InMatchPatterns -> "'case' expression patterns"
                InMatchArms -> "'case' expression arms"

            ((a_part_printed, b_part_printed, a_whole_printed, b_whole_printed), var_names) =
                run_infer_var_namer $ -- TODO: somehow make it so that this can be used without InferVarNamer
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
                : make_infer_var_name_messages infer_vars var_names)
            []

    to_error (ExpectError context@(ErrorTypeContext _ _ _ infer_vars) in_what got_whole expect_whole got_part expect_part) =
        let what = case in_what of
                InTypeAnnotation -> "type annotation"
                InIfCondition -> "'if' condition"
                InCallExpr -> "call expression"
                InTypeApplication -> "type application"
                InADTVariantPatternField -> "ADT variant pattern field"

            sp = just_span got_whole

            ((expect_part_printed, got_part_printed, expect_whole_printed, got_whole_printed), var_names) =
                run_infer_var_namer $
                    print_type False context expect_part >>= \ expect_part_printed ->
                    print_type False context got_part >>= \ got_part_printed ->
                    print_type False context expect_whole >>= \ expect_whole_printed ->
                    print_type False context (unlocate got_whole) >>= \ got_whole_printed ->
                    pure (PP.render expect_part_printed, PP.render got_part_printed, PP.render expect_whole_printed, PP.render got_whole_printed)

        in Diagnostic.Error
            (Just sp)
            ("conflicting types in " <> what <> ": '" <> expect_part_printed <> "' vs '" <> got_part_printed <> "'")
            ((sp `Diagnostic.msg_note_at` convert_str ("expected '" <> expect_whole_printed <> "', got '" <> got_whole_printed <> "'")) : make_infer_var_name_messages infer_vars var_names)
            []

    to_error (OccursCheckError context@(ErrorTypeContext _ _ _ infer_vars) span var_key ty) =
        let var_as_type = Type'InferVar var_key

            ((ty_printed, var_printed), var_names) =
                run_infer_var_namer $
                    print_type True context ty >>= \ ty_printed ->
                    print_type True context var_as_type >>= \ var_printed ->
                    pure (PP.render ty_printed, PP.render var_printed)
        in Diagnostic.Error
            (Just span)
            ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> " = " <> ty_printed <> "'")
            (make_infer_var_name_messages infer_vars var_names)
            []

    to_error (AmbiguousType for_what) =
        let sp = infer_var_for_what_sp for_what
            name = infer_var_for_what_name for_what
        in Diagnostic.Error
                (Just sp)
                ("ambiguous type: could not infer the type of this " <> name)
                []
                []

    to_error (DoesNotTakeTypeArgument context@(ErrorTypeContext _ _ _ infer_vars) sp ty) =
        let (ty_printed, var_names) =
                run_infer_var_namer $
                    print_type False context ty >>= \ ty_printed ->
                    pure (PP.render ty_printed)
        in Diagnostic.Error
            (Just sp)
            -- TODO: type '' of kind does not take a type argument
            ("type '" <> ty_printed <> "' does not accept a type argument")
            (make_infer_var_name_messages infer_vars var_names)
            []

    to_error (WrongTypeArgument context@(ErrorTypeContext _ _ _ infer_vars) sp ty arg) =
        let ((ty_printed, arg_printed), var_names) =
                run_infer_var_namer $
                    print_type False context ty >>= \ ty_printed ->
                    print_type False context arg >>= \ arg_printed ->
                    pure (PP.render ty_printed, PP.render arg_printed)
        in Diagnostic.Error
            (Just sp)
            -- TODO: type '' of kind '' expects type argument of kind '', but got argument of kind ''
            ("type '" <> ty_printed <> "' does not accept a type argument '" <> arg_printed <> "'")
            (make_infer_var_name_messages infer_vars var_names)
            []

print_type :: Bool -> ErrorTypeContext -> TypeWithInferVars -> InferVarNamer PP.Token -- TODO: since this already a monad, put the arenas and things into a reader monad?
print_type infer_vars_show_index context@(ErrorTypeContext adts type_synonyms vars infer_vars) = todo -- TODO: Type.PP.refer_type_m show_infer_var adts type_synonyms vars
    where
        show_infer_var infer_var =
            case Arena.get infer_vars infer_var of
                InferVar _ Fresh
                    | infer_vars_show_index -> PP.String <$> name_infer_var infer_var
                    | otherwise -> pure "_"
                InferVar _ (Substituted other) -> print_type infer_vars_show_index context other

