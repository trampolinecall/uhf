module UHF.Phases.Middle.Type.Error (Error(..)) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import UHF.Phases.Middle.Type.Var
import UHF.Phases.Middle.Type.Aliases
import UHF.Phases.Middle.Type.Constraint

type VarNamer = State (Int, Map TypeVarKey Text)
run_var_namer :: VarNamer a -> (a, Map TypeVarKey Text)
run_var_namer s = let (r, (_, n)) = runState s (1, Map.empty) in (r, n)
make_var_name_messages :: TypeVarArena -> Map TypeVarKey Text -> [Diagnostic.Message]
make_var_name_messages vars names =
    map
        (\ (key, name) ->
            let (TypeVar var_for_what _) = Arena.get vars key
                var_sp = type_var_for_what_sp var_for_what
                var_name = type_var_for_what_name var_for_what
             in var_sp `Diagnostic.msg_note_at` convert_str ("where '" <> name <> "' is the type of this " <> var_name))
        (Map.toList names)
name_var :: TypeVarKey -> VarNamer Text
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
        { eq_error_adts :: TypedWithVarsADTArena
        , eq_error_type_synonyms :: TypedWithVarsTypeSynonymArena
        , eq_error_vars :: TypeVarArena
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithVars
        , eq_error_b_whole :: Located TypeWithVars
        , eq_error_a_part :: TypeWithVars
        , eq_error_b_part :: TypeWithVars
        }
    | ExpectError
        { expect_error_adts :: TypedWithVarsADTArena
        , expect_error_type_synonyms :: TypedWithVarsTypeSynonymArena
        , expect_error_vars :: TypeVarArena
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithVars
        , expect_error_expect_whole :: TypeWithVars
        , expect_error_got_part :: TypeWithVars
        , expect_error_expect_part :: TypeWithVars
        }

    | OccursCheckError TypedWithVarsADTArena TypedWithVarsTypeSynonymArena TypeVarArena Span TypeVarKey TypeWithVars

    | AmbiguousType TypeVarForWhat

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
                    pure (a_part_printed, b_part_printed, a_whole_printed, b_whole_printed)
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
                    pure (expect_part_printed, got_part_printed, expect_whole_printed, got_whole_printed)

        in Diagnostic.Error Diagnostic.Codes.type_mismatch -- TODO: change code?
            (Just sp)
            (convert_str $ "conflicting types in " <> what <> ": '" <> expect_part_printed <> "' vs '" <> got_part_printed <> "'")
            ((sp `Diagnostic.msg_note_at` convert_str ("expected '" <> expect_whole_printed <> "', got '" <> got_whole_printed <> "'")) : make_var_name_messages vars var_names)
            []

    to_error (OccursCheckError adts type_synonyms vars span var_key ty) =
        let var_as_type = Type.Type'Variable var_key

            ((ty_printed, var_printed), var_names) =
                run_var_namer $
                    print_type True adts type_synonyms vars ty >>= \ ty_printed ->
                    print_type True adts type_synonyms vars var_as_type >>= \ var_printed ->
                    pure (ty_printed, var_printed)
        in Diagnostic.Error Diagnostic.Codes.occurs_check
            (Just span)
            ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> " = " <> ty_printed <> "'")
            (make_var_name_messages vars var_names)
            []

    to_error (AmbiguousType for_what) =
        let sp = type_var_for_what_sp for_what
            name = type_var_for_what_name for_what
        in Diagnostic.Error Diagnostic.Codes.ambiguous_type
                (Just sp) -- TODO
                ("ambiguous type: could not infer the type of this " <> name) -- TODO: better message
                []
                []

    to_error (NotAType sp instead) = Diagnostic.Error Diagnostic.Codes.not_a_type (Just sp) ("not a type: got " <> instead) [] []

print_type :: Bool -> TypedWithVarsADTArena -> TypedWithVarsTypeSynonymArena ->TypeVarArena -> TypeWithVars -> VarNamer Text -- TODO: since this already a monad, put the arenas and things into a reader monad?
-- TODO: construct an ast and print it
print_type _ adts _ _ (Type.Type'ADT key) =
    case Arena.get adts key of
        HIR.ADT name _ -> pure name

print_type _ _ type_synonyms _ (Type.Type'Synonym key) =
    case Arena.get type_synonyms key of
        HIR.TypeSynonym name _ -> pure name

print_type _ _ _ _ Type.Type'Int = pure "int"
print_type _ _ _ _ Type.Type'Float = pure "float"
print_type _ _ _ _ Type.Type'Char = pure "char"
print_type _ _ _ _ Type.Type'String = pure "string"
print_type _ _ _ _ Type.Type'Bool = pure "bool"
print_type vars_show_index adts type_synonyms vars (Type.Type'Function a r) = print_type vars_show_index adts type_synonyms vars a >>= \ a -> print_type vars_show_index adts type_synonyms vars r >>= \ r -> pure (a <> " -> " <> r) -- TODO: parentheses and grouping
print_type vars_show_index adts type_synonyms vars (Type.Type'Tuple a b) = print_type vars_show_index adts type_synonyms vars a >>= \ a -> print_type vars_show_index adts type_synonyms vars b >>= \ b -> pure ("(" <> a <> ", " <> b <> ")")
print_type vars_show_index adts type_synonyms vars (Type.Type'Variable var) =
    case Arena.get vars var of
        TypeVar _ Fresh
            | vars_show_index -> name_var var
            | otherwise -> pure "_"
        TypeVar _ (Substituted other) -> print_type vars_show_index adts type_synonyms vars other

