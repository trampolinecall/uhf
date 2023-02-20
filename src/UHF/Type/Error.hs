module UHF.Type.Error (Error(..)) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import UHF.Type.Var
import UHF.Type.Aliases
import UHF.Type.Constraint

data Error
    = EqError
        { eq_error_nominal_types :: TypedWithVarsNominalTypeArena
        , eq_error_vars :: TypeVarArena
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located TypeWithVars
        , eq_error_b_whole :: Located TypeWithVars
        , eq_error_a_part :: TypeWithVars
        , eq_error_b_part :: TypeWithVars
        }
    | ExpectError
        { expect_error_nominal_types :: TypedWithVarsNominalTypeArena
        , expect_error_vars :: TypeVarArena
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located TypeWithVars
        , expect_error_expect_whole :: TypeWithVars
        , expect_error_got_part :: TypeWithVars
        , expect_error_expect_part :: TypeWithVars
        }

    | OccursCheckError TypedWithVarsNominalTypeArena TypeVarArena Span TypeVarKey TypeWithVars

    | AmbiguousType TypeVarForWhat

instance Diagnostic.ToError Error where
    to_error (EqError nominal_types vars in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InCasePatterns -> "'case' expression patterns"
                InCaseArms -> "'case' expression arms"

        in Diagnostic.Error Diagnostic.Codes.type_mismatch
                (Just span)
                ("conflicting types in " <> what <> ": '" <> print_type False nominal_types vars a_part <> "' vs '" <> print_type False nominal_types vars b_part <> "'")
                [ just_span a_whole `Diagnostic.msg_note_at` convert_str (print_type False nominal_types vars $ unlocate a_whole)
                , just_span b_whole `Diagnostic.msg_note_at` convert_str (print_type False nominal_types vars $ unlocate b_whole)
                ]
                []

    to_error (ExpectError nominal_types vars in_what got_whole expect_whole got_part expect_part) =
        let what = case in_what of
                InTypeAnnotation -> "type annotation"
                InIfCondition -> "'if' condition"
                InCallExpr -> "call expression"

            sp = just_span got_whole

        in Diagnostic.Error Diagnostic.Codes.type_mismatch -- TODO: change code?
                (Just sp)
                (convert_str $ "conflicting types in " <> what <> ": '" <> print_type False nominal_types vars expect_part <> "' vs '" <> print_type False nominal_types vars got_part <> "'")
                [ sp `Diagnostic.msg_note_at` convert_str ("expected '" <> print_type False nominal_types vars expect_whole <> "', got '" <> print_type False nominal_types vars (unlocate got_whole) <> "'") ]
                []

    to_error (OccursCheckError nominal_types vars span var_key ty) =
        let var_as_type = IR.Type'Variable var_key
            (TypeVar var_for_what _) = Arena.get vars var_key

            var_sp = type_var_for_what_sp var_for_what
            var_name = type_var_for_what_name var_for_what
            var_printed = print_type True nominal_types vars var_as_type

        in Diagnostic.Error Diagnostic.Codes.occurs_check
                (Just span)
                ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> "' = '" <> print_type True nominal_types vars ty <> "'")
                [ var_sp `Diagnostic.msg_note_at` convert_str ("where " <> var_printed <> " is the type of this " <> var_name)]
                []

    to_error (AmbiguousType for_what) =
        let sp = type_var_for_what_sp for_what
            name = type_var_for_what_name for_what
        in Diagnostic.Error Diagnostic.Codes.ambiguous_type
                (Just sp) -- TODO
                ("ambiguous type: could not infer the type of this " <> name) -- TODO: better message
                []
                []

print_type :: Bool -> TypedWithVarsNominalTypeArena -> TypeVarArena -> TypeWithVars -> Text
-- TODO: construct an ast and print it
print_type _ nominals _ (IR.Type'Nominal key) =
    case Arena.get nominals key of
        IR.NominalType'Data name _ -> name
        IR.NominalType'Synonym name _ -> name
print_type _ _ _ IR.Type'Int = "int"
print_type _ _ _ IR.Type'Float = "float"
print_type _ _ _ IR.Type'Char = "char"
print_type _ _ _ IR.Type'String = "string"
print_type _ _ _ IR.Type'Bool = "bool"
print_type vars_show_index nominals vars (IR.Type'Function a r) = print_type vars_show_index nominals vars a <> " -> " <> print_type vars_show_index nominals vars r -- TODO: parentheses and grouping
print_type vars_show_index nominals vars (IR.Type'Tuple a b) = "(" <> print_type vars_show_index nominals vars a <> ", " <> print_type vars_show_index nominals vars b <> ")"
print_type vars_show_index nominals vars (IR.Type'Variable var) =
    case Arena.get vars var of
        TypeVar _ Fresh
            | vars_show_index -> "<unknown " <> show (Arena.unmake_key var) <> ">"
            | otherwise -> "_"
        TypeVar _ (Substituted other) -> print_type vars_show_index nominals vars other

