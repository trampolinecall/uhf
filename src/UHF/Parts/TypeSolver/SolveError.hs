module UHF.Parts.TypeSolver.SolveError
    ( ErrorTypeContext(..) -- TODO: remove?
    , SolveError (..)
    ) where

import UHF.Prelude

import UHF.Parts.TypeSolver.Constraint
import UHF.Parts.TypeSolver.TypeWithInferVar
import UHF.Parts.TypeSolver.TypeWithInferVar.PP
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.PP as PP

data ErrorTypeContext t
    = ErrorTypeContext
        (Arena.Arena (Type.ADT t) Type.ADTKey)
        (Arena.Arena (Type.TypeSynonym t) Type.TypeSynonymKey)
        (Arena.Arena Type.QuantVar Type.QuantVarKey)
        InferVarArena
data SolveError t
    = EqError
        { eq_error_context :: ErrorTypeContext t
        , eq_error_in_what :: EqInWhat
        , eq_error_span :: Span
        , eq_error_a_whole :: Located Type
        , eq_error_b_whole :: Located Type
        , eq_error_a_part :: Type
        , eq_error_b_part :: Type
        }
    | ExpectError
        { expect_error_context :: ErrorTypeContext t
        , expect_error_in_what :: ExpectInWhat
        , expect_error_got_whole :: Located Type
        , expect_error_expect_whole :: Type
        , expect_error_got_part :: Type
        , expect_error_expect_part :: Type
        }

    | OccursCheckError (ErrorTypeContext t) Span InferVarKey Type
    | DoesNotTakeTypeArgument (ErrorTypeContext t) Span Type
    | WrongTypeArgument (ErrorTypeContext t) Span Type Type

instance Diagnostic.ToError (SolveError t) where
    to_error (EqError context@(ErrorTypeContext _ _ _ infer_vars) in_what span a_whole b_whole a_part b_part) =
        let what = case in_what of
                InAssignment -> "assignment"
                InNamedPattern -> "named pattern"
                InIfBranches -> "'if' expression"
                InMatchPatterns -> "'case' expression patterns"
                InMatchArms -> "'case' expression arms"

            ((a_part_printed, b_part_printed, a_whole_printed, b_whole_printed), var_names) =
                run_infer_var_namer $ -- TODO: somehow make it so that this can be used without InferVarNamer
                    pp_type_with_error_context False context a_part >>= \ a_part_printed ->
                    pp_type_with_error_context False context b_part >>= \ b_part_printed ->
                    pp_type_with_error_context False context (unlocate a_whole) >>= \ a_whole_printed ->
                    pp_type_with_error_context False context (unlocate b_whole) >>= \ b_whole_printed ->
                    pure (PP.render a_part_printed, PP.render b_part_printed, PP.render a_whole_printed, PP.render b_whole_printed)
        in Diagnostic.Error
            (Just span)
            ("conflicting types in " <> what <> ": '" <> a_part_printed <> "' vs '" <> b_part_printed <> "'") -- TODO: somehow make this say conflicting kinds or maybe just reword the whole message?
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
                InADTFieldType -> "ADT field type"

            sp = just_span got_whole

            ((expect_part_printed, got_part_printed, expect_whole_printed, got_whole_printed), var_names) =
                run_infer_var_namer $
                    pp_type_with_error_context False context expect_part >>= \ expect_part_printed ->
                    pp_type_with_error_context False context got_part >>= \ got_part_printed ->
                    pp_type_with_error_context False context expect_whole >>= \ expect_whole_printed ->
                    pp_type_with_error_context False context (unlocate got_whole) >>= \ got_whole_printed ->
                    pure (PP.render expect_part_printed, PP.render got_part_printed, PP.render expect_whole_printed, PP.render got_whole_printed)

        in Diagnostic.Error
            (Just sp)
            ("conflicting types in " <> what <> ": '" <> expect_part_printed <> "' vs '" <> got_part_printed <> "'")
            ((sp `Diagnostic.msg_error_at` convert_str ("expected '" <> expect_whole_printed <> "', got '" <> got_whole_printed <> "'")) : make_infer_var_name_messages infer_vars var_names)
            []

    to_error (OccursCheckError context@(ErrorTypeContext _ _ _ infer_vars) span var_key ty) =
        let var_as_type = Type'InferVar var_key

            ((ty_printed, var_printed), var_names) =
                run_infer_var_namer $
                    pp_type_with_error_context True context ty >>= \ ty_printed ->
                    pp_type_with_error_context True context var_as_type >>= \ var_printed ->
                    pure (PP.render ty_printed, PP.render var_printed)
        in Diagnostic.Error
            (Just span)
            ("occurs check failure: infinite cyclic type arising from constraint '" <> var_printed <> " = " <> ty_printed <> "'")
            (make_infer_var_name_messages infer_vars var_names)
            []

    to_error (DoesNotTakeTypeArgument context@(ErrorTypeContext _ _ _ infer_vars) sp ty) =
        let (ty_printed, var_names) =
                run_infer_var_namer $
                    pp_type_with_error_context False context ty >>= \ ty_printed ->
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
                    pp_type_with_error_context False context ty >>= \ ty_printed ->
                    pp_type_with_error_context False context arg >>= \ arg_printed ->
                    pure (PP.render ty_printed, PP.render arg_printed)
        in Diagnostic.Error
            (Just sp)
            -- TODO: type '' of kind '' expects type argument of kind '', but got argument of kind ''
            ("type '" <> ty_printed <> "' does not accept a type argument '" <> arg_printed <> "'")
            (make_infer_var_name_messages infer_vars var_names)
            []

pp_type_with_error_context :: Bool -> ErrorTypeContext t -> Type -> InferVarNamer PP.Token
pp_type_with_error_context name_infer_vars (ErrorTypeContext adts type_synonyms quant_vars infer_vars) = pp_type name_infer_vars adts type_synonyms quant_vars infer_vars
