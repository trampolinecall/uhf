module UHF.Parts.TypeSolver.TypeWithInferVar
    ( Type (..)
    , InferVar (..)
    , InferVarArena
    , InferVarKey
    , InferVarForWhat (..)
    , InferVarStatus (..)
    , infer_var_for_what_sp
    , infer_var_for_what_name

    , kind_of
    )
    where

import UHF.Prelude

import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Util.Arena as Arena

data Type
    = Type'ADT Type.ADTKey [Type]
    | Type'Synonym Type.TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function Type Type
    | Type'Tuple Type Type
    | Type'QuantVar Type.QuantVarKey
    | Type'InferVar InferVarKey
    | Type'Forall (NonEmpty Type.QuantVarKey) Type
    | Type'Class Type.ClassKey [Type]
    | Type'Kind'Type
    | Type'Kind'Arrow Type Type
    | Type'Kind'Kind
    | Type'Kind'Constraint
    deriving Show

kind_of :: Arena.Arena (Type.ADT adt_t) Type.ADTKey -> Arena.Arena (Type.TypeSynonym (t, Type)) Type.TypeSynonymKey -> Arena.Arena Type.Class Type.ClassKey -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Type -> Type
kind_of adt_arena type_synonym_arena class_arena quant_var_arena = go
    where
        go :: Type -> Type
        go t = case t of
            Type'ADT adt_key applied ->
                let Type.ADT _ _ quant_vars _ = Arena.get adt_arena adt_key
                in make_arrows (map quant_var_kind (drop (length applied) quant_vars)) Type'Kind'Type
            Type'Synonym ts_key ->
                let Type.TypeSynonym _ _ (_, expansion) = Arena.get type_synonym_arena ts_key
                in go expansion -- TODO: need to modify this when type synonyms can be parameterized
            Type'Int -> Type'Kind'Type
            Type'Float -> Type'Kind'Type
            Type'Char -> Type'Kind'Type
            Type'String -> Type'Kind'Type
            Type'Bool -> Type'Kind'Type
            Type'Function _ _ -> Type'Kind'Type
            Type'Tuple _ _ -> Type'Kind'Type
            Type'QuantVar qvk -> quant_var_kind qvk
            Type'InferVar _ -> Type'Kind'Type -- TODO: infer vars with different kinds
            Type'Forall quant_vars result -> make_arrows (map quant_var_kind (toList quant_vars)) result
            Type'Class class_key applied ->
                let Type.Class _ _ quant_vars = Arena.get class_arena class_key
                in make_arrows (map quant_var_kind (drop (length applied) quant_vars)) Type'Kind'Constraint
            Type'Kind'Type -> Type'Kind'Kind
            Type'Kind'Arrow _ _ -> Type'Kind'Kind
            Type'Kind'Kind -> Type'Kind'Kind
            Type'Kind'Constraint -> Type'Kind'Kind

        quant_var_kind :: Type.QuantVarKey -> Type
        quant_var_kind qvk = Type'Kind'Type -- TODO: quant vars with different kinds

        make_arrows :: [Type] -> Type -> Type
        make_arrows [] res = res
        make_arrows (cur_arg:more_args) res = Type'Kind'Arrow cur_arg (make_arrows more_args res)

newtype InferVarKey = InferVarKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InferVarKey where
    make_key = InferVarKey
    unmake_key (InferVarKey i) = i
type InferVarArena = Arena.Arena InferVar InferVarKey

data InferVar = InferVar InferVarForWhat InferVarStatus
data InferVarForWhat
    = Variable Span
    | UnresolvedIdenExpr Span
    | CallExpr Span
    | MatchExpr Span
    | PoisonExpr Span
    | PoisonPattern Span
    | TypeExpr Span
    | HoleExpr Span
    | WildcardPattern Span
    | TypeApplyExpr Span
    | TypeHole Span
    | UnresolvedADTVariantPattern Span
    | ImplicitTyParam Span
    | SomeError Span -- TODO: remove this
data InferVarStatus = Fresh | Substituted Type

infer_var_for_what_sp :: InferVarForWhat -> Span
infer_var_for_what_sp (Variable sp) = sp
infer_var_for_what_sp (UnresolvedIdenExpr sp) = sp
infer_var_for_what_sp (CallExpr sp) = sp
infer_var_for_what_sp (MatchExpr sp) = sp
infer_var_for_what_sp (PoisonExpr sp) = sp
infer_var_for_what_sp (PoisonPattern sp) = sp
infer_var_for_what_sp (TypeExpr sp) = sp
infer_var_for_what_sp (HoleExpr sp) = sp
infer_var_for_what_sp (WildcardPattern sp) = sp
infer_var_for_what_sp (TypeApplyExpr sp) = sp
infer_var_for_what_sp (TypeHole sp) = sp
infer_var_for_what_sp (UnresolvedADTVariantPattern sp) = sp
infer_var_for_what_sp (ImplicitTyParam sp) = sp
infer_var_for_what_sp (SomeError sp) = sp

infer_var_for_what_name :: InferVarForWhat -> Text
infer_var_for_what_name (Variable _) = "binding"
infer_var_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
infer_var_for_what_name (CallExpr _) = "call expression"
infer_var_for_what_name (MatchExpr _) = "match expression"
infer_var_for_what_name (PoisonExpr _) = "expression"
infer_var_for_what_name (PoisonPattern _) = "pattern"
infer_var_for_what_name (TypeExpr _) = "type expression"
infer_var_for_what_name (HoleExpr _) = "hole expression"
infer_var_for_what_name (WildcardPattern _) = "wildcard pattern"
infer_var_for_what_name (TypeApplyExpr _) = "type application expression"
infer_var_for_what_name (TypeHole _) = "type hole"
infer_var_for_what_name (UnresolvedADTVariantPattern _) = "ADT variant pattern"
infer_var_for_what_name (ImplicitTyParam _) = "implicit type parameter" -- TODO: better message
infer_var_for_what_name (SomeError _) = "some error" -- TODO: remove this
