module UHF.Phases.SolveTypes.Solver.InferVar
    ( TypeInferVar (..)
    , TypeInferVarArena
    , TypeInferVarKey
    , TypeInferVarForWhat (..)
    , TypeInferVarStatus (..)
    , type_infer_var_for_what_sp
    , type_infer_var_for_what_name

    , KindInferVar (..)
    , KindInferVarArena
    , KindInferVarKey
    , KindInferVarForWhat (..)
    , KindInferVarStatus (..)
    , kind_infer_var_for_what_sp
    , kind_infer_var_for_what_name
    )
    where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.Type as Type

import UHF.Source.Span (Span)

newtype TypeInferVarKey = TypeInferVarKey Int deriving (Show, Eq, Ord)
instance Arena.Key TypeInferVarKey where
    make_key = TypeInferVarKey
    unmake_key (TypeInferVarKey i) = i
type TypeInferVarArena = Arena.Arena TypeInferVar TypeInferVarKey

data TypeInferVar = TypeInferVar TypeInferVarForWhat TypeInferVarStatus
data TypeInferVarForWhat
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
data TypeInferVarStatus = TFresh | TSubstituted (Type.Type TypeInferVarKey)

type_infer_var_for_what_sp :: TypeInferVarForWhat -> Span
type_infer_var_for_what_sp (Variable sp) = sp
type_infer_var_for_what_sp (UnresolvedIdenExpr sp) = sp
type_infer_var_for_what_sp (CallExpr sp) = sp
type_infer_var_for_what_sp (MatchExpr sp) = sp
type_infer_var_for_what_sp (PoisonExpr sp) = sp
type_infer_var_for_what_sp (PoisonPattern sp) = sp
type_infer_var_for_what_sp (TypeExpr sp) = sp
type_infer_var_for_what_sp (HoleExpr sp) = sp
type_infer_var_for_what_sp (WildcardPattern sp) = sp
type_infer_var_for_what_sp (TypeApplyExpr sp) = sp
type_infer_var_for_what_sp (TypeHole sp) = sp
type_infer_var_for_what_sp (UnresolvedADTVariantPattern sp) = sp
type_infer_var_for_what_sp (ImplicitTyParam sp) = sp
type_infer_var_for_what_sp (SomeError sp) = sp

type_infer_var_for_what_name :: TypeInferVarForWhat -> Text
type_infer_var_for_what_name (Variable _) = "binding"
type_infer_var_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
type_infer_var_for_what_name (CallExpr _) = "call expression"
type_infer_var_for_what_name (MatchExpr _) = "match expression"
type_infer_var_for_what_name (PoisonExpr _) = "expression"
type_infer_var_for_what_name (PoisonPattern _) = "pattern"
type_infer_var_for_what_name (TypeExpr _) = "type expression"
type_infer_var_for_what_name (HoleExpr _) = "hole expression"
type_infer_var_for_what_name (WildcardPattern _) = "wildcard pattern"
type_infer_var_for_what_name (TypeApplyExpr _) = "type application expression"
type_infer_var_for_what_name (TypeHole _) = "type hole"
type_infer_var_for_what_name (UnresolvedADTVariantPattern _) = "ADT variant pattern"
type_infer_var_for_what_name (ImplicitTyParam _) = "implicit type parameter" -- TODO: better message
type_infer_var_for_what_name (SomeError _) = "some error" -- TODO: remove this

-- TODO: actually do kinds correctly
newtype KindInferVarKey = KindInferVarKey Int deriving (Show, Eq, Ord)
instance Arena.Key KindInferVarKey where
    make_key = KindInferVarKey
    unmake_key (KindInferVarKey i) = i

type KindInferVarArena = Arena.Arena KindInferVar KindInferVarKey
data KindInferVar = KindInferVar KindInferVarForWhat KindInferVarStatus
data KindInferVarForWhat
data KindInferVarStatus = KFresh | KSubstituted (Type.Type TypeInferVarKey)

kind_infer_var_for_what_sp :: TypeInferVarForWhat -> Span
kind_infer_var_for_what_sp _ = todo

kind_infer_var_for_what_name :: TypeInferVarForWhat -> Text
kind_infer_var_for_what_name _ = todo
