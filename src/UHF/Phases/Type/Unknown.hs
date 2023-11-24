module UHF.Phases.Type.Unknown
    ( TypeUnknownKey
    , TypeUnknown (..)
    , TypeUnknownForWhat (..)
    , TypeUnknownState (..)
    , TypeUnknownArena
    , type_unk_for_what_sp
    , type_unk_for_what_name
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Span (Span)

newtype TypeUnknownKey = TypeUnknownKey Int deriving (Show, Eq, Ord)
instance Arena.Key TypeUnknownKey where
    make_key = TypeUnknownKey
    unmake_key (TypeUnknownKey i) = i
type TypeUnknownArena = Arena.Arena TypeUnknown TypeUnknownKey

data TypeUnknown = TypeUnknown TypeUnknownForWhat TypeUnknownState
data TypeUnknownForWhat
    = BoundValue Span
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
data TypeUnknownState = Fresh | Substituted (Type.Type TypeUnknownKey)

type_unk_for_what_sp :: TypeUnknownForWhat -> Span
type_unk_for_what_sp (BoundValue sp) = sp
type_unk_for_what_sp (UnresolvedIdenExpr sp) = sp
type_unk_for_what_sp (CallExpr sp) = sp
type_unk_for_what_sp (MatchExpr sp) = sp
type_unk_for_what_sp (PoisonExpr sp) = sp
type_unk_for_what_sp (PoisonPattern sp) = sp
type_unk_for_what_sp (TypeExpr sp) = sp
type_unk_for_what_sp (HoleExpr sp) = sp
type_unk_for_what_sp (WildcardPattern sp) = sp
type_unk_for_what_sp (TypeApplyExpr sp) = sp
type_unk_for_what_sp (TypeHole sp) = sp
type_unk_for_what_sp (UnresolvedADTVariantPattern sp) = sp
type_unk_for_what_sp (ImplicitTyParam sp) = sp
type_unk_for_what_sp (SomeError sp) = sp

type_unk_for_what_name :: TypeUnknownForWhat -> Text
type_unk_for_what_name (BoundValue _) = "binding"
type_unk_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
type_unk_for_what_name (CallExpr _) = "call expression"
type_unk_for_what_name (MatchExpr _) = "match expression"
type_unk_for_what_name (PoisonExpr _) = "expression"
type_unk_for_what_name (PoisonPattern _) = "pattern"
type_unk_for_what_name (TypeExpr _) = "type expression"
type_unk_for_what_name (HoleExpr _) = "hole expression"
type_unk_for_what_name (WildcardPattern _) = "wildcard pattern"
type_unk_for_what_name (TypeApplyExpr _) = "type application expression"
type_unk_for_what_name (TypeHole _) = "type hole"
type_unk_for_what_name (UnresolvedADTVariantPattern _) = "ADT variant pattern"
type_unk_for_what_name (ImplicitTyParam _) = "implicit type parameter" -- TODO: better message
type_unk_for_what_name (SomeError _) = "some error" -- TODO: remove this
