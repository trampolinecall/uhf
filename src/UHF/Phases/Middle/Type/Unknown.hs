module UHF.Phases.Middle.Type.Unknown
    ( TypeUnknownKey
    , TypeUnknown (..)
    , TypeUnknownForWhat (..)
    , TypeUnknownState (..)
    , TypeUnknownArena
    , type_var_for_what_sp
    , type_var_for_what_name
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
    | CaseExpr Span
    | PoisonExpr Span
    | PoisonPattern Span
    | TypeExpr Span
    | HoleExpr Span
    | WildcardPattern Span
    | TypeApplyExpr Span
data TypeUnknownState = Fresh | Substituted (Type.Type TypeUnknownKey)

type_var_for_what_sp :: TypeUnknownForWhat -> Span
type_var_for_what_sp (BoundValue sp) = sp
type_var_for_what_sp (UnresolvedIdenExpr sp) = sp
type_var_for_what_sp (CallExpr sp) = sp
type_var_for_what_sp (CaseExpr sp) = sp
type_var_for_what_sp (PoisonExpr sp) = sp
type_var_for_what_sp (PoisonPattern sp) = sp
type_var_for_what_sp (TypeExpr sp) = sp
type_var_for_what_sp (HoleExpr sp) = sp
type_var_for_what_sp (WildcardPattern sp) = sp
type_var_for_what_sp (TypeApplyExpr sp) = sp

type_var_for_what_name :: TypeUnknownForWhat -> Text
type_var_for_what_name (BoundValue _) = "binding"
type_var_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
type_var_for_what_name (CallExpr _) = "call expression"
type_var_for_what_name (CaseExpr _) = "case expression"
type_var_for_what_name (PoisonExpr _) = "expression"
type_var_for_what_name (PoisonPattern _) = "pattern"
type_var_for_what_name (TypeExpr _) = "type expression"
type_var_for_what_name (HoleExpr _) = "hole expression"
type_var_for_what_name (WildcardPattern _) = "wildcard pattern"
type_var_for_what_name (TypeApplyExpr _) = "type application expression"
