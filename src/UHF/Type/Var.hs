module UHF.Type.Var
    ( TypeVarKey
    , TypeVar (..)
    , TypeVarForWhat (..)
    , TypeVarState (..)
    , TypeVarArena
    , type_var_for_what_sp
    , type_var_for_what_name
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.IR as IR

import UHF.IO.Span (Span)

newtype TypeVarKey = TypeVarKey Int deriving (Show, Eq, Ord)
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i
type TypeVarArena = Arena.Arena TypeVar TypeVarKey

data TypeVar = TypeVar TypeVarForWhat TypeVarState
data TypeVarForWhat
    = BoundValue Span
    | UnresolvedIdenExpr Span
    | CallExpr Span
    | CaseExpr Span
    | PoisonExpr Span
    | PoisonPattern Span
    | TypeExpr Span
    | WildcardPattern Span
data TypeVarState = Fresh | Substituted (IR.Type TypeVarKey)

type_var_for_what_sp :: TypeVarForWhat -> Span
type_var_for_what_sp (BoundValue sp) = sp
type_var_for_what_sp (UnresolvedIdenExpr sp) = sp
type_var_for_what_sp (CallExpr sp) = sp
type_var_for_what_sp (CaseExpr sp) = sp
type_var_for_what_sp (PoisonExpr sp) = sp
type_var_for_what_sp (PoisonPattern sp) = sp
type_var_for_what_sp (TypeExpr sp) = sp
type_var_for_what_sp (WildcardPattern sp) = sp

type_var_for_what_name :: TypeVarForWhat -> Text
type_var_for_what_name (BoundValue _) = "binding"
type_var_for_what_name (UnresolvedIdenExpr _) = "identifier expression"
type_var_for_what_name (CallExpr _) = "call expression"
type_var_for_what_name (CaseExpr _) = "case expression"
type_var_for_what_name (PoisonExpr _) = "expression"
type_var_for_what_name (PoisonPattern _) = "pattern"
type_var_for_what_name (TypeExpr _) = "type expression"
type_var_for_what_name (WildcardPattern _) = "wildcard pattern"

