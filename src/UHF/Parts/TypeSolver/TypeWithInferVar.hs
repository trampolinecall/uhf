module UHF.Parts.TypeSolver.TypeWithInferVar
    ( Type (..)
    , Kind (..)
    , InferVar (..)
    , InferVarArena
    , InferVarKey
    , InferVarForWhat (..)
    , InferVarStatus (..)
    , infer_var_for_what_sp
    , infer_var_for_what_name
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
    | Type'Kind Kind
    deriving Show
data Kind
    = Kind'Type
    | Kind'Arrow Type Type
    | Kind'Kind
    deriving Show

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
