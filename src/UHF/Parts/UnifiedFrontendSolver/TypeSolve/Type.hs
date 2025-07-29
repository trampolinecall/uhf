{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Type
    ( Type (..)
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
import UHF.Data.SIR.Stage as Stage

data Type stage
    = Type'ADT Type.ADTKey [Type stage]
    | Type'Synonym Type.TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function (Type stage) (Type stage)
    | Type'Tuple (Type stage) (Type stage)
    | Type'QuantVar Type.QuantVarKey
    | Type'InferVar (Stage.InferVarAllowed stage) InferVarKey
    | Type'Forall (NonEmpty Type.QuantVarKey) (Type stage)
    | Type'Kind'Type
    | Type'Kind'Arrow (Type stage) (Type stage)
    | Type'Kind'Kind
deriving instance Show (Stage.InferVarAllowed stage) => Show (Type stage)

newtype InferVarKey = InferVarKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InferVarKey where
    make_key = InferVarKey
    unmake_key (InferVarKey i) = i
type InferVarArena stage = Arena.Arena (InferVar stage) InferVarKey

data InferVar stage = InferVar InferVarForWhat (InferVarStatus stage)
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
data InferVarStatus stage = Fresh | Substituted (Type stage)

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

-- TODO: remove this
-- from_ir_type :: Type.Type -> Type
-- from_ir_type (Type.Type'ADT adtk args) = Type'ADT adtk (map from_ir_type args)
-- from_ir_type (Type.Type'Synonym tsk) = Type'Synonym tsk
-- from_ir_type Type.Type'Int = Type'Int
-- from_ir_type Type.Type'Float = Type'Float
-- from_ir_type Type.Type'Char = Type'Char
-- from_ir_type Type.Type'String = Type'String
-- from_ir_type Type.Type'Bool = Type'Bool
-- from_ir_type (Type.Type'Function a r) = Type'Function (from_ir_type a) (from_ir_type r)
-- from_ir_type (Type.Type'Tuple a b) = Type'Tuple (from_ir_type a) (from_ir_type b)
-- from_ir_type (Type.Type'QuantVar qvk) = Type'QuantVar qvk
-- from_ir_type (Type.Type'Forall qvars res) = Type'Forall qvars (from_ir_type res)
-- from_ir_type Type.Type'Kind'Type = Type'Kind'Type
-- from_ir_type (Type.Type'Kind'Arrow a b) = Type'Kind'Arrow (from_ir_type a) (from_ir_type b)
-- from_ir_type Type.Type'Kind'Kind = Type'Kind'Kind
