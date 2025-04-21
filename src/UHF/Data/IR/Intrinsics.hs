module UHF.Data.IR.Intrinsics
    ( Intrinsic (..)
    , intrinsic_bv_type
    , intrinsic_bv_name
    , intrinsic_bv_id
    ) where

import UHF.Prelude

import GHC.Enum (Enum, Bounded)

import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type

data Intrinsic
    = Intrinsic'StrConcat
    | Intrinsic'IntAdd
    | Intrinsic'IntSub
    | Intrinsic'IntMul
    | Intrinsic'IntDiv
    | Intrinsic'IntMod
    | Intrinsic'ImpurePrint
    deriving (Show, Bounded, Enum)

intrinsic_bv_info :: Intrinsic -> (Text, Type.Type)
intrinsic_bv_info Intrinsic'StrConcat = ("str_concat", Type.Type'String `Type.Type'Function` (Type.Type'String `Type.Type'Function` Type.Type'String)) -- str_concat :: string -> string -> string
intrinsic_bv_info Intrinsic'IntAdd = ("int_add", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_add :: int -> int -> int
intrinsic_bv_info Intrinsic'IntSub = ("int_sub", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_sub :: int -> int -> int
intrinsic_bv_info Intrinsic'IntMul = ("int_mul", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_mul :: int -> int -> int
intrinsic_bv_info Intrinsic'IntDiv = ("int_div", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_div :: int -> int -> int
intrinsic_bv_info Intrinsic'IntMod = ("int_mod", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_mod :: int -> int -> int
intrinsic_bv_info Intrinsic'ImpurePrint = ("impure_print", Type.Type'String `Type.Type'Function` Type.Type'Int) -- impure_print :: string -> int (always returns 0) TODO: this is really bad

intrinsic_bv_type :: Intrinsic -> Type.Type
intrinsic_bv_type i = let (_, ty) = intrinsic_bv_info i in ty

intrinsic_bv_name :: Intrinsic -> Text
intrinsic_bv_name i = let (name, _) = intrinsic_bv_info i in name

intrinsic_bv_id :: Intrinsic -> ID.IntrinsicBVID
intrinsic_bv_id = ID.IntrinsicBVID . intrinsic_bv_name
