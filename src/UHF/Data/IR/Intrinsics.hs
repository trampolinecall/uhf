module UHF.Data.IR.Intrinsics
    ( IntrinsicBoundValue (..)
    , intrinsic_bv_type
    , intrinsic_bv_name
    , intrinsic_bv_id
    ) where

import UHF.Prelude

import GHC.Enum (Enum, Bounded)

import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type

data IntrinsicBoundValue
    = IntrinsicBoundValue'StrConcat
    | IntrinsicBoundValue'IntAdd
    | IntrinsicBoundValue'IntSub
    | IntrinsicBoundValue'IntMul
    | IntrinsicBoundValue'IntDiv
    | IntrinsicBoundValue'IntMod
    deriving (Show, Bounded, Enum)

intrinsic_bv_info :: IntrinsicBoundValue -> (Text, Type.Type)
intrinsic_bv_info IntrinsicBoundValue'StrConcat = ("str_concat", Type.Type'String `Type.Type'Function` (Type.Type'String `Type.Type'Function` Type.Type'String)) -- str_concat :: string -> string -> string
intrinsic_bv_info IntrinsicBoundValue'IntAdd = ("int_add", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_add :: int -> int -> int
intrinsic_bv_info IntrinsicBoundValue'IntSub = ("int_sub", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_sub :: int -> int -> int
intrinsic_bv_info IntrinsicBoundValue'IntMul = ("int_mul", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_mul :: int -> int -> int
intrinsic_bv_info IntrinsicBoundValue'IntDiv = ("int_div", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_div :: int -> int -> int
intrinsic_bv_info IntrinsicBoundValue'IntMod = ("int_mod", Type.Type'Int `Type.Type'Function` (Type.Type'Int `Type.Type'Function` Type.Type'Int)) -- int_mod :: int -> int -> int

intrinsic_bv_type :: IntrinsicBoundValue -> Type.Type
intrinsic_bv_type i = let (_, ty) = intrinsic_bv_info i in ty

intrinsic_bv_name :: IntrinsicBoundValue -> Text
intrinsic_bv_name i = let (name, _) = intrinsic_bv_info i in name

intrinsic_bv_id :: IntrinsicBoundValue -> ID.IntrinsicBVID
intrinsic_bv_id = ID.IntrinsicBVID . intrinsic_bv_name

