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
    deriving (Show, Bounded, Enum)

intrinsic_bv_type :: IntrinsicBoundValue -> Type.Type
intrinsic_bv_type IntrinsicBoundValue'StrConcat = Type.Type'String `Type.Type'Function` (Type.Type'String `Type.Type'Function` Type.Type'String) -- str_concat :: string -> string -> string

intrinsic_bv_name :: IntrinsicBoundValue -> Text
intrinsic_bv_name IntrinsicBoundValue'StrConcat = "str_concat"

intrinsic_bv_id :: IntrinsicBoundValue -> ID.IntrinsicBVID
intrinsic_bv_id IntrinsicBoundValue'StrConcat = ID.IntrinsicBVID'StrConcat
