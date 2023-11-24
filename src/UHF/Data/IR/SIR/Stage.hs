{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.IR.SIR.Stage
    ( Stage (..)
    , AllHaveInstance
    , AllShowable
    ) where

import UHF.Util.Prelude

import Data.Kind (Type, Constraint)

class Stage s where
    type DIden s
    type VIden s
    type PIden s

    type TypeExprEvaled s
    type TypeExprEvaledAsType s

    type TypeInfo s

    type BinaryOpsAllowed s

instance Stage (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) where
    type DIden (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = d_iden
    type VIden (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = v_iden
    type PIden (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = p_iden

    type TypeExprEvaled (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = type_expr_evaled
    type TypeExprEvaledAsType (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = type_expr_evaled_as_type

    type TypeInfo (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = type_info

    type BinaryOpsAllowed (d_iden, v_iden, p_iden, type_expr_evaled, type_expr_evaled_as_type, type_info, binary_ops_allowed) = binary_ops_allowed

type AllHaveInstance (c :: Type -> Constraint) s = (c (DIden s), c (VIden s), c (PIden s), c (TypeExprEvaled s), c (TypeExprEvaledAsType s), c (TypeInfo s), c (BinaryOpsAllowed s))
type AllShowable s = AllHaveInstance Show s
