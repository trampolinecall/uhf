{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.SIR.Stage
    ( Stage (..)
    , AllHaveInstance
    , AllShowable
    ) where

import UHF.Prelude

import Data.Kind (Type, Constraint)

class Stage s where
    type NameMapIndex s

    type IdenResolvedFunctor s

    type DIdenStart s
    type TypeExprEvaled s
    type TypeExprEvaledAsType s

    type VIdenStart s
    type VIdenResolved s
    type PIdenStart s
    type PIdenResolved s

    type TypeInfo s

    type BinaryOpsAllowed s

instance Stage (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) where
    type NameMapIndex (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = name_map_index

    type IdenResolvedFunctor (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = iden_resolved_functor

    -- TODO: remove DIdenStart, VIdenStart, VIdenResolved, PIdenStart, PIdenResolved
    -- TODO: see if it is possible to remove TypeExprEvaled and TypeExprEvaledAsType
    type DIdenStart (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = d_iden_start
    type TypeExprEvaled (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = type_expr_evaled
    type TypeExprEvaledAsType (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = type_expr_evaled_as_type

    type VIdenStart (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = v_iden_start
    type VIdenResolved (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = v_iden_resolved
    type PIdenStart (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = p_iden_start
    type PIdenResolved (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = p_iden_resolved

    type TypeInfo (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = type_info

    type BinaryOpsAllowed (name_map_index, iden_resolved_functor, d_iden_start, type_expr_evaled, type_expr_evaled_as_type, v_iden_start, v_iden_resolved, p_iden_start, p_iden_resolved, type_info, binary_ops_allowed) = binary_ops_allowed

type AllHaveInstance (c :: Type -> Constraint) s =
    ( c (NameMapIndex s)
    , c (DIdenStart s)
    , c (TypeExprEvaled s)
    , c (TypeExprEvaledAsType s)
    , c (VIdenStart s)
    , c (VIdenResolved s)
    , c (PIdenStart s)
    , c (PIdenResolved s)
    , c (TypeInfo s)
    , c (BinaryOpsAllowed s)
    )
type AllShowable s = AllHaveInstance Show s
