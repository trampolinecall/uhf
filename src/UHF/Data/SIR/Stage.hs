{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.SIR.Stage
    ( Stage (..)
    , AllHaveInstance
    , IdenResolvedFunctorHasInstance
    , AllShowable
    ) where

import UHF.Prelude

import Data.Kind (Type, Constraint)

class Stage s where
    type NameMapIndex s

    type IdenResolvedFunctor s :: Type -> Type

    type TypeExprEvaled s
    type TypeExprEvaledAsType s

    type TypeInfo s

    type BinaryOpsAllowed s

instance Stage (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) where
    type NameMapIndex (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = name_map_index

    type IdenResolvedFunctor (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = iden_resolved_functor

    -- TODO: see if it is possible to remove TypeExprEvaled and TypeExprEvaledAsType
    type TypeExprEvaled (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = type_expr_evaled
    type TypeExprEvaledAsType (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = type_expr_evaled_as_type

    type TypeInfo (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = type_info

    type BinaryOpsAllowed (name_map_index, iden_resolved_functor (), type_expr_evaled, type_expr_evaled_as_type, infer_var_allowed, type_info, binary_ops_allowed) = binary_ops_allowed

type AllHaveInstance (c :: Type -> Constraint) s =
    ( c (NameMapIndex s)
    , c (TypeExprEvaled s)
    , c (TypeExprEvaledAsType s)
    , c (TypeInfo s)
    , c (BinaryOpsAllowed s)
    )
type IdenResolvedFunctorHasInstance d (c :: Type -> Constraint) s = c (IdenResolvedFunctor s d)
type AllShowable s = AllHaveInstance Show s
