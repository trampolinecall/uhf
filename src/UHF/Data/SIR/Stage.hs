{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.SIR.Stage
    ( Stage (..)
    , AllHaveInstance
    , IdenResolvedKeyHasInstance
    , AllShowable
    ) where

import UHF.Prelude

import Data.Kind (Type, Constraint)

class Stage s where
    type NameMapIndex s

    type IdenResolvedKey s :: Type -> Type

    type TypeInRefer s

    type TypeExprEvaledKey s
    type TypeExprEvaledAsTypeKey s

    type TypeInfo s

    type BinaryOpsAllowed s

instance Stage (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) where
    type NameMapIndex (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = name_map_index

    type IdenResolvedKey (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = iden_resolved_key

    type TypeInRefer (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = type_in_refer

    -- TODO: see if it is possible to remove TypeExprEvaledKey and TypeExprEvaledAsTypeKey
    type TypeExprEvaledKey (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = type_expr_evaled_key
    type TypeExprEvaledAsTypeKey (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = type_expr_evaled_as_type_key

    type TypeInfo (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = type_info

    type BinaryOpsAllowed (name_map_index, iden_resolved_key (), type_in_refer, type_expr_evaled_key, type_expr_evaled_as_type_key, type_info, binary_ops_allowed) = binary_ops_allowed

type AllHaveInstance (c :: Type -> Constraint) s =
    ( c (NameMapIndex s)
    , c (TypeExprEvaledKey s)
    , c (TypeExprEvaledAsTypeKey s)
    , c (TypeInfo s)
    , c (BinaryOpsAllowed s)
    )
type IdenResolvedKeyHasInstance d (c :: Type -> Constraint) s = c (IdenResolvedKey s d)
type AllShowable s = AllHaveInstance Show s
