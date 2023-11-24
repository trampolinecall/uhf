{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.IR.SIR.Stage
    ( Stage (..)
    , AllHaveInstance
    , AllShowable
    ) where

import UHF.Util.Prelude

class Stage s where
    type DIden s
    type VIden s
    type PIden s
    type TypeInfo s
    type BinaryOpsAllowed s

instance Stage (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) where
    type DIden (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) = d_iden
    type VIden (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) = v_iden
    type PIden (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) = p_iden
    type TypeInfo (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) = type_info
    type BinaryOpsAllowed (d_iden, v_iden, p_iden, type_info, binary_ops_allowed) = binary_ops_allowed

type AllHaveInstance c s = (c (DIden s), c (VIden s), c (PIden s), c (TypeInfo s), c (BinaryOpsAllowed s))
type AllShowable s = (Show (DIden s), Show (VIden s), Show (PIden s), Show (TypeInfo s), Show (BinaryOpsAllowed s))
