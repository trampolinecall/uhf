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

type AllHaveInstance c s = (c (DIden s), c (VIden s), c (PIden s), c (TypeInfo s), c (BinaryOpsAllowed s))
type AllShowable s = (Show (DIden s), Show (VIden s), Show (PIden s), Show (TypeInfo s), Show (BinaryOpsAllowed s))
