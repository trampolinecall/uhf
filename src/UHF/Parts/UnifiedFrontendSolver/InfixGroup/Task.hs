{-# LANGUAGE DataKinds #-}
module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..), priority) where

import UHF.Prelude

import qualified UHF.Data.SIR.ID as SIR.ID

data InfixGroupTask
    = InfixGroupTask [SIR.ID.ID "ValueIden"] (SIR.ID.ID "BinaryOpsExpr")

priority :: InfixGroupTask -> Int
priority _ = 1
