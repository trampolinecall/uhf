{-# LANGUAGE DataKinds #-}
module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..), priority) where

import UHF.Prelude

import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey)
import qualified UHF.Data.SIR.ID as SIR.ID

data InfixGroupTask
    = InfixGroupTask [SIR.ID.ID "ValueIden"] InfixGroupedKey

priority :: InfixGroupTask -> Int
priority _ = 1
