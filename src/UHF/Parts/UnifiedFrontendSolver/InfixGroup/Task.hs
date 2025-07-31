module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..), priority) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey)

data InfixGroupTask
    = InfixGroupTask [IdenResolvedKey SIR.ValueRef] InfixGroupedKey

priority :: InfixGroupTask -> Int
priority _ = 1
