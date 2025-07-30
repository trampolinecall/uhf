module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..)) where

import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena (IdenResolvedKey)

data InfixGroupTask
    = InfixGroupTask [IdenResolvedKey SIR.ValueRef] InfixGroupedKey
