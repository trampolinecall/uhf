module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Finalize (finalize) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Parts.UnifiedFrontendSolver.Error as SolveError
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult, InfixGroupedArena, InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena

finalize :: InfixGroupedArena -> Compiler.WithDiagnostics SolveError.Error Void (Arena.Arena (Maybe InfixGroupResult) InfixGroupedKey)
finalize = Arena.transformM finalize_result

-- TODO: this is almost exactly the same as finalize_result in NameResolve.Finalize so maybe there should be a report_solve_result in SolveResult?
finalize_result :: SolveResult () () result -> Compiler.WithDiagnostics SolveError.Error Void (Maybe result)
finalize_result (Inconclusive ()) = pure Nothing
finalize_result (Errored ()) = pure Nothing
finalize_result (Solved r) = pure $ Just r
