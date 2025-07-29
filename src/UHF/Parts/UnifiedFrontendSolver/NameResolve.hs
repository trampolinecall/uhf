module UHF.Parts.UnifiedFrontendSolver.NameResolve (resolve) where

import UHF.Prelude

import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps as AssignNameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.EvalTypeExprs as EvalTypeExprs
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveReferStarts as ResolveReferStarts
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveVPIdens as ResolveVPIdens
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver

resolve :: SIR.SIR AssignNameMaps.Unassigned -> Error.WithErrors (SIR.SIR ResolveVPIdens.Resolved, TypeSolver.SolverState, [TypeSolver.Constraint])
    NameMaps.collect_child_maps sir >>= \child_maps ->
    NameMaps.collect_child_maps sir >>= \child_maps ->
        AssignNameMaps.assign child_maps sir >>= \sir ->
            ResolveReferStarts.resolve sir >>= \sir ->
                EvalTypeExprs.eval child_maps sir >>= \(sir, solver_state, constraints) ->
                    ResolveVPIdens.resolve child_maps sir >>= \sir ->
                        pure (sir, solver_state, constraints)_state, constraints)
