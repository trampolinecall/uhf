module UHF.Parts.NameResolve
    ( resolve
    ) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Parts.NameResolve.AssignNameMaps as AssignNameMaps
import qualified UHF.Parts.NameResolve.Error as Error
import qualified UHF.Parts.NameResolve.EvalTypeExprs as EvalTypeExprs
import qualified UHF.Parts.NameResolve.NameMaps as NameMaps
import qualified UHF.Parts.NameResolve.ResolveReferStarts as ResolveReferStarts
import qualified UHF.Parts.NameResolve.ResolveVPIdens as ResolveVPIdens

resolve :: SIR.SIR AssignNameMaps.Unassigned -> Error.WithErrors (SIR.SIR ResolveVPIdens.Resolved, TypeSolver.SolverState, [TypeSolver.Constraint])
resolve sir =
    NameMaps.collect_child_maps sir >>= \ child_maps ->
    AssignNameMaps.assign child_maps sir >>= \ sir ->
    ResolveReferStarts.resolve sir >>= \ sir ->
    EvalTypeExprs.eval child_maps sir >>= \ (sir, solver_state, constraints) ->
    ResolveVPIdens.resolve child_maps sir >>= \ sir ->
    pure (sir, solver_state, constraints)
