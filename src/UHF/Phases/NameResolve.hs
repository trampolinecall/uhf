module UHF.Phases.NameResolve
    ( resolve
    ) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Phases.NameResolve.AssignNameMaps as AssignNameMaps
import qualified UHF.Phases.NameResolve.Error as Error
import qualified UHF.Phases.NameResolve.EvalTypeExprs as EvalTypeExprs
import qualified UHF.Phases.NameResolve.NameMaps as NameMaps
import qualified UHF.Phases.NameResolve.ResolveReferStarts as ResolveReferStarts
import qualified UHF.Phases.NameResolve.ResolveVPIdens as ResolveVPIdens

resolve :: SIR.SIR AssignNameMaps.Unassigned -> Error.WithErrors (SIR.SIR ResolveVPIdens.Resolved, TypeSolver.SolverState)
resolve sir =
    NameMaps.collect_child_maps sir >>= \ child_maps ->
    AssignNameMaps.assign child_maps sir >>= \ sir ->
    ResolveReferStarts.resolve sir >>= \ sir ->
    EvalTypeExprs.eval child_maps sir >>= \ (sir, solver_state) ->
    ResolveVPIdens.resolve child_maps sir >>= \ sir ->
    pure (sir, solver_state)

