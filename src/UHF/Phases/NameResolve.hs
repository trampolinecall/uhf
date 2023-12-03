module UHF.Phases.NameResolve
    ( resolve
    ) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import qualified UHF.Phases.NameResolve.EvalTypeExprs as EvalTypeExprs
import qualified UHF.Phases.NameResolve.ResolveReferStarts as ResolveReferStarts
import qualified UHF.Phases.NameResolve.ResolveVPIdens as ResolveVPIdens
import qualified UHF.Phases.NameResolve.Utils as Utils

-- resolve entry point {{{1
resolve :: SIR.SIR ResolveReferStarts.Unresolved -> Utils.WithErrors (SIR.SIR ResolveVPIdens.Resolved)
resolve sir =
    Utils.collect_child_maps sir >>= \ child_maps ->
    ResolveReferStarts.resolve child_maps sir >>= \ sir ->
    EvalTypeExprs.eval child_maps sir >>= \ sir ->
    ResolveVPIdens.resolve child_maps sir

