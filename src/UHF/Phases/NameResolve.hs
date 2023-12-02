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
resolve :: SIR.SIR ResolveReferStarts.Unresolved -> Utils.CollectingErrors (SIR.SIR ResolveVPIdens.Resolved)
resolve sir =
    ResolveReferStarts.resolve sir >>= \ sir ->
    EvalTypeExprs.eval sir >>= \ sir ->
    ResolveVPIdens.resolve sir

