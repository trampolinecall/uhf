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
resolve sir@(SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT (Utils.collect_child_maps sir) decls >>= \ (child_maps, decls) ->
    let sir = (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod)
    in ResolveReferStarts.resolve child_maps sir >>= \ sir ->
    EvalTypeExprs.eval child_maps sir >>= \ sir ->
    ResolveVPIdens.resolve child_maps sir

