module UHF.Phases.NameResolve.ResolveReferStarts
    ( resolve

    , resolve_d_iden
    , resolve_v_iden
    , resolve_p_iden
    ) where

import UHF.Prelude

import UHF.Phases.NameResolve.Keys
import UHF.Source.Located (Located (unlocate))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Phases.NameResolve.Error as Error
import qualified UHF.Phases.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

type UnresolvedIdenStart = Located Text

type ResolvedDIdenStart = Maybe (SIR.Decl TypeSolver.Type)
type ResolvedVIdenStart = Maybe SIR.VariableKey
type ResolvedPIdenStart = Maybe Type.ADT.VariantIndex

resolve ::
    Arena.Arena UnresolvedIdenStart DIdenStartKey -> Arena.Arena NameMaps.NameMapStack DIdenStartKey ->
    Arena.Arena UnresolvedIdenStart VIdenStartKey -> Arena.Arena NameMaps.NameMapStack VIdenStartKey ->
    Arena.Arena UnresolvedIdenStart PIdenStartKey -> Arena.Arena NameMaps.NameMapStack PIdenStartKey ->
    Error.WithErrors (Arena.Arena ResolvedDIdenStart DIdenStartKey, Arena.Arena ResolvedVIdenStart VIdenStartKey, Arena.Arena ResolvedPIdenStart PIdenStartKey)
resolve d_iden_starts d_iden_start_nms v_iden_starts v_iden_start_nms p_iden_starts p_iden_start_nms = do
    d_iden_starts_resolved <- Arena.transform_with_keyM (resolve_d_iden' d_iden_start_nms) d_iden_starts
    v_iden_starts_resolved <- Arena.transform_with_keyM (resolve_v_iden' v_iden_start_nms) v_iden_starts
    p_iden_starts_resolved <- Arena.transform_with_keyM (resolve_p_iden' p_iden_start_nms) p_iden_starts

    pure (d_iden_starts_resolved, v_iden_starts_resolved, p_iden_starts_resolved)

-- TODO: remove these
resolve_d_iden' :: Arena.Arena NameMaps.NameMapStack DIdenStartKey -> DIdenStartKey -> UnresolvedIdenStart -> Error.WithErrors ResolvedDIdenStart
resolve_d_iden' d_iden_start_nms d_iden_key d_iden_start =
    let name_map = Arena.get d_iden_start_nms d_iden_key
    in resolve_d_iden name_map d_iden_start
resolve_v_iden' :: Arena.Arena NameMaps.NameMapStack VIdenStartKey -> VIdenStartKey -> UnresolvedIdenStart -> Error.WithErrors ResolvedVIdenStart
resolve_v_iden' v_iden_start_nms v_iden_key v_iden_start =
    let name_map = Arena.get v_iden_start_nms v_iden_key
    in resolve_v_iden name_map v_iden_start
resolve_p_iden' :: Arena.Arena NameMaps.NameMapStack PIdenStartKey -> PIdenStartKey -> UnresolvedIdenStart -> Error.WithErrors ResolvedPIdenStart
resolve_p_iden' p_iden_start_nms p_iden_key p_iden_start =
    let name_map = Arena.get p_iden_start_nms p_iden_key
    in resolve_p_iden name_map p_iden_start

resolve_d_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedDIdenStart
resolve_d_iden name_map d_iden_start = resolve_iden_start (\ (NameMaps.NameMaps d_children _ _) -> d_children) name_map d_iden_start
resolve_v_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedVIdenStart
resolve_v_iden name_map v_iden_start = resolve_iden_start (\ (NameMaps.NameMaps _ var_children _) -> var_children) name_map v_iden_start
resolve_p_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedPIdenStart
resolve_p_iden name_map p_iden_start = resolve_iden_start (\ (NameMaps.NameMaps _ _ adtv_children) -> adtv_children) name_map p_iden_start

resolve_iden_start :: (NameMaps.NameMaps -> Map Text resolved) -> NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors (Maybe resolved)
resolve_iden_start which_map name_map_stack iden =
    case go name_map_stack of
        Right resolved -> pure $ Just resolved
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        go (NameMaps.NameMapStack name_maps parent) =
            case Map.lookup (unlocate iden) (which_map name_maps) of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Left $ Error.Error'CouldNotFind iden

