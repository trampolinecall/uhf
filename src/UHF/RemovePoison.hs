module UHF.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.HIR as HIR
import qualified UHF.ANFIR as ANFIR

type Decl = HIR.Decl
type DeclArena = Arena.Arena Decl HIR.DeclKey

type PoisonedType = Maybe (HIR.Type Void)
type PoisonedNominalType = HIR.NominalType PoisonedType
type PoisonedGraphNode = ANFIR.Node PoisonedType ()
type PoisonedGraphParam = ANFIR.Param PoisonedType
type PoisonedBoundValue = HIR.BoundValue PoisonedType

type PoisonedNominalTypeArena = Arena.Arena PoisonedNominalType HIR.NominalTypeKey
type PoisonedGraphNodeArena = Arena.Arena PoisonedGraphNode ANFIR.NodeKey
type PoisonedGraphParamArena = Arena.Arena PoisonedGraphParam ANFIR.ParamKey
type PoisonedBoundValueArena = Arena.Arena PoisonedBoundValue HIR.BoundValueKey

type NoPoisonType = HIR.Type Void
type NoPoisonNominalType = HIR.NominalType NoPoisonType
type NoPoisonGraphNode = ANFIR.Node NoPoisonType Void
type NoPoisonGraphParam = ANFIR.Param NoPoisonType
type NoPoisonBoundValue = HIR.BoundValue NoPoisonType

type NoPoisonNominalTypeArena = Arena.Arena NoPoisonNominalType HIR.NominalTypeKey
type NoPoisonGraphNodeArena = Arena.Arena NoPoisonGraphNode ANFIR.NodeKey
type NoPoisonGraphParamArena = Arena.Arena NoPoisonGraphParam ANFIR.ParamKey
type NoPoisonBoundValueArena = Arena.Arena NoPoisonBoundValue HIR.BoundValueKey
remove_poison :: (DeclArena, PoisonedNominalTypeArena, PoisonedGraphNodeArena, PoisonedGraphParamArena, PoisonedBoundValueArena) -> Maybe (DeclArena, NoPoisonNominalTypeArena, NoPoisonGraphNodeArena, NoPoisonGraphParamArena, NoPoisonBoundValueArena)
-- TODO: probably dont pass DeclArena if it is not going to be changed
remove_poison (decls, nominal_types, graph_nodes, graph_params, bound_values) =
    (decls,,,,)
        <$> Arena.transformM rp_nominal_type nominal_types
        <*> Arena.transformM rp_graph_node graph_nodes
        <*> Arena.transformM rp_graph_param graph_params
        <*> Arena.transformM rp_bound_value bound_values

-- rp short for remove poison

rp_nominal_type :: PoisonedNominalType -> Maybe NoPoisonNominalType
rp_nominal_type (HIR.NominalType'Data name variants) = HIR.NominalType'Data name <$> mapM rp_variant variants
    where
        rp_variant (HIR.DataVariant'Named name fields) = HIR.DataVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (HIR.DataVariant'Anon name fields) = HIR.DataVariant'Anon name <$> sequence fields
rp_nominal_type (HIR.NominalType'Synonym name expansion) = HIR.NominalType'Synonym name <$> expansion

rp_graph_node :: PoisonedGraphNode -> Maybe NoPoisonGraphNode
rp_graph_node (ANFIR.Node'Int ty i) = ty >>= \ ty -> pure (ANFIR.Node'Int ty i)
rp_graph_node (ANFIR.Node'Float ty f) = ty >>= \ ty -> pure (ANFIR.Node'Float ty f)
rp_graph_node (ANFIR.Node'Bool ty b) = ty >>= \ ty -> pure (ANFIR.Node'Bool ty b)
rp_graph_node (ANFIR.Node'Char ty c) = ty >>= \ ty -> pure (ANFIR.Node'Char ty c)
rp_graph_node (ANFIR.Node'String ty t) = ty >>= \ ty -> pure (ANFIR.Node'String ty t)
rp_graph_node (ANFIR.Node'Tuple ty a b) = ty >>= \ ty -> pure (ANFIR.Node'Tuple ty a b)

rp_graph_node (ANFIR.Node'Lambda ty a r) = ty >>= \ ty -> pure (ANFIR.Node'Lambda ty a r)
rp_graph_node (ANFIR.Node'Param ty p) = ty >>= \ ty -> pure (ANFIR.Node'Param ty p)

rp_graph_node (ANFIR.Node'Call ty c a) = ty >>= \ ty -> pure (ANFIR.Node'Call ty c a)

rp_graph_node (ANFIR.Node'TupleDestructure1 ty t) = ty >>= \ ty -> pure (ANFIR.Node'TupleDestructure1 ty t)
rp_graph_node (ANFIR.Node'TupleDestructure2 ty t) = ty >>= \ ty -> pure (ANFIR.Node'TupleDestructure2 ty t)

rp_graph_node (ANFIR.Node'Poison _ _) = Nothing

rp_graph_param :: PoisonedGraphParam -> Maybe NoPoisonGraphParam
rp_graph_param (ANFIR.Param ty) = ANFIR.Param <$> ty

rp_bound_value :: PoisonedBoundValue -> Maybe NoPoisonBoundValue
rp_bound_value (HIR.BoundValue ty sp) = ty >>= \ ty -> pure (HIR.BoundValue ty sp)
