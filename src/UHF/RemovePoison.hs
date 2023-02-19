module UHF.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IR as IR

type Decl = IR.Decl
type DeclArena = Arena.Arena Decl IR.DeclKey

type PoisonedType = Maybe (IR.Type Void)
type PoisonedNominalType = IR.NominalType PoisonedType
type PoisonedGraphNode = IR.GraphNode PoisonedType ()
type PoisonedGraphParam = IR.GraphParam PoisonedType
type PoisonedBoundValue = IR.BoundValue PoisonedType

type PoisonedNominalTypeArena = Arena.Arena PoisonedNominalType IR.NominalTypeKey
type PoisonedGraphNodeArena = Arena.Arena PoisonedGraphNode IR.GraphNodeKey
type PoisonedGraphParamArena = Arena.Arena PoisonedGraphParam IR.GraphParamKey
type PoisonedBoundValueArena = Arena.Arena PoisonedBoundValue IR.BoundValueKey

type NoPoisonType = IR.Type Void
type NoPoisonNominalType = IR.NominalType NoPoisonType
type NoPoisonGraphNode = IR.GraphNode NoPoisonType Void
type NoPoisonGraphParam = IR.GraphParam NoPoisonType
type NoPoisonBoundValue = IR.BoundValue NoPoisonType

type NoPoisonNominalTypeArena = Arena.Arena NoPoisonNominalType IR.NominalTypeKey
type NoPoisonGraphNodeArena = Arena.Arena NoPoisonGraphNode IR.GraphNodeKey
type NoPoisonGraphParamArena = Arena.Arena NoPoisonGraphParam IR.GraphParamKey
type NoPoisonBoundValueArena = Arena.Arena NoPoisonBoundValue IR.BoundValueKey
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
rp_nominal_type (IR.NominalType'Data name variants) = IR.NominalType'Data name <$> mapM rp_variant variants
    where
        rp_variant (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> sequence fields
rp_nominal_type (IR.NominalType'Synonym name expansion) = IR.NominalType'Synonym name <$> expansion

rp_graph_node :: PoisonedGraphNode -> Maybe NoPoisonGraphNode
rp_graph_node (IR.GraphNode'Int ty i) = ty >>= \ ty -> pure (IR.GraphNode'Int ty i)
rp_graph_node (IR.GraphNode'Float ty f) = ty >>= \ ty -> pure (IR.GraphNode'Float ty f)
rp_graph_node (IR.GraphNode'Bool ty b) = ty >>= \ ty -> pure (IR.GraphNode'Bool ty b)
rp_graph_node (IR.GraphNode'Char ty c) = ty >>= \ ty -> pure (IR.GraphNode'Char ty c)
rp_graph_node (IR.GraphNode'String ty t) = ty >>= \ ty -> pure (IR.GraphNode'String ty t)
rp_graph_node (IR.GraphNode'Tuple ty a b) = ty >>= \ ty -> pure (IR.GraphNode'Tuple ty a b)

rp_graph_node (IR.GraphNode'Lambda ty a r) = ty >>= \ ty -> pure (IR.GraphNode'Lambda ty a r)
rp_graph_node (IR.GraphNode'Param ty p) = ty >>= \ ty -> pure (IR.GraphNode'Param ty p)

rp_graph_node (IR.GraphNode'Call ty c a) = ty >>= \ ty -> pure (IR.GraphNode'Call ty c a)

rp_graph_node (IR.GraphNode'TupleDestructure1 ty t) = ty >>= \ ty -> pure (IR.GraphNode'TupleDestructure1 ty t)
rp_graph_node (IR.GraphNode'TupleDestructure2 ty t) = ty >>= \ ty -> pure (IR.GraphNode'TupleDestructure2 ty t)

rp_graph_node (IR.GraphNode'Poison _ _) = Nothing

rp_graph_param :: PoisonedGraphParam -> Maybe NoPoisonGraphParam
rp_graph_param (IR.GraphParam ty) = IR.GraphParam <$> ty

rp_bound_value :: PoisonedBoundValue -> Maybe NoPoisonBoundValue
rp_bound_value (IR.BoundValue ty sp) = ty >>= \ ty -> pure (IR.BoundValue ty sp)
