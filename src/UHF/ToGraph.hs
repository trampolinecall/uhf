module UHF.ToGraph (to_graph) where

import UHF.Util.Prelude

import UHF.IO.Location (Located, unlocate)

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.IR as IR

type Expr = IR.Expr (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void
type Pattern = IR.Pattern (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void))
type Binding = IR.Binding (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void

type Type = Maybe (IR.Type Void)
type GraphNode = IR.GraphNode Type ()
type GraphParam = IR.GraphParam Type

type BindingArena = Arena.Arena Binding IR.BindingKey
type BoundValueArena = Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey
type GraphArena = Arena.Arena GraphNode IR.GraphNodeKey
type ParamArena = Arena.Arena GraphParam IR.GraphParamKey

type BoundValueMap = Map.Map IR.BoundValueKey IR.GraphNodeKey

type MakeGraphState = WriterT BoundValueMap (State (GraphArena, ParamArena))

to_graph :: BoundValueArena -> BindingArena -> (GraphArena, ParamArena)
to_graph bvs bindings =
    let ((_, bv_map), graph) = runState (runWriterT (Arena.transformM (convert_binding bv_map) bindings)) (Arena.new, Arena.new)
    in graph

convert_binding :: BoundValueMap -> Binding -> MakeGraphState ()
-- TODO: decide what to do to prevent nonterminating compiles in cases like `x = x`
--       because 'x' is mapped to the result node of the identifier expression
--       and the identifier expression is mapped directly to the node for 'x' without indirection
--
--       (most other cases like `x = f x` do not create a nonterminating compile because x is mapped to a function call node that has children `f` and `x` so there is one level of indirection (this still will be an infinite loop at runtime though))
convert_binding bv_map (IR.Binding target _ expr) =
    convert_expr bv_map expr >>= \ expr ->
    assign_pattern target expr

new_graph_node :: GraphNode -> MakeGraphState IR.GraphNodeKey
new_graph_node node = lift $ state $ \ (g, p) -> let (i, g') = Arena.put node g in (i, (g', p))
new_param_node :: GraphParam -> MakeGraphState IR.GraphParamKey
new_param_node node = lift $ state $ \ (g, p) -> let (i, p') = Arena.put node p in (i, (g, p'))

convert_expr :: BoundValueMap -> Expr -> MakeGraphState IR.GraphNodeKey
convert_expr bv_map (IR.Expr'Identifier ty _ bvkey) =
    case unlocate bvkey of
        Just bvkey -> pure $ bv_map Map.! bvkey
        Nothing -> new_graph_node (IR.GraphNode'Poison ty ())
convert_expr _ (IR.Expr'Char ty _ c) = new_graph_node (IR.GraphNode'Char ty c)
convert_expr _ (IR.Expr'String ty _ s) = new_graph_node (IR.GraphNode'String ty s)
convert_expr _ (IR.Expr'Int ty _ i) = new_graph_node (IR.GraphNode'Int ty i)
convert_expr _ (IR.Expr'Float ty _ f) = new_graph_node (IR.GraphNode'Float ty f)
convert_expr _ (IR.Expr'Bool ty _ b) = new_graph_node (IR.GraphNode'Bool ty b)

convert_expr bv_map (IR.Expr'Tuple ty _ a b) = IR.GraphNode'Tuple ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_graph_node

convert_expr bv_map (IR.Expr'Lambda ty _ param body) =
    new_param_node (IR.GraphParam (IR.pattern_type param)) >>= \ graph_param ->
    new_graph_node (IR.GraphNode'Param (IR.pattern_type param) graph_param) >>= \ graph_param_node ->
    assign_pattern param graph_param_node >>
    convert_expr bv_map body >>= \ body ->
    new_graph_node (IR.GraphNode'Lambda ty graph_param body)

convert_expr bv_map (IR.Expr'Let ty _ e) = convert_expr bv_map e
convert_expr bv_map (IR.Expr'LetRec ty _ e) = convert_expr bv_map e

convert_expr _ (IR.Expr'BinaryOps void _ _ _ _) = absurd void

convert_expr bv_map (IR.Expr'Call ty _ callee arg) = IR.GraphNode'Call ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_graph_node

convert_expr _ (IR.Expr'If ty _ _ cond true false) = todo
convert_expr _ (IR.Expr'Case ty _ _ testing arms) = todo

convert_expr bv_map (IR.Expr'TypeAnnotation _ _ _ e) = convert_expr bv_map e

convert_expr _ (IR.Expr'Poison ty _) = new_graph_node (IR.GraphNode'Poison ty ())

map_bound_value :: IR.BoundValueKey -> IR.GraphNodeKey -> MakeGraphState ()
map_bound_value k node = tell $ Map.singleton k node

assign_pattern :: Pattern -> IR.GraphNodeKey -> MakeGraphState ()
assign_pattern (IR.Pattern'Identifier _ _ bvk) initializer = map_bound_value bvk initializer
assign_pattern (IR.Pattern'Tuple _ _ a b) initializer =
    new_graph_node (IR.GraphNode'TupleDestructure1 (IR.pattern_type a) initializer) >>= assign_pattern a >>
    new_graph_node (IR.GraphNode'TupleDestructure2 (IR.pattern_type b) initializer) >>= assign_pattern b

assign_pattern (IR.Pattern'Named _ _ _ bvk subpat) initializer = map_bound_value (unlocate bvk) initializer >> assign_pattern subpat initializer
assign_pattern (IR.Pattern'Poison _ _) _ = pure ()
