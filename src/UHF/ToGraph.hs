module UHF.ToGraph (to_graph) where

import UHF.Util.Prelude

import UHF.IO.Location (Located, unlocate)

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.IR as IR

type Expr = IR.Expr (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void
type Pattern = IR.Pattern (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void))
type Binding = IR.Binding (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void

type BindingArena = Arena.Arena Binding IR.BindingKey
type BoundValueArena = Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey
type GraphArena = Arena.Arena (IR.GraphNode) IR.GraphNodeKey

type BoundValueMap = Map.Map IR.BoundValueKey IR.GraphNodeKey

type MakeGraphState = StateT GraphArena (State BoundValueMap)

to_graph :: BoundValueArena -> BindingArena -> GraphArena
to_graph bvs bindings =
    let ((_, graph), bv_map) = runState (runStateT (Arena.transformM (convert_binding bv_map) bindings) Arena.new) Map.empty
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

new_node :: IR.GraphNode -> MakeGraphState IR.GraphNodeKey
new_node node = state $ Arena.put node

convert_expr :: BoundValueMap -> Expr -> MakeGraphState IR.GraphNodeKey
convert_expr bv_map (IR.Expr'Identifier ty _ bvkey) =
    case unlocate bvkey of
        Just bvkey -> pure $ bv_map Map.! bvkey
        Nothing -> new_node (IR.GraphNode'Poison ty)
convert_expr bv_map (IR.Expr'Char ty _ c) = new_node (IR.GraphNode'Char ty c)
convert_expr bv_map (IR.Expr'String ty _ s) = new_node (IR.GraphNode'String ty s)
convert_expr bv_map (IR.Expr'Int ty _ i) = new_node (IR.GraphNode'Int ty i)
convert_expr bv_map (IR.Expr'Float ty _ f) = new_node (IR.GraphNode'Float ty f)
convert_expr bv_map (IR.Expr'Bool ty _ b) = new_node (IR.GraphNode'Bool ty b)

convert_expr bv_map (IR.Expr'Tuple ty _ a b) = IR.GraphNode'Tuple ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_node

convert_expr bv_map (IR.Expr'Lambda ty _ param body) =
    new_node (IR.GraphNode'Param (IR.pattern_type param)) >>= \ param_node ->
    assign_pattern param param_node >>
    convert_expr bv_map body >>= \ body ->
    new_node (IR.GraphNode'Lambda ty param_node body)

convert_expr bv_map (IR.Expr'Let ty _ e) = convert_expr bv_map e
convert_expr bv_map (IR.Expr'LetRec ty _ e) = convert_expr bv_map e

convert_expr bv_map (IR.Expr'BinaryOps void _ _ _ _) = absurd void

convert_expr bv_map (IR.Expr'Call ty _ callee arg) = IR.GraphNode'Call ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_node

convert_expr bv_map (IR.Expr'If ty _ _ cond true false) = todo
convert_expr bv_map (IR.Expr'Case ty _ _ testing arms) = todo

convert_expr bv_map (IR.Expr'TypeAnnotation _ _ _ e) = convert_expr bv_map e

convert_expr bv_map (IR.Expr'Poison ty _) = todo

map_bound_value :: IR.BoundValueKey -> IR.GraphNodeKey -> MakeGraphState ()
map_bound_value k node = lift $ modify $ Map.insert k node

assign_pattern :: Pattern -> IR.GraphNodeKey -> MakeGraphState ()
assign_pattern (IR.Pattern'Identifier typeinfo _ bvk) initializer = map_bound_value bvk initializer
assign_pattern (IR.Pattern'Tuple typeinfo _ a b) initializer =
    new_node (IR.GraphNode'TupleDestructure1 (IR.pattern_type a) initializer) >>= assign_pattern a >>
    new_node (IR.GraphNode'TupleDestructure2 (IR.pattern_type b) initializer) >>= assign_pattern b

assign_pattern (IR.Pattern'Named typeinfo _ _ bvk subpat) initializer = map_bound_value (unlocate bvk) initializer >> assign_pattern subpat initializer
assign_pattern (IR.Pattern'Poison _ _) _ = pure ()
