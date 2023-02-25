module UHF.ToGraph (to_graph) where

import UHF.Util.Prelude

import UHF.IO.Located (Located (unlocate))

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.HIR as HIR
import qualified UHF.ANFIR as ANFIR

type Decl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void
type Expr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void
type Pattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void))
type Binding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void

type Type = Maybe (HIR.Type Void)
type GraphNode = ANFIR.Node Type ()
type GraphParam = ANFIR.Param Type

type DeclArena = Arena.Arena Decl HIR.DeclKey
type BoundValueArena = Arena.Arena (HIR.BoundValue (Maybe (HIR.Type Void))) HIR.BoundValueKey

type GraphArena = Arena.Arena GraphNode ANFIR.NodeKey
type ParamArena = Arena.Arena GraphParam ANFIR.ParamKey

type BoundValueMap = Map.Map HIR.BoundValueKey ANFIR.NodeKey

type MakeGraphState = WriterT BoundValueMap (State (GraphArena, ParamArena))

to_graph :: BoundValueArena -> DeclArena -> (GraphArena, ParamArena)
to_graph bvs decls =
    let ((_, bv_map), graph) = runState (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new)
    in graph

convert_decl :: BoundValueMap -> Decl -> MakeGraphState ()
convert_decl bv_map (HIR.Decl'Module _ bindings) = mapM_ (convert_binding bv_map) bindings
convert_decl _ (HIR.Decl'Type _) = pure ()

convert_binding :: BoundValueMap -> Binding -> MakeGraphState ()
-- TODO: decide what to do to prevent nonterminating compiles in cases like `x = x`
--       because 'x' is mapped to the result node of the identifier expression
--       and the identifier expression is mapped directly to the node for 'x' without indirection
--
--       (most other cases like `x = f x` do not create a nonterminating compile because x is mapped to a function call node that has children `f` and `x` so there is one level of indirection (this still will be an infinite loop at runtime though))
convert_binding bv_map (HIR.Binding target _ expr) =
    convert_expr bv_map expr >>= \ expr ->
    assign_pattern target expr

new_graph_node :: GraphNode -> MakeGraphState ANFIR.NodeKey
new_graph_node node = lift $ state $ \ (g, p) -> let (i, g') = Arena.put node g in (i, (g', p))
new_param_node :: GraphParam -> MakeGraphState ANFIR.ParamKey
new_param_node node = lift $ state $ \ (g, p) -> let (i, p') = Arena.put node p in (i, (g, p'))

convert_expr :: BoundValueMap -> Expr -> MakeGraphState ANFIR.NodeKey
convert_expr bv_map (HIR.Expr'Identifier ty _ bvkey) =
    case unlocate bvkey of
        Just bvkey -> pure $ bv_map Map.! bvkey
        Nothing -> new_graph_node (ANFIR.Node'Poison ty ())
convert_expr _ (HIR.Expr'Char ty _ c) = new_graph_node (ANFIR.Node'Char ty c)
convert_expr _ (HIR.Expr'String ty _ s) = new_graph_node (ANFIR.Node'String ty s)
convert_expr _ (HIR.Expr'Int ty _ i) = new_graph_node (ANFIR.Node'Int ty i)
convert_expr _ (HIR.Expr'Float ty _ f) = new_graph_node (ANFIR.Node'Float ty f)
convert_expr _ (HIR.Expr'Bool ty _ b) = new_graph_node (ANFIR.Node'Bool ty b)

convert_expr bv_map (HIR.Expr'Tuple ty _ a b) = ANFIR.Node'Tuple ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_graph_node

convert_expr bv_map (HIR.Expr'Lambda ty _ param body) =
    new_param_node (ANFIR.Param (HIR.pattern_type param)) >>= \ graph_param ->
    new_graph_node (ANFIR.Node'Param (HIR.pattern_type param) graph_param) >>= \ graph_param_node ->
    assign_pattern param graph_param_node >>
    convert_expr bv_map body >>= \ body ->
    new_graph_node (ANFIR.Node'Lambda ty graph_param body)

convert_expr bv_map (HIR.Expr'Let _ _ bindings e) = mapM (convert_binding bv_map) bindings >> convert_expr bv_map e
convert_expr bv_map (HIR.Expr'LetRec _ _ bindings e) = mapM (convert_binding bv_map) bindings >> convert_expr bv_map e

convert_expr _ (HIR.Expr'BinaryOps void _ _ _ _) = absurd void

convert_expr bv_map (HIR.Expr'Call ty _ callee arg) = ANFIR.Node'Call ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_graph_node

convert_expr _ (HIR.Expr'If ty _ _ cond true false) = todo
convert_expr _ (HIR.Expr'Case ty _ _ testing arms) = todo

convert_expr bv_map (HIR.Expr'TypeAnnotation _ _ _ e) = convert_expr bv_map e

convert_expr _ (HIR.Expr'Poison ty _) = new_graph_node (ANFIR.Node'Poison ty ())

map_bound_value :: HIR.BoundValueKey -> ANFIR.NodeKey -> MakeGraphState ()
map_bound_value k node = tell $ Map.singleton k node

assign_pattern :: Pattern -> ANFIR.NodeKey -> MakeGraphState ()
assign_pattern (HIR.Pattern'Identifier _ _ bvk) initializer = map_bound_value bvk initializer
assign_pattern (HIR.Pattern'Wildcard _ _) initializer = pure ()
assign_pattern (HIR.Pattern'Tuple _ _ a b) initializer =
    new_graph_node (ANFIR.Node'TupleDestructure1 (HIR.pattern_type a) initializer) >>= assign_pattern a >>
    new_graph_node (ANFIR.Node'TupleDestructure2 (HIR.pattern_type b) initializer) >>= assign_pattern b

assign_pattern (HIR.Pattern'Named _ _ _ bvk subpat) initializer = map_bound_value (unlocate bvk) initializer >> assign_pattern subpat initializer
assign_pattern (HIR.Pattern'Poison _ _) _ = pure ()
