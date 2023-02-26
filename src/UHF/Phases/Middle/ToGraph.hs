module UHF.Phases.Middle.ToGraph (to_graph) where

import UHF.Util.Prelude

import UHF.IO.Located (Located (unlocate))

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type HIRDecl = HIR.Decl (Located (Maybe BoundValueKey)) (Maybe (Type.Type Void)) (Maybe (Type.Type Void)) Void
type Expr = HIR.Expr (Located (Maybe BoundValueKey)) (Maybe (Type.Type Void)) (Maybe (Type.Type Void)) Void
type Pattern = HIR.Pattern (Located (Maybe BoundValueKey)) (Maybe (Type.Type Void))
type Binding = HIR.Binding (Located (Maybe BoundValueKey)) (Maybe (Type.Type Void)) (Maybe (Type.Type Void)) Void

type ANFIRDecl = ANFIR.Decl
type Type = Maybe (Type.Type Void)
type GraphNode = ANFIR.Node Type ()
type GraphParam = ANFIR.Param Type

type HIRDeclArena = Arena.Arena HIRDecl DeclKey
type BoundValueArena = Arena.Arena (HIR.BoundValue (Maybe (Type.Type Void))) BoundValueKey

type ANFIRDeclArena = Arena.Arena ANFIRDecl DeclKey
type GraphArena = Arena.Arena GraphNode ANFIR.NodeKey
type ParamArena = Arena.Arena GraphParam ANFIR.ParamKey

type BoundValueMap = Map.Map BoundValueKey ANFIR.NodeKey

type MakeGraphState = WriterT BoundValueMap (State (GraphArena, ParamArena))

to_graph :: BoundValueArena -> HIRDeclArena -> (ANFIRDeclArena, GraphArena, ParamArena)
to_graph bvs decls =
    let ((decls', bv_map), (nodes, params)) = runState (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new)
    in (decls', nodes, params)

convert_decl :: BoundValueMap -> HIRDecl -> MakeGraphState ANFIRDecl
convert_decl bv_map (HIR.Decl'Module _ bindings) = ANFIR.Decl'Module <$> (concat <$> mapM (convert_binding bv_map) bindings)
convert_decl _ (HIR.Decl'Type ty) = pure $ ANFIR.Decl'Type ty

convert_binding :: BoundValueMap -> Binding -> MakeGraphState [ANFIR.NodeKey]
-- TODO: decide what to do to prevent nonterminating compiles in cases like `x = x`
--       because 'x' is mapped to the result node of the identifier expression
--       and the identifier expression is mapped directly to the node for 'x' without indirection
--
--       (most other cases like `x = f x` do not create a nonterminating compile because x is mapped to a function call node that has children `f` and `x` so there is one level of indirection (this still will be an infinite loop at runtime though))
convert_binding bv_map (HIR.Binding target _ expr) =
    runWriterT (convert_expr bv_map expr) >>= \ (expr_result_node, expr_involved_nodes) ->
    runWriterT (assign_pattern target expr_result_node) >>= \ ((), destructure_involved_nodes) ->
    pure (expr_involved_nodes ++ destructure_involved_nodes)

new_graph_node :: GraphNode -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.NodeKey
new_graph_node node = lift (lift $ state $ \ (g, p) -> let (i, g') = Arena.put node g in (i, (g', p))) >>= \ node_key -> tell [node_key] >> pure node_key
new_param_node :: GraphParam -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.ParamKey
new_param_node node = lift (lift $ state $ \ (g, p) -> let (i, p') = Arena.put node p in (i, (g, p')))

convert_expr :: BoundValueMap -> Expr -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.NodeKey
convert_expr bv_map (HIR.Expr'Identifier ty _ bvkey) =
    case unlocate bvkey of
        Just bvkey -> pure $ bv_map Map.! bvkey -- included nodes of the identifier does not need to be included because even though evaluating the identifier expression requires evaluating those nodes, this is not creating those nodes
                                                -- those nodes will be created by their bindings
        Nothing -> new_graph_node (ANFIR.Node'Poison ty ())
convert_expr _ (HIR.Expr'Char ty _ c) = new_graph_node (ANFIR.Node'Char ty c)
convert_expr _ (HIR.Expr'String ty _ s) = new_graph_node (ANFIR.Node'String ty s)
convert_expr _ (HIR.Expr'Int ty _ i) = new_graph_node (ANFIR.Node'Int ty i)
convert_expr _ (HIR.Expr'Float ty _ f) = new_graph_node (ANFIR.Node'Float ty f)
convert_expr _ (HIR.Expr'Bool ty _ b) = new_graph_node (ANFIR.Node'Bool ty b)

convert_expr bv_map (HIR.Expr'Tuple ty _ a b) = ANFIR.Node'Tuple ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_graph_node

convert_expr bv_map (HIR.Expr'Lambda ty _ param body) =
    new_param_node (ANFIR.Param (HIR.pattern_type param)) >>= \ graph_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the global included nodes because they do not need to be evaluated to create the lambda object
        new_graph_node (ANFIR.Node'Param (HIR.pattern_type param) graph_param) >>= \ graph_param_node ->
        assign_pattern param graph_param_node >>
        convert_expr bv_map body
    ) >>= \ (body, body_included_nodes) ->
    new_graph_node (ANFIR.Node'Lambda ty graph_param body_included_nodes body)

convert_expr bv_map (HIR.Expr'Let _ _ bindings e) = mapM (lift . convert_binding bv_map) bindings >>= \ binding_involved_nodes -> tell (concat binding_involved_nodes) >> convert_expr bv_map e
convert_expr bv_map (HIR.Expr'LetRec _ _ bindings e) = mapM (lift . convert_binding bv_map) bindings >>= \ binding_involved_nodes -> tell (concat binding_involved_nodes) >> convert_expr bv_map e

convert_expr _ (HIR.Expr'BinaryOps void _ _ _ _) = absurd void

convert_expr bv_map (HIR.Expr'Call ty _ callee arg) = ANFIR.Node'Call ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_graph_node

convert_expr _ (HIR.Expr'If ty _ _ cond true false) = todo
convert_expr _ (HIR.Expr'Case ty _ _ testing arms) = todo

convert_expr bv_map (HIR.Expr'TypeAnnotation _ _ _ e) = convert_expr bv_map e

convert_expr _ (HIR.Expr'Poison ty _) = new_graph_node (ANFIR.Node'Poison ty ())

map_bound_value :: BoundValueKey -> ANFIR.NodeKey -> WriterT [ANFIR.NodeKey] MakeGraphState ()
map_bound_value k node = lift $ tell $ Map.singleton k node

assign_pattern :: Pattern -> ANFIR.NodeKey -> WriterT [ANFIR.NodeKey] MakeGraphState ()
assign_pattern (HIR.Pattern'Identifier _ _ bvk) initializer = map_bound_value bvk initializer >> pure ()
assign_pattern (HIR.Pattern'Wildcard _ _) initializer = pure ()
assign_pattern (HIR.Pattern'Tuple _ _ a b) initializer =
    new_graph_node (ANFIR.Node'TupleDestructure1 (HIR.pattern_type a) initializer) >>= assign_pattern a >>
    new_graph_node (ANFIR.Node'TupleDestructure2 (HIR.pattern_type b) initializer) >>= assign_pattern b

assign_pattern (HIR.Pattern'Named _ _ _ bvk subpat) initializer = map_bound_value (unlocate bvk) initializer >> assign_pattern subpat initializer
assign_pattern (HIR.Pattern'Poison _ _) _ = pure ()
