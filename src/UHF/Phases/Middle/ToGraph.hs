module UHF.Phases.Middle.ToGraph (to_graph) where

import UHF.Util.Prelude

import UHF.IO.Located (Located (unlocate))

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type RIRDecl = RIR.Decl
type Expr = RIR.Expr
type Pattern = RIR.Pattern
type Binding = RIR.Binding

type ANFIRDecl = ANFIR.Decl
type Type = Maybe (Type.Type Void)
type GraphNode = ANFIR.Node Type ()
type GraphParam = ANFIR.Param Type

type RIRDeclArena = Arena.Arena RIRDecl DeclKey
type BoundValueArena = Arena.Arena (HIR.BoundValue (Maybe (Type.Type Void))) BoundValueKey

type ANFIRDeclArena = Arena.Arena ANFIRDecl DeclKey
type GraphArena = Arena.Arena GraphNode ANFIR.NodeKey
type ParamArena = Arena.Arena GraphParam ANFIR.ParamKey

type BoundValueMap = Map.Map BoundValueKey ANFIR.NodeKey

type MakeGraphState = WriterT BoundValueMap (StateT (GraphArena, ParamArena) (Reader BoundValueArena))

to_graph :: BoundValueArena -> RIRDeclArena -> (ANFIRDeclArena, GraphArena, ParamArena)
to_graph bvs decls =
    let ((decls', bv_map), (nodes, params)) = runReader (runStateT (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new)) bvs
    in (decls', nodes, params)

convert_decl :: BoundValueMap -> RIRDecl -> MakeGraphState ANFIRDecl
convert_decl bv_map (RIR.Decl'Module bindings) = ANFIR.Decl'Module <$> (concat <$> mapM (convert_binding bv_map) bindings)
convert_decl _ (RIR.Decl'Type ty) = pure $ ANFIR.Decl'Type ty

map_bound_value :: BoundValueKey -> ANFIR.NodeKey -> MakeGraphState ()
map_bound_value k node = tell $ Map.singleton k node

get_bv :: BoundValueKey -> MakeGraphState (HIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: BoundValueMap -> Binding -> MakeGraphState [ANFIR.NodeKey]
-- TODO: decide what to do to prevent nonterminating compiles in cases like `x = x`
--       because 'x' is mapped to the result node of the identifier expression
--       and the identifier expression is mapped directly to the node for 'x' without indirection
--
--       (most other cases like `x = f x` do not create a nonterminating compile because x is mapped to a function call node that has children `f` and `x` so there is one level of indirection (this still will be an infinite loop at runtime though))
convert_binding bv_map (RIR.Binding target expr) =
    runWriterT (convert_expr bv_map expr) >>= \ (expr_result_node, expr_involved_nodes) ->
    map_bound_value target expr_result_node >>
    pure (expr_involved_nodes)

new_graph_node :: GraphNode -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.NodeKey
new_graph_node node = lift (lift $ state $ \ (g, p) -> let (i, g') = Arena.put node g in (i, (g', p))) >>= \ node_key -> tell [node_key] >> pure node_key
new_param_node :: GraphParam -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.ParamKey
new_param_node node = lift (lift $ state $ \ (g, p) -> let (i, p') = Arena.put node p in (i, (g, p')))

convert_expr :: BoundValueMap -> Expr -> WriterT [ANFIR.NodeKey] MakeGraphState ANFIR.NodeKey
convert_expr bv_map (RIR.Expr'Identifier ty _ bvkey) =
    case bvkey of
        Just bvkey -> pure $ bv_map Map.! bvkey -- included nodes of the identifier does not need to be included because even though evaluating the identifier expression requires evaluating those nodes, this is not creating those nodes
                                                -- those nodes will be created by their bindings
        Nothing -> new_graph_node (ANFIR.Node'Poison ty ())
convert_expr _ (RIR.Expr'Char ty _ c) = new_graph_node (ANFIR.Node'Char ty c)
convert_expr _ (RIR.Expr'String ty _ s) = new_graph_node (ANFIR.Node'String ty s)
convert_expr _ (RIR.Expr'Int ty _ i) = new_graph_node (ANFIR.Node'Int ty i)
convert_expr _ (RIR.Expr'Float ty _ f) = new_graph_node (ANFIR.Node'Float ty f)
convert_expr _ (RIR.Expr'Bool ty _ b) = new_graph_node (ANFIR.Node'Bool ty b)

convert_expr bv_map (RIR.Expr'Tuple ty _ a b) = ANFIR.Node'Tuple ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_graph_node

convert_expr bv_map (RIR.Expr'Lambda ty _ param body) =
    lift (get_bv param) >>= \ (HIR.BoundValue param_ty _) ->
    new_param_node (ANFIR.Param param_ty) >>= \ graph_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included nodes because they do not need to be evaluated to create the lambda object
        new_graph_node (ANFIR.Node'Param param_ty graph_param) >>= \ graph_param_node ->
        lift (map_bound_value param graph_param_node) >>
        convert_expr bv_map body
    ) >>= \ (body, body_included_nodes) ->
    new_graph_node (ANFIR.Node'Lambda ty graph_param body_included_nodes body)

convert_expr bv_map (RIR.Expr'Let _ _ bindings e) = mapM (lift . convert_binding bv_map) bindings >>= \ binding_involved_nodes -> tell (concat binding_involved_nodes) >> convert_expr bv_map e

convert_expr bv_map (RIR.Expr'Call ty _ callee arg) = ANFIR.Node'Call ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_graph_node

convert_expr _ (RIR.Expr'If ty _ cond true false) = todo
convert_expr _ (RIR.Expr'Case ty _ testing arms) = todo

convert_expr _ (RIR.Expr'Poison ty _) = new_graph_node (ANFIR.Node'Poison ty ())

assign_pattern :: Pattern -> ANFIR.NodeKey -> WriterT [ANFIR.NodeKey] MakeGraphState ()
assign_pattern (RIR.Pattern'Identifier _ _ bvk) initializer = lift (map_bound_value bvk initializer) >> pure ()
assign_pattern (RIR.Pattern'Wildcard _ _) initializer = pure ()
assign_pattern (RIR.Pattern'Tuple _ _ a b) initializer =
    new_graph_node (ANFIR.Node'TupleDestructure1 (RIR.pattern_type a) initializer) >>= assign_pattern a >>
    new_graph_node (ANFIR.Node'TupleDestructure2 (RIR.pattern_type b) initializer) >>= assign_pattern b

assign_pattern (RIR.Pattern'Named _ _ bvk subpat) initializer = lift (map_bound_value (unlocate bvk) initializer) >> assign_pattern subpat initializer
assign_pattern (RIR.Pattern'Poison _ _) _ = pure ()
