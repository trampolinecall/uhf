module UHF.Phases.Middle.ToANFIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type Type = Maybe (Type.Type Void)

type RIRDecl = RIR.Decl
type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type ANFIRDecl = ANFIR.Decl
type ANFIRExpr = ANFIR.Expr Type ()
type ANFIRParam = ANFIR.Param Type
type ANFIRBinding = ANFIR.Binding Type ()

type RIRDeclArena = Arena.Arena RIRDecl DeclKey
type BoundValueArena = Arena.Arena (HIR.BoundValue (Maybe (Type.Type Void))) BoundValueKey

type ANFIRDeclArena = Arena.Arena ANFIRDecl DeclKey
type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BoundValueMap = Map.Map BoundValueKey ANFIR.BindingKey

type MakeGraphState = WriterT BoundValueMap (StateT (ANFIRBindingArena, ANFIRParamArena) (Reader BoundValueArena))

convert :: BoundValueArena -> RIRDeclArena -> (ANFIRDeclArena, ANFIRBindingArena, ANFIRParamArena)
convert bvs decls =
    let ((decls', bv_map), (nodes, params)) = runReader (runStateT (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new)) bvs
    in (decls', nodes, params)

convert_decl :: BoundValueMap -> RIRDecl -> MakeGraphState ANFIRDecl
convert_decl bv_map (RIR.Decl'Module bindings) = ANFIR.Decl'Module <$> (concat <$> mapM (convert_binding ANFIR.InModule bv_map) bindings)
convert_decl _ (RIR.Decl'Type ty) = pure $ ANFIR.Decl'Type ty

map_bound_value :: BoundValueKey -> ANFIR.BindingKey -> MakeGraphState ()
map_bound_value k node = tell $ Map.singleton k node

get_bv :: BoundValueKey -> MakeGraphState (HIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: ANFIR.BoundWhere -> BoundValueMap -> RIRBinding -> MakeGraphState [ANFIR.BindingKey]
convert_binding bound_where bv_map (RIR.Binding target expr) =
    runWriterT (convert_expr bound_where bv_map expr) >>= \ (expr_result_node, expr_involved_nodes) ->
    map_bound_value target expr_result_node >>
    pure expr_involved_nodes

new_binding :: ANFIR.BoundWhere -> ANFIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
new_binding bound_where expr = lift (lift $ state $ \ (g, p) -> let (i, g') = Arena.put (ANFIR.Binding bound_where (ANFIR.node_type expr) expr) g in (i, (g', p))) >>= \ node_key -> tell [node_key] >> pure node_key
new_param :: ANFIRParam -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (g, p) -> let (i, p') = Arena.put param p in (i, (g, p')))

convert_expr :: ANFIR.BoundWhere -> BoundValueMap -> RIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
convert_expr bound_where bv_map (RIR.Expr'Identifier ty _ bvkey) =
    case bvkey of
        Just bvkey -> new_binding bound_where (ANFIR.Expr'Identifier ty (bv_map Map.! bvkey))
        Nothing -> new_binding bound_where (ANFIR.Expr'Poison ty ())
convert_expr bound_where _ (RIR.Expr'Char ty _ c) = new_binding bound_where (ANFIR.Expr'Char ty c)
convert_expr bound_where _ (RIR.Expr'String ty _ s) = new_binding bound_where (ANFIR.Expr'String ty s)
convert_expr bound_where _ (RIR.Expr'Int ty _ i) = new_binding bound_where (ANFIR.Expr'Int ty i)
convert_expr bound_where _ (RIR.Expr'Float ty _ f) = new_binding bound_where (ANFIR.Expr'Float ty f)
convert_expr bound_where _ (RIR.Expr'Bool ty _ b) = new_binding bound_where (ANFIR.Expr'Bool ty b)

convert_expr bound_where bv_map (RIR.Expr'Tuple ty _ a b) = ANFIR.Expr'Tuple ty <$> convert_expr bound_where bv_map a <*> convert_expr bound_where bv_map b >>= new_binding bound_where

convert_expr bound_where bv_map (RIR.Expr'Lambda ty _ param_bv body) =
    lift (get_bv param_bv) >>= \ (HIR.BoundValue param_ty _) ->
    new_param (ANFIR.Param param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included nodes because they do not need to be evaluated to create the lambda object
        new_binding bound_where (ANFIR.Expr'Param param_ty anfir_param) >>= \ param_binding ->
        lift (map_bound_value param_bv param_binding) >>
        convert_expr bound_where bv_map body
    ) >>= \ (body, body_included_nodes) ->
    new_binding bound_where (ANFIR.Expr'Lambda ty anfir_param body_included_nodes body)

convert_expr bound_where bv_map (RIR.Expr'Let _ _ bindings e) = mapM (lift . convert_binding bound_where bv_map) bindings >>= \ binding_involved_nodes -> tell (concat binding_involved_nodes) >> convert_expr bound_where bv_map e

convert_expr bound_where bv_map (RIR.Expr'Call ty _ callee arg) = ANFIR.Expr'Call ty <$> convert_expr bound_where bv_map callee <*> convert_expr bound_where bv_map arg >>= new_binding bound_where

convert_expr bound_where bv_map (RIR.Expr'Switch ty _ testing arms) =
    convert_expr bound_where bv_map testing >>= \ testing ->
    ANFIR.Expr'Switch ty testing <$> mapM (\ (matcher, arm) -> (,) <$> convert_matcher matcher testing <*> convert_expr bound_where bv_map arm) arms >>= new_binding bound_where
    where
        convert_matcher (RIR.Switch'BoolLiteral b) _ = pure $ ANFIR.Switch'BoolLiteral b
        convert_matcher (RIR.Switch'Tuple a b) testing =
            -- case thing {
            --     (a, b) -> e
            -- }
            -- becomes
            -- case thing {
            --     (,) ->
            --         let a = TupleDestructure1 thing;
            --         let b = TupleDestructure2 thing;
            --         e
            -- }
            (case a of
                Just a -> lift (get_bv a) >>= \ (HIR.BoundValue a_ty _) -> new_binding bound_where (ANFIR.Expr'TupleDestructure1 a_ty testing) >>= \ a_destructure -> lift (map_bound_value a a_destructure)
                Nothing -> pure ()) >>
            (case b of
                Just b -> lift (get_bv b) >>= \ (HIR.BoundValue b_ty _) -> new_binding bound_where (ANFIR.Expr'TupleDestructure2 b_ty testing) >>= \ b_destructure -> lift (map_bound_value b b_destructure)
                Nothing -> pure ()) >>
            pure ANFIR.Switch'Tuple
        convert_matcher RIR.Switch'Default _ = pure ANFIR.Switch'Default

convert_expr bound_where _ (RIR.Expr'Poison ty _) = new_binding bound_where (ANFIR.Expr'Poison ty ())

