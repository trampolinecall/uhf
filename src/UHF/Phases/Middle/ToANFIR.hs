module UHF.Phases.Middle.ToANFIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

type Type = Maybe (Type.Type Void)
type CaptureList = Set RIR.BoundValueKey

type RIRDecl = RIR.Decl CaptureList
type RIRExpr = RIR.Expr CaptureList
type RIRBinding = RIR.Binding CaptureList

type ANFIR = ANFIR.ANFIR Type ()
type ANFIRDecl = ANFIR.Decl
type ANFIRExpr = ANFIR.Expr Type ()
type ANFIRParam = ANFIR.Param Type
type ANFIRBinding = ANFIR.Binding Type ()

type BoundValueArena = Arena.Arena (RIR.BoundValue (Maybe (Type.Type Void))) RIR.BoundValueKey

type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BoundValueMap = Map.Map RIR.BoundValueKey ANFIR.BindingKey

type MakeGraphState = WriterT BoundValueMap (StateT (ANFIRBindingArena, ANFIRParamArena) (IDGen.IDGenT ID.ExprID (Reader BoundValueArena)))

convert :: RIR.RIR CaptureList -> ANFIR
convert (RIR.RIR decls adts type_synonyms bound_values mod) =
    let ((decls', bv_map), (bindings, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new))) bound_values
    in ANFIR.ANFIR decls' adts type_synonyms bindings params mod

convert_decl :: BoundValueMap -> RIRDecl -> MakeGraphState ANFIRDecl
convert_decl bv_map (RIR.Decl'Module bindings adts type_synonyms) = ANFIR.Decl'Module <$> (concat <$> mapM (convert_binding bv_map) bindings) <*> pure adts <*> pure type_synonyms
convert_decl _ (RIR.Decl'Type ty) = pure $ ANFIR.Decl'Type ty

map_bound_value :: RIR.BoundValueKey -> ANFIR.BindingKey -> MakeGraphState ()
map_bound_value k binding = tell $ Map.singleton k binding

get_bv :: RIR.BoundValueKey -> MakeGraphState (RIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = lift $ lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: BoundValueMap -> RIRBinding -> MakeGraphState [ANFIR.BindingKey]
convert_binding bv_map (RIR.Binding target expr) =
    runWriterT (convert_expr bv_map expr) >>= \ (expr_result_binding, expr_involved_bindings) ->
    map_bound_value target expr_result_binding >>
    pure expr_involved_bindings

new_binding :: ANFIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
new_binding expr = lift (lift $ state $ \ (g, p) -> let (i, g') = Arena.put (ANFIR.Binding expr) g in (i, (g', p))) >>= \ binding_key -> tell [binding_key] >> pure binding_key
new_param :: ANFIRParam -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (g, p) -> let (i, p') = Arena.put param p in (i, (g, p')))

convert_expr :: BoundValueMap -> RIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
convert_expr bv_map (RIR.Expr'Identifier id ty _ bvkey) =
    case bvkey of
        Just bvkey -> new_binding (ANFIR.Expr'Identifier id ty (bv_map Map.! bvkey))
        Nothing -> new_binding (ANFIR.Expr'Poison id ty ())
convert_expr _ (RIR.Expr'Char id ty _ c) = new_binding (ANFIR.Expr'Char id ty c)
convert_expr _ (RIR.Expr'String id ty _ s) = new_binding (ANFIR.Expr'String id ty s)
convert_expr _ (RIR.Expr'Int id ty _ i) = new_binding (ANFIR.Expr'Int id ty i)
convert_expr _ (RIR.Expr'Float id ty _ f) = new_binding (ANFIR.Expr'Float id ty f)
convert_expr _ (RIR.Expr'Bool id ty _ b) = new_binding (ANFIR.Expr'Bool id ty b)

convert_expr bv_map (RIR.Expr'Tuple id ty _ a b) = ANFIR.Expr'Tuple id ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_binding

convert_expr bv_map (RIR.Expr'Lambda id ty _ _ captures param_bv body) =
    lift (get_bv param_bv) >>= \ (RIR.BoundValue param_id param_ty _ _) ->
    new_param (ANFIR.Param param_id param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included bindings because they do not need to be evaluated to create the lambda object
        new_binding (ANFIR.Expr'Param (ID.add ID.LambdaReferParam id) param_ty anfir_param) >>= \ param_binding ->
        lift (map_bound_value param_bv param_binding) >>
        convert_expr bv_map body
    ) >>= \ (body, body_included_bindings) ->
    new_binding (ANFIR.Expr'Lambda id ty (Set.map (bv_map Map.!) captures) anfir_param body_included_bindings body)

convert_expr bv_map (RIR.Expr'Let _ _ _ bindings e) = mapM (lift . convert_binding bv_map) bindings >>= \ binding_involved_bindings -> tell (concat binding_involved_bindings) >> convert_expr bv_map e

convert_expr bv_map (RIR.Expr'Call id ty _ callee arg) = ANFIR.Expr'Call id ty <$> convert_expr bv_map callee <*> convert_expr bv_map arg >>= new_binding

convert_expr bv_map (RIR.Expr'Switch id ty _ testing arms) =
    convert_expr bv_map testing >>= \ testing ->
    ANFIR.Expr'Switch id ty testing <$> mapM (\ (matcher, arm) -> (,) <$> convert_matcher matcher testing <*> convert_expr bv_map arm) arms >>= new_binding
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
                Just a -> lift (get_bv a) >>= \ (RIR.BoundValue _ a_ty _ _) -> lift (lift $ lift IDGen.gen_id) >>= \ id -> new_binding (ANFIR.Expr'TupleDestructure1 id a_ty testing) >>= \ a_destructure -> lift (map_bound_value a a_destructure)
                Nothing -> pure ()) >>
            (case b of
                Just b -> lift (get_bv b) >>= \ (RIR.BoundValue _ b_ty _ _) -> lift (lift $ lift IDGen.gen_id) >>= \ id -> new_binding (ANFIR.Expr'TupleDestructure2 id b_ty testing) >>= \ b_destructure -> lift (map_bound_value b b_destructure)
                Nothing -> pure ()) >>
            pure ANFIR.Switch'Tuple
        convert_matcher RIR.Switch'Default _ = pure ANFIR.Switch'Default

convert_expr bv_map (RIR.Expr'Seq id ty _ a b) = ANFIR.Expr'Seq id ty <$> convert_expr bv_map a <*> convert_expr bv_map b >>= new_binding

convert_expr _ (RIR.Expr'Poison id ty _) = new_binding (ANFIR.Expr'Poison id ty ())
