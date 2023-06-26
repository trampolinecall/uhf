module UHF.Phases.Middle.ToANFIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

type Type = Maybe (Type.Type Void)

type RIRDecl = RIR.Decl
type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type ANFIR = ANFIR.ANFIR
type ANFIRDecl = ANFIR.Decl
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup

type BoundValueArena = Arena.Arena (RIR.BoundValue Type) RIR.BoundValueKey

type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BoundValueMap = Map.Map RIR.BoundValueKey ANFIR.BindingKey

type MakeGraphState = WriterT BoundValueMap (StateT (ANFIRBindingArena, ANFIRParamArena) (IDGen.IDGenT ID.ExprID (Reader BoundValueArena)))

make_binding_group :: [ANFIR.BindingKey] -> MakeGraphState ANFIRBindingGroup
make_binding_group bindings =
    pure (ANFIR.BindingGroup captures bindings_sorted)
    where
        binding_dependencies = todo
        captures = Set.fromList $ filter (not . (`List.elem` bindings)) (Map.elems binding_dependencies)

        bindings_sorted = todo

    {- TODO
        bindings_sorted = List.reverse $ sort [] bindings
        binding_dependencies = Map.fromList $ map (\ b -> (b, get_dependencies b)) bindings
            where
                get_dependencies b = Arena.get <$> get <*> pure b >>= \case
                    BackendIR.
annotate_expr _ (BackendIR.Expr'Refer id ty i) = [i]
annotate_expr _ (BackendIR.Expr'Char id ty c) = []
annotate_expr _ (BackendIR.Expr'String id ty s) = []
annotate_expr _ (BackendIR.Expr'Int id ty i) = []
annotate_expr _ (BackendIR.Expr'Float id ty r) = []
annotate_expr _ (BackendIR.Expr'Bool id ty b) = []
annotate_expr _ (BackendIR.Expr'Tuple id ty a b) = [a
annotate_expr binding_arena (BackendIR.Expr'Lambda id ty param group result) =
    let group' = annotate_binding_group binding_arena group
    in (BackendIR.binding_group_captures group' <> exclude_if_in_group binding_arena group' result, BackendIR.Expr'Lambda id ty param group' result)
annotate_expr _ (BackendIR.Expr'Param id ty param) = []
annotate_expr _ (BackendIR.Expr'Call id ty callee arg) = [callee
annotate_expr binding_arena (BackendIR.Expr'Switch id ty test arms) =
    let arms' = map (\ (p, group, e) -> (p, annotate_binding_group binding_arena group, e)) arms
    in
        ( [test] <> Set.unions (map (\ (_, g, res) -> BackendIR.binding_group_captures g <> exclude_if_in_group binding_arena g res) arms')
        , BackendIR.Expr'Switch id ty test arms'
        )
annotate_expr _ (BackendIR.Expr'TupleDestructure1 id ty tup) = [tup]
annotate_expr _ (BackendIR.Expr'TupleDestructure2 id ty tup) = [tup]
annotate_expr binding_arena (BackendIR.Expr'Forall id ty vars group e) =
    let group' = annotate_binding_group binding_arena group
    in (BackendIR.binding_group_captures group' <> exclude_if_in_group binding_arena group' e)
annotate_expr _ (BackendIR.Expr'TypeApply id ty e arg) = [e]
annotate_expr _ (BackendIR.Expr'MakeADT id ty variant args) = Set.fromList args
annotate_expr _ (BackendIR.Expr'Poison id ty allowed) = []

        -- returns result in reverse order; ie bindings with no dependencies appear at the end
        sort done left =
            let (ready, waiting) = List.partition dependencies_satisfied left
            in if null ready
                then
                    let (loops, not_loop) = find_loops waiting
                        loops' = map deal_with_loops loops
                    in sort (loops':done) not_loop
                else
                    sort (ready:done) waiting
    -}

convert :: RIR.RIR -> ANFIR
convert (RIR.RIR decls adts type_synonyms type_vars bound_values mod) =
    let ((decls', bv_map), (bindings, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (Arena.transformM (convert_decl bv_map) decls)) (Arena.new, Arena.new))) bound_values
    in ANFIR.ANFIR decls' adts type_synonyms type_vars bindings params mod

convert_decl :: BoundValueMap -> RIRDecl -> MakeGraphState ANFIRDecl
convert_decl bv_map (RIR.Decl'Module bindings adts type_synonyms) = ANFIR.Decl'Module <$> (concat <$> mapM (convert_binding bv_map) bindings >>= make_binding_group) <*> pure adts <*> pure type_synonyms
convert_decl _ (RIR.Decl'Type ty) = pure $ ANFIR.Decl'Type ty

map_bound_value :: RIR.BoundValueKey -> ANFIR.BindingKey -> MakeGraphState ()
map_bound_value k binding = tell $ Map.singleton k binding

get_bv :: RIR.BoundValueKey -> MakeGraphState (RIR.BoundValue (Maybe (Type.Type Void)))
get_bv k = lift $ lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: BoundValueMap -> RIRBinding -> MakeGraphState [ANFIR.BindingKey]
convert_binding bv_map (RIR.Binding target expr) =
    get_bv target >>= \ (RIR.BoundValue bvid _ _) ->
    runWriterT (convert_expr bv_map (Just bvid) expr) >>= \ (expr_result_binding, expr_involved_bindings) ->
    map_bound_value target expr_result_binding >>
    pure expr_involved_bindings

new_binding :: ANFIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
new_binding expr = lift (lift $ state $ \ (bindings, params) -> let (i, bindings') = Arena.put (ANFIR.Binding expr) bindings in (i, (bindings', params))) >>= \ binding_key -> tell [binding_key] >> pure binding_key
new_param :: ANFIRParam -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (bindings, params) -> let (i, params') = Arena.put param params in (i, (bindings, params')))

new_expr_id :: MakeGraphState ID.ExprID
new_expr_id = lift $ lift IDGen.gen_id

choose_id :: Maybe ID.BoundValueID -> ID.ExprID -> ANFIR.ID
choose_id (Just bvid) _ = ANFIR.BVID bvid
choose_id Nothing eid = ANFIR.ExprID eid

convert_expr :: BoundValueMap -> Maybe ID.BoundValueID -> RIRExpr -> WriterT [ANFIR.BindingKey] MakeGraphState ANFIR.BindingKey
convert_expr bv_map m_bvid (RIR.Expr'Identifier id ty _ bvkey) =
    case bvkey of
        Just bvkey -> new_binding (ANFIR.Expr'Refer (choose_id m_bvid id) ty (bv_map Map.! bvkey))
        Nothing -> new_binding (ANFIR.Expr'Poison (choose_id m_bvid id) ty)
convert_expr _ m_bvid (RIR.Expr'Char id ty _ c) = new_binding (ANFIR.Expr'Char (choose_id m_bvid id) ty c)
convert_expr _ m_bvid (RIR.Expr'String id ty _ s) = new_binding (ANFIR.Expr'String (choose_id m_bvid id) ty s)
convert_expr _ m_bvid (RIR.Expr'Int id ty _ i) = new_binding (ANFIR.Expr'Int (choose_id m_bvid id) ty i)
convert_expr _ m_bvid (RIR.Expr'Float id ty _ f) = new_binding (ANFIR.Expr'Float (choose_id m_bvid id) ty f)
convert_expr _ m_bvid (RIR.Expr'Bool id ty _ b) = new_binding (ANFIR.Expr'Bool (choose_id m_bvid id) ty b)

convert_expr bv_map m_bvid (RIR.Expr'Tuple id ty _ a b) = ANFIR.Expr'Tuple (choose_id m_bvid id) ty <$> convert_expr bv_map Nothing a <*> convert_expr bv_map Nothing b >>= new_binding

convert_expr bv_map m_bvid (RIR.Expr'Lambda id ty _ _ param_bv body) =
    lift (get_bv param_bv) >>= \ (RIR.BoundValue param_id param_ty _) ->
    new_param (ANFIR.Param param_id param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included bindings because they do not need to be evaluated to create the lambda object
        lift new_expr_id >>= \ param_binding_id ->
        new_binding (ANFIR.Expr'Param (ANFIR.ExprID param_binding_id) param_ty anfir_param) >>= \ param_binding ->
        lift (map_bound_value param_bv param_binding) >>
        convert_expr bv_map Nothing body
    ) >>= \ (body, body_included_bindings) ->
    lift (make_binding_group body_included_bindings) >>= \ body_group ->
    new_binding (ANFIR.Expr'Lambda (choose_id m_bvid id) ty anfir_param body_group body)

convert_expr bv_map _ (RIR.Expr'Let _ _ _ bindings e) = mapM (lift . convert_binding bv_map) bindings >>= \ binding_involved_bindings -> tell (concat binding_involved_bindings) >> convert_expr bv_map Nothing e

convert_expr bv_map m_bvid (RIR.Expr'Call id ty _ callee arg) = ANFIR.Expr'Call (choose_id m_bvid id) ty <$> convert_expr bv_map Nothing callee <*> convert_expr bv_map Nothing arg >>= new_binding

convert_expr bv_map m_bvid (RIR.Expr'Switch id ty _ testing arms) =
    convert_expr bv_map Nothing testing >>= \ testing ->
    ANFIR.Expr'Switch (choose_id m_bvid id) ty testing
        <$>
            mapM
                (\ (matcher, arm) ->
                    lift (runWriterT $
                        convert_matcher matcher testing >>= \ matcher ->
                        convert_expr bv_map Nothing arm >>= \ arm ->
                        pure (matcher, arm)) >>= \ ((matcher, arm), arm_involved_bindings) ->
                    (matcher,,arm) <$> lift (make_binding_group arm_involved_bindings))
                arms
        >>= new_binding
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
                Just a -> lift (get_bv a) >>= \ (RIR.BoundValue _ a_ty _) -> lift new_expr_id >>= \ id -> new_binding (ANFIR.Expr'TupleDestructure1 (ANFIR.ExprID id) a_ty testing) >>= \ a_destructure -> lift (map_bound_value a a_destructure)
                Nothing -> pure ()) >>
            (case b of
                Just b -> lift (get_bv b) >>= \ (RIR.BoundValue _ b_ty _) -> lift new_expr_id >>= \ id -> new_binding (ANFIR.Expr'TupleDestructure2 (ANFIR.ExprID id) b_ty testing) >>= \ b_destructure -> lift (map_bound_value b b_destructure)
                Nothing -> pure ()) >>
            pure ANFIR.Switch'Tuple
        convert_matcher RIR.Switch'Default _ = pure ANFIR.Switch'Default

convert_expr bv_map m_bvid (RIR.Expr'Forall id ty _ vars e) =
    lift (runWriterT (convert_expr bv_map Nothing e)) >>= \ (e, e_involved_bindings) ->
    ANFIR.Expr'Forall (choose_id m_bvid id) ty vars <$> lift (make_binding_group e_involved_bindings) <*> pure e >>= new_binding
convert_expr bv_map m_bvid (RIR.Expr'TypeApply id ty _ e arg) = ANFIR.Expr'TypeApply (choose_id m_bvid id) ty <$> convert_expr bv_map Nothing e <*> pure arg >>= new_binding

convert_expr bv_map m_bvid (RIR.Expr'MakeADT id ty _ variant args) = ANFIR.Expr'MakeADT (choose_id m_bvid id) (Just ty) variant <$> mapM (convert_expr bv_map Nothing) args >>= new_binding

convert_expr _ m_bvid (RIR.Expr'Poison id ty _) = new_binding (ANFIR.Expr'Poison (choose_id m_bvid id) ty)
