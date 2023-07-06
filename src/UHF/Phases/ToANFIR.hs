{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module UHF.Phases.ToANFIR (convert) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen

type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type ANFIR = ANFIR.ANFIR
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup

type BoundValueArena = Arena.Arena RIR.BoundValue RIR.BoundValueKey

type ANFIRBindingArena = Arena.Arena ANFIRBinding ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type BoundValueMap = Map.Map RIR.BoundValueKey ANFIR.BindingKey

type MakeGraphState = WriterT BoundValueMap (StateT (ANFIRBindingArena, ANFIRParamArena) (IDGen.IDGenT ID.ExprID (Reader BoundValueArena)))

make_binding_group :: [ANFIR.BindingKey] -> MakeGraphState ANFIRBindingGroup
make_binding_group bindings =
    Map.fromList <$> mapM (\ b -> (b,) <$> get_dependencies b) bindings >>= \ binding_dependencies ->

    -- get state outside of topological sort because if it was inside then the number of gets would be forced so that the monad would know how many operations it has and because topological sort branches based on binding_depenencies that will force it when it shouldnt
    fst <$> lift get >>= \ binding_arena ->
    let captures = Set.filter (not . (`List.elem` bindings)) (Set.unions $ Map.elems binding_dependencies)
        binding_dependencies_no_outwards = Map.map (Set.filter is_not_outward_dependency) binding_dependencies
        bindings_sorted = topological_sort binding_arena binding_dependencies_no_outwards [] bindings
    in

    pure (ANFIR.BindingGroup captures bindings_sorted)
    where
        get_dependencies bk =
            Arena.get <$> (fst <$> lift get) <*> pure bk >>= \ binding ->
            pure (
                case ANFIR.binding_initializer binding of
                    ANFIR.Expr'Refer _ _ i -> [i]
                    ANFIR.Expr'Char _ _ _ -> []
                    ANFIR.Expr'String _ _ _ -> []
                    ANFIR.Expr'Int _ _ _ -> []
                    ANFIR.Expr'Float _ _ _ -> []
                    ANFIR.Expr'Bool _ _ _ -> []
                    ANFIR.Expr'Tuple _ _ a b -> [a, b]
                    ANFIR.Expr'Lambda _ _ _ group result -> ANFIR.binding_group_captures group <> exclude_if_in_group group result
                    ANFIR.Expr'Param _ _ _ -> []
                    ANFIR.Expr'Call _ _ callee arg -> [callee, arg]
                    ANFIR.Expr'Switch _ _ test arms -> [test] <> Set.unions (map (\ (_, g, res) -> ANFIR.binding_group_captures g <> exclude_if_in_group g res) arms)
                    ANFIR.Expr'TupleDestructure1 _ _ tup -> [tup]
                    ANFIR.Expr'TupleDestructure2 _ _ tup -> [tup]
                    ANFIR.Expr'Forall _ _ _ group e -> ANFIR.binding_group_captures group <> exclude_if_in_group group e
                    ANFIR.Expr'TypeApply _ _ e _ -> [e]
                    ANFIR.Expr'MakeADT _ _ _ _ args -> Set.fromList args
                    ANFIR.Expr'Poison _ _ -> []
            )
            where
                exclude_if_in_group (ANFIR.BindingGroup _ chunks) binding
                    | binding `List.elem` (concatMap ANFIR.chunk_bindings chunks) = []
                    | otherwise = [binding]

        topological_sort _ _ done [] = done
        topological_sort binding_arena binding_dependencies done left =
            case List.partition dependencies_satisfied left of
                ([], waiting) ->
                    let (loops, not_loop) = find_loops waiting
                        loops' = map (deal_with_loop binding_arena) loops
                    in topological_sort binding_arena binding_dependencies (done ++ loops') not_loop

                (ready, waiting) -> topological_sort binding_arena binding_dependencies (done ++ map ANFIR.SingleBinding ready) waiting
            where
                dependencies_satisfied bk = and $ Set.map (`List.elem` concatMap ANFIR.chunk_bindings done) (binding_dependencies Map.! bk)

                find_loops = find [] []
                    where
                        find loops not_in_loop [] = (loops, not_in_loop)
                        find loops not_in_loop searching_for_loops_in@(first:more) =
                            case trace_single_loop [] first more of
                                Just (loop, bks_not_in_loop, remaining') -> find (loop:loops) (not_in_loop ++ bks_not_in_loop) remaining'
                                Nothing -> find loops (first:not_in_loop) more
                            where
                                trace_single_loop visited_stack current unvisited =
                                    case List.elemIndex current visited_stack of
                                        Just current_idx ->
                                            let (loop, not_in_loop) = splitAt (current_idx + 1) visited_stack -- top of stack is at beginning
                                            in Just (loop, not_in_loop, unvisited) -- all items of visited stack and current should not be in unvisited
                                        Nothing ->
                                            let current_dependencies = binding_dependencies Map.! current
                                            in headMay $
                                                mapMaybe
                                                    (\ neighbor -> trace_single_loop (current:visited_stack) neighbor (filter (/=neighbor) unvisited))
                                                    (toList $ Set.filter (`elem` searching_for_loops_in) current_dependencies)

                deal_with_loop binding_arena loop =
                    if and (map (allowed_in_loop . ANFIR.binding_initializer . Arena.get binding_arena) loop)
                        then ANFIR.MutuallyRecursiveBindings loop
                        else error "illegal loop" -- TODO: proper error message for this
                    where
                        allowed_in_loop (ANFIR.Expr'Lambda _ _ _ _ _) = True
                        allowed_in_loop (ANFIR.Expr'Forall _ _ _ _ _) = True
                        allowed_in_loop _ = False

        is_not_outward_dependency bk = bk `elem` bindings

convert :: RIR.RIR -> ANFIR
convert (RIR.RIR adts type_synonyms type_vars bound_values cu) =
    let ((cu', bv_map), (bindings, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (convert_cu bv_map cu)) (Arena.new, Arena.new))) bound_values
    in ANFIR.ANFIR adts type_synonyms type_vars bindings params cu'

convert_cu :: BoundValueMap -> RIR.CU -> MakeGraphState ANFIR.CU
convert_cu bv_map (RIR.CU bindings adts type_synonyms) = ANFIR.CU <$> (concat <$> mapM (convert_binding bv_map) bindings >>= make_binding_group) <*> pure adts <*> pure type_synonyms

map_bound_value :: RIR.BoundValueKey -> ANFIR.BindingKey -> MakeGraphState ()
map_bound_value k binding = tell $ Map.singleton k binding

get_bv :: RIR.BoundValueKey -> MakeGraphState RIR.BoundValue
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
        Just bvkey -> new_binding $ ANFIR.Expr'Refer (choose_id m_bvid id) ty (bv_map Map.! bvkey)
        Nothing -> new_binding $ ANFIR.Expr'Poison (choose_id m_bvid id) ty
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

convert_expr bv_map m_bvid (RIR.Expr'MakeADT id ty _ variant tyargs args) = ANFIR.Expr'MakeADT (choose_id m_bvid id) (Just ty) variant tyargs <$> mapM (convert_expr bv_map Nothing) args >>= new_binding

convert_expr _ m_bvid (RIR.Expr'Poison id ty _) = new_binding (ANFIR.Expr'Poison (choose_id m_bvid id) ty)
