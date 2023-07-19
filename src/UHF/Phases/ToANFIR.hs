{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
import qualified Unique

type RIRExpr = RIR.Expr RIRCaptureList
type RIRBinding = RIR.Binding RIRCaptureList
type RIRCaptureList = Set.Set RIR.BoundValueKey

type ANFIR = ANFIR.ANFIR
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup

type BoundValueArena = Arena.Arena RIR.BoundValue RIR.BoundValueKey

type BindingArena b = Arena.Arena b ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

data BoundWhere = InGlobal | InLambda Unique.Unique deriving Eq
type DependencyList = Set.Set ANFIR.BindingKey

type NeedsTopoSort result = forall e. Reader (BindingArena (WithTopoSortInfo e)) result
type WithTopoSortInfo e = (DependencyList, e)

type ExprNeedsBVMap = BoundValueMap -> WithTopoSortInfo ExprNeedsTopoSort
type ExprNeedsTopoSort = NeedsTopoSort ANFIRExpr

type BoundValueMap = Map.Map RIR.BoundValueKey ANFIR.BindingKey

type MakeGraphState binding = WriterT BoundValueMap (StateT (BindingArena binding, ANFIRParamArena) (IDGen.IDGenT ID.ExprID (Reader BoundValueArena)))

-- also returns all the dependencies of all the bindings in the binding group except the ones that are defined in inner lambdas
make_binding_group :: [ANFIR.BindingKey] -> Reader (BindingArena (WithTopoSortInfo e)) (ANFIRBindingGroup, Set.Set ANFIR.BindingKey)
make_binding_group bindings =
    reader $ \ binding_arena ->
        let binding_dependencies = Map.fromList $ map (\ b -> (b, get_dependencies binding_arena b)) bindings
            binding_dependencies_only_here = Map.map (Set.filter (`elem` bindings)) binding_dependencies
            bindings_sorted = topological_sort (todo binding_arena) binding_dependencies_only_here [] bindings
        in (ANFIR.BindingGroup bindings_sorted, Set.unions $ Map.elems binding_dependencies)
    where
        get_dependencies binding_arena bk =
            let (dependencies, binding) = Arena.get binding_arena bk
            in dependencies
            {- TODO: remove
            case ANFIR.binding_initializer binding of
                ANFIR.Expr'Refer _ _ i -> [i]
                ANFIR.Expr'Char _ _ _ -> []
                ANFIR.Expr'String _ _ _ -> []
                ANFIR.Expr'Int _ _ _ -> []
                ANFIR.Expr'Float _ _ _ -> []
                ANFIR.Expr'Bool _ _ _ -> []
                ANFIR.Expr'Tuple _ _ a b -> [a, b]
                ANFIR.Expr'Lambda _ _ _ captures group result -> captures <> exclude_if_in_group group result
                ANFIR.Expr'Param _ _ _ -> []
                ANFIR.Expr'Call _ _ callee arg -> [callee, arg]
                ANFIR.Expr'Switch _ _ test arms ->
                    [test]
                        <> Set.unions (
                                map
                                    (\ (_, g, res) ->
                                        -- this can return dependencies from the inner binding group but that is fine because they are filtered out with the 'Set.filter (`elem` bindings)' in binding_dependencies_only_here
                                        Set.unions (map (get_dependencies binding_arena) (concatMap ANFIR.chunk_bindings (ANFIR.binding_group_chunks g))) <> exclude_if_in_group g res
                                    )
                                    arms
                            )
                ANFIR.Expr'TupleDestructure1 _ _ tup -> [tup]
                ANFIR.Expr'TupleDestructure2 _ _ tup -> [tup]
                ANFIR.Expr'Forall _ _ _ group e -> Set.unions (map (get_dependencies binding_arena) (concatMap ANFIR.chunk_bindings (ANFIR.binding_group_chunks group))) <> exclude_if_in_group group e -- same thing about dependencies from the inner group
                ANFIR.Expr'TypeApply _ _ e _ -> [e]
                ANFIR.Expr'MakeADT _ _ _ _ args -> Set.fromList args
                ANFIR.Expr'Poison _ _ -> []
            -}
            where
                exclude_if_in_group (ANFIR.BindingGroup chunks) binding
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
                    if and (map (allowed_in_loop . ANFIR.binding_initializer . snd . Arena.get binding_arena) loop)
                        then ANFIR.MutuallyRecursiveBindings loop
                        else error "illegal loop" -- TODO: proper error message for this
                    where
                        -- TODO: fix this because this allows loops where with only foralls
                        -- eg x = #(A) x#(A) is allowed
                        allowed_in_loop (ANFIR.Expr'Lambda _ _ _ _ _ _) = True
                        allowed_in_loop (ANFIR.Expr'Forall _ _ _ _ _) = True
                        allowed_in_loop _ = False

convert :: RIR.RIR RIRCaptureList -> ANFIR
convert (RIR.RIR adts type_synonyms type_vars bound_values cu) =
    let ((cu_needs_deps, bv_map), (bindings_needs_bv_map, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (convert_cu cu)) (Arena.new, Arena.new))) bound_values
        bindings_needs_deps = Arena.transform ($ bv_map) bindings_needs_bv_map
        bindings = Arena.transform (\ (_, x) -> ANFIR.Binding $ runReader x bindings_needs_deps) bindings_needs_deps
        cu' = runReader cu_needs_deps bindings_needs_deps
    in ANFIR.ANFIR adts type_synonyms type_vars bindings params cu'

convert_cu :: RIR.CU RIRCaptureList -> MakeGraphState (ExprNeedsBVMap) (NeedsTopoSort ANFIR.CU)
convert_cu (RIR.CU bindings adts type_synonyms) = concat <$> mapM convert_binding bindings >>= \ (bindings) -> pure (make_binding_group bindings >>= \ (group, _) -> pure (ANFIR.CU group adts type_synonyms))

map_bound_value :: RIR.BoundValueKey -> ANFIR.BindingKey -> MakeGraphState binding ()
map_bound_value k binding = tell $ Map.singleton k binding

get_bv :: RIR.BoundValueKey -> MakeGraphState binding RIR.BoundValue
get_bv k = lift $ lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: RIRBinding -> MakeGraphState ExprNeedsBVMap [ANFIR.BindingKey]
convert_binding (RIR.Binding target expr) =
    get_bv target >>= \ (RIR.BoundValue bvid _ _) ->
    runWriterT (convert_expr (Just bvid) expr) >>= \ (expr_result_binding, expr_involved_bindings) ->
    map_bound_value target expr_result_binding >>
    pure expr_involved_bindings

new_binding :: binding -> WriterT [ANFIR.BindingKey] (MakeGraphState binding) ANFIR.BindingKey
new_binding binding = lift (lift $ state $ \ (bindings, params) -> let (i, bindings') = Arena.put binding bindings in (i, (bindings', params))) >>= \ binding_key -> tell [binding_key] >> pure binding_key
new_param :: ANFIRParam -> WriterT [ANFIR.BindingKey] (MakeGraphState binding) ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (bindings, params) -> let (i, params') = Arena.put param params in (i, (bindings, params')))

new_expr_id :: MakeGraphState binding ID.ExprID
new_expr_id = lift $ lift IDGen.gen_id

choose_id :: Maybe ID.BoundValueID -> ID.ExprID -> ANFIR.ID
choose_id (Just bvid) _ = ANFIR.BVID bvid
choose_id Nothing eid = ANFIR.ExprID eid

get_dependencies_of_binding_list_and_expr :: BindingArena ExprNeedsBVMap -> BoundValueMap -> [ANFIR.BindingKey] -> ANFIR.BindingKey -> Set.Set ANFIR.BindingKey
get_dependencies_of_binding_list_and_expr binding_arena bv_map bindings e =
    let bindings_dependencies = Set.unions $ map (fst . ($ bv_map) . Arena.get binding_arena) bindings
    in (bindings_dependencies <> [e]) `Set.difference` Set.fromList bindings

convert_expr :: Maybe ID.BoundValueID -> RIRExpr -> WriterT [ANFIR.BindingKey] (MakeGraphState (ExprNeedsBVMap)) ANFIR.BindingKey
convert_expr m_bvid (RIR.Expr'Identifier id ty _ bvkey) =
    case bvkey of
        Just bvkey ->
            new_binding $
                \ bv_map ->
                    let anfir_bv = bv_map Map.! bvkey
                    in ([anfir_bv], pure $ ANFIR.Expr'Refer (choose_id m_bvid id) ty anfir_bv)
        Nothing -> new_binding $ \ _ -> ([], pure $ ANFIR.Expr'Poison (choose_id m_bvid id) ty)
convert_expr m_bvid (RIR.Expr'Char id ty _ c) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'Char (choose_id m_bvid id) ty c))
convert_expr m_bvid (RIR.Expr'String id ty _ s) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'String (choose_id m_bvid id) ty s))
convert_expr m_bvid (RIR.Expr'Int id ty _ i) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'Int (choose_id m_bvid id) ty i))
convert_expr m_bvid (RIR.Expr'Float id ty _ f) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'Float (choose_id m_bvid id) ty f))
convert_expr m_bvid (RIR.Expr'Bool id ty _ b) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'Bool (choose_id m_bvid id) ty b))

convert_expr m_bvid (RIR.Expr'Tuple id ty _ a b) = convert_expr Nothing a >>= \ a -> convert_expr Nothing b >>= \ b -> new_binding (\ _ -> ([a, b], pure $ ANFIR.Expr'Tuple (choose_id m_bvid id) ty a b))

convert_expr m_bvid (RIR.Expr'Lambda id ty _ rir_captures param_bv body) =
    lift (get_bv param_bv) >>= \ (RIR.BoundValue param_id param_ty _) ->
    new_param (ANFIR.Param param_id param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included bindings because they do not need to be evaluated to create the lambda object
        lift new_expr_id >>= \ param_binding_id ->
        new_binding (\ _ -> ([], pure $ ANFIR.Expr'Param (ANFIR.ExprID param_binding_id) param_ty anfir_param)) >>= \ param_binding ->
        lift (map_bound_value param_bv param_binding) >>
        convert_expr Nothing body
    ) >>= \ (body, body_included_bindings) ->

    new_binding
        (\ bv_map ->
            let anfir_captures = Set.map (bv_map Map.!) rir_captures
            in
                ( anfir_captures
                ,
                    make_binding_group body_included_bindings >>= \ (body_group, body_deps) ->
                    pure (ANFIR.Expr'Lambda (choose_id m_bvid id) ty anfir_param anfir_captures body_group body)
                )
        )

convert_expr _ (RIR.Expr'Let _ _ _ bindings e) = mapM (lift . convert_binding) bindings >>= \ binding_involved_bindings -> tell (concat binding_involved_bindings) >> convert_expr Nothing e

convert_expr m_bvid (RIR.Expr'Call id ty _ callee arg) = convert_expr Nothing callee >>= \ callee -> convert_expr Nothing arg >>= \ arg -> new_binding (\ _ -> ([callee, arg], pure $ ANFIR.Expr'Call (choose_id m_bvid id) ty callee arg))

convert_expr m_bvid (RIR.Expr'Switch id ty _ testing arms) =
    convert_expr Nothing testing >>= \ testing ->
    mapM
        (\ (matcher, arm) ->
            lift (runWriterT $
                convert_matcher matcher testing >>= \ matcher ->
                convert_expr Nothing arm >>= \ arm ->
                pure (matcher, arm)) >>= \ ((matcher, arm), arm_involved_bindings) ->
            pure (matcher, arm_involved_bindings, arm))
        arms >>= \ arms ->

    lift (lift get) >>= \ (binding_arena, _) ->
    let testing_dependencies bv_map =
            let almost_expr = Arena.get binding_arena testing
            in fst $ almost_expr bv_map -- need to be in let because of forall in NeedsTopoSort cannot be unified with unification variable if it is pointfree
        arms_dependencies bv_map = arms
            & map (\ (_, bindings, res) -> get_dependencies_of_binding_list_and_expr binding_arena bv_map bindings res)
            & Set.unions
    in

    new_binding
        (\ bv_map ->
            ( testing_dependencies bv_map <> arms_dependencies bv_map
            ,
                mapM (\ (matcher, bindings, result) -> make_binding_group bindings >>= \ (group, _) -> pure (matcher, group, result)) arms >>= \ arms ->
                pure (ANFIR.Expr'Switch (choose_id m_bvid id) ty testing arms)
            )
        )
    where
        convert_matcher :: RIR.SwitchMatcher -> ANFIR.BindingKey -> WriterT [ANFIR.BindingKey] (MakeGraphState ExprNeedsBVMap) ANFIR.SwitchMatcher
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
                Just a ->
                    lift (get_bv a) >>= \ (RIR.BoundValue _ a_ty _) ->
                    lift new_expr_id >>= \ id ->
                    new_binding (\ _ -> ([testing], pure $ ANFIR.Expr'TupleDestructure1 (ANFIR.ExprID id) a_ty testing)) >>= \ a_destructure ->
                    lift (map_bound_value a a_destructure)
                Nothing -> pure ()) >>
            (case b of
                Just b ->
                    lift (get_bv b) >>= \ (RIR.BoundValue _ b_ty _) ->
                    lift new_expr_id >>= \ id ->
                    new_binding (\ _ -> ([testing], pure $ ANFIR.Expr'TupleDestructure2 (ANFIR.ExprID id) b_ty testing)) >>= \ b_destructure ->
                    lift (map_bound_value b b_destructure)
                Nothing -> pure ()) >>
            pure ANFIR.Switch'Tuple
        convert_matcher RIR.Switch'Default _ = pure ANFIR.Switch'Default

convert_expr m_bvid (RIR.Expr'Forall id ty _ vars e) =
    lift (runWriterT (convert_expr Nothing e)) >>= \ (e, e_involved_bindings) ->

    lift (lift get) >>= \ (binding_arena, _) ->
    let e_dependencies bv_map =
            let almost_expr = Arena.get binding_arena e
            in fst $ almost_expr bv_map -- same note as above about needing to be in a let because of polytypes and unification
    in

    new_binding
        (\ bv_map ->
            ( e_dependencies bv_map
            ,
                fst <$> make_binding_group e_involved_bindings >>= \ group ->
                pure (ANFIR.Expr'Forall (choose_id m_bvid id) ty vars group e)
            )
        )
convert_expr m_bvid (RIR.Expr'TypeApply id ty _ e arg) = convert_expr Nothing e >>= \ e -> new_binding (\ _ -> ([e], pure $ ANFIR.Expr'TypeApply (choose_id m_bvid id) ty e arg))

convert_expr m_bvid (RIR.Expr'MakeADT id ty _ variant tyargs args) = mapM (convert_expr Nothing) args >>= \ args -> new_binding (\ _ -> (Set.fromList args, pure $ ANFIR.Expr'MakeADT (choose_id m_bvid id) (Just ty) variant tyargs args))

convert_expr m_bvid (RIR.Expr'Poison id ty _) = new_binding (\ _ -> ([], pure $ ANFIR.Expr'Poison (choose_id m_bvid id) ty))
