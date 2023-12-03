{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module UHF.Phases.ToANFIR (convert) where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.IDGen as IDGen
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.RIR as RIR
import qualified UHF.Util.Arena as Arena

type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

type ANFIR = ANFIR.ANFIR
type ANFIRExpr = ANFIR.Expr
type ANFIRParam = ANFIR.Param
type ANFIRBinding = ANFIR.Binding
type ANFIRBindingGroup = ANFIR.BindingGroup

type VariableArena = Arena.Arena RIR.Variable RIR.VariableKey

type BindingArena b = Arena.Arena b ANFIR.BindingKey
type ANFIRParamArena = Arena.Arena ANFIRParam ANFIR.ParamKey

type NeedsVarMap e = VariableMap -> e
type NeedsTopoSort result = Reader (BindingArena AlmostExpr) result

type VariableMap = Map.Map RIR.VariableKey ANFIR.BindingKey

type MakeGraphState binding = WriterT VariableMap (StateT (BindingArena binding, ANFIRParamArena) (IDGen.IDGenT ID.ExprID (Reader VariableArena)))

-- the same thing as ANFIR.Expr except lambdas dont have captures and all the binding groups are actually just [BindingKey]
data AlmostExpr
    = AlmostExpr'Refer ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey

    | AlmostExpr'Int ANFIR.ID (Maybe (Type.Type Void)) Integer
    | AlmostExpr'Float ANFIR.ID (Maybe (Type.Type Void)) Rational
    | AlmostExpr'Bool ANFIR.ID (Maybe (Type.Type Void)) Bool
    | AlmostExpr'Char ANFIR.ID (Maybe (Type.Type Void)) Char
    | AlmostExpr'String ANFIR.ID (Maybe (Type.Type Void)) Text
    | AlmostExpr'Tuple ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey ANFIR.BindingKey
    | AlmostExpr'MakeADT ANFIR.ID (Maybe (Type.Type Void)) Type.ADT.VariantIndex [Maybe (Type.Type Void)] [ANFIR.BindingKey]

    | AlmostExpr'Lambda ANFIR.ID (Maybe (Type.Type Void)) ANFIR.ParamKey [ANFIR.BindingKey] ANFIR.BindingKey
    | AlmostExpr'Param ANFIR.ID (Maybe (Type.Type Void)) ANFIR.ParamKey

    | AlmostExpr'Call ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey ANFIR.BindingKey

    | AlmostExpr'Match ANFIR.ID (Maybe (Type.Type Void)) AlmostMatchTree

    | AlmostExpr'TupleDestructure1 ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey
    | AlmostExpr'TupleDestructure2 ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey
    | AlmostExpr'ADTDestructure ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey (Maybe Type.ADT.FieldIndex)

    | AlmostExpr'Forall ANFIR.ID (Maybe (Type.Type Void)) (NonEmpty Type.QuantVarKey) [ANFIR.BindingKey] ANFIR.BindingKey
    | AlmostExpr'TypeApply ANFIR.ID (Maybe (Type.Type Void)) ANFIR.BindingKey (Maybe (Type.Type Void))

    | AlmostExpr'Poison ANFIR.ID (Maybe (Type.Type Void))

data AlmostMatchTree
    = AlmostMatchTree [([ANFIR.MatchClause], Either AlmostMatchTree ([ANFIR.BindingKey], ANFIR.BindingKey))]

convert :: RIR.RIR -> ANFIR
convert (RIR.RIR adts type_synonyms type_vars variables cu) =
    let (bindings_step_1, params, cu_step_1) = convert_step_1 variables cu
        (bindings_step_2, cu_step_2) = convert_step_2 bindings_step_1 cu_step_1
    in ANFIR.ANFIR adts type_synonyms type_vars bindings_step_2 params cu_step_2

-- step 1: converting from rir to almost anfir {{{1
convert_step_1 :: VariableArena -> RIR.CU -> (BindingArena AlmostExpr, ANFIRParamArena, NeedsTopoSort ANFIR.CU)
convert_step_1 variables cu =
    let ((cu_needs_deps, var_map), (bindings_needs_var_map, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (convert_cu cu)) (Arena.new, Arena.new))) variables
        bindings_needs_deps = Arena.transform ($ var_map) bindings_needs_var_map
    in (bindings_needs_deps, params, cu_needs_deps)

convert_cu :: RIR.CU -> MakeGraphState (NeedsVarMap AlmostExpr) (NeedsTopoSort ANFIR.CU)
convert_cu (RIR.CU bindings adts type_synonyms) = concat <$> mapM convert_binding bindings >>= \ bindings -> pure (make_binding_group bindings >>= \ group -> pure (ANFIR.CU group adts type_synonyms))

map_variable :: RIR.VariableKey -> ANFIR.BindingKey -> MakeGraphState binding ()
map_variable k binding = tell $ Map.singleton k binding

get_var :: RIR.VariableKey -> MakeGraphState binding RIR.Variable
get_var k = lift $ lift $ lift $ reader (\ a -> Arena.get a k)

convert_binding :: RIRBinding -> MakeGraphState (NeedsVarMap AlmostExpr) [ANFIR.BindingKey]
convert_binding (RIR.Binding target expr) =
    get_var target >>= \ (RIR.Variable varid _ _) ->
    runWriterT (convert_expr (Just varid) expr) >>= \ (expr_result_binding, expr_involved_bindings) ->
    map_variable target expr_result_binding >>
    pure expr_involved_bindings

new_binding :: binding -> WriterT [ANFIR.BindingKey] (MakeGraphState binding) ANFIR.BindingKey
new_binding binding = lift (lift $ state $ \ (bindings, params) -> let (i, bindings') = Arena.put binding bindings in (i, (bindings', params))) >>= \ binding_key -> tell [binding_key] >> pure binding_key
new_param :: ANFIRParam -> WriterT [ANFIR.BindingKey] (MakeGraphState binding) ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (bindings, params) -> let (i, params') = Arena.put param params in (i, (bindings, params')))

new_expr_id :: MakeGraphState binding ID.ExprID
new_expr_id = lift $ lift IDGen.gen_id

choose_id :: Maybe ID.VariableID -> ID.ExprID -> ANFIR.ID
choose_id (Just varid) _ = ANFIR.VarID varid
choose_id Nothing eid = ANFIR.ExprID eid

convert_expr :: Maybe ID.VariableID -> RIR.Expr -> WriterT [ANFIR.BindingKey] (MakeGraphState (NeedsVarMap AlmostExpr)) ANFIR.BindingKey
convert_expr m_varid expr@(RIR.Expr'Identifier id _ _ varkey) =
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    case varkey of
        Just varkey -> new_binding $ \ var_map -> AlmostExpr'Refer (choose_id m_varid id) ty (var_map Map.! varkey)
        Nothing -> new_binding $ \ _ -> AlmostExpr'Poison (choose_id m_varid id) ty
convert_expr m_varid expr@(RIR.Expr'Char id _ c) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> AlmostExpr'Char (choose_id m_varid id) ty c)

convert_expr m_varid expr@(RIR.Expr'String id _ s) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> AlmostExpr'String (choose_id m_varid id) ty s)
convert_expr m_varid expr@(RIR.Expr'Int id _ i) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> AlmostExpr'Int (choose_id m_varid id) ty i)
convert_expr m_varid expr@(RIR.Expr'Float id _ f) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> AlmostExpr'Float (choose_id m_varid id) ty f)
convert_expr m_varid expr@(RIR.Expr'Bool id _ b) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> AlmostExpr'Bool (choose_id m_varid id) ty b)

convert_expr m_varid expr@(RIR.Expr'Tuple id _ a b) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in convert_expr Nothing a >>= \ a -> convert_expr Nothing b >>= \ b -> new_binding (\ _ -> AlmostExpr'Tuple (choose_id m_varid id) ty a b)

convert_expr m_varid expr@(RIR.Expr'Lambda id _ param_var body) =
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    lift (get_var param_var) >>= \ (RIR.Variable param_id param_ty _) ->
    new_param (ANFIR.Param param_id param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included bindings because they do not need to be evaluated to create the lambda object
        lift new_expr_id >>= \ param_binding_id ->
        new_binding (\ _ -> AlmostExpr'Param (ANFIR.ExprID param_binding_id) param_ty anfir_param) >>= \ param_binding ->
        lift (map_variable param_var param_binding) >>
        convert_expr Nothing body
    ) >>= \ (body, body_included_bindings) ->

    new_binding (\ _ -> AlmostExpr'Lambda (choose_id m_varid id) ty anfir_param body_included_bindings body)

convert_expr _ (RIR.Expr'Let _ _ bindings e) = mapM (lift . convert_binding) bindings >>= \ binding_involved_bindings -> tell (concat binding_involved_bindings) >> convert_expr Nothing e

convert_expr m_varid expr@(RIR.Expr'Call id _ callee arg) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in convert_expr Nothing callee >>= \ callee -> convert_expr Nothing arg >>= \ arg -> new_binding (\ _ -> AlmostExpr'Call (choose_id m_varid id) ty callee arg)

convert_expr m_varid (RIR.Expr'Match id ty _ tree) =
    lift (convert_tree tree) >>= \ tree ->

    new_binding (\ var_map -> AlmostExpr'Match (choose_id m_varid id) ty (tree var_map))
    where
        convert_tree (RIR.MatchTree arms) =
            mapM
                (\ (clauses, result) ->
                    mapM convert_clause clauses >>= \ clauses ->
                    (case result of
                        Right e ->
                            runWriterT (convert_expr Nothing e) >>= \ (result, result_involved_bindings) ->
                            pure (\ _ -> Right (result_involved_bindings, result))
                        Left subtree ->
                            convert_tree subtree >>= \ subtree ->
                            pure (\ var_map -> Left (subtree var_map))) >>= \ result ->
                    pure (\ var_map -> (concatMap ($ var_map) clauses, result var_map)))
                arms >>= \ arms ->
            pure (\ var_map -> AlmostMatchTree (map ($ var_map) arms))

        convert_clause (RIR.MatchClause'Match binding (RIR.Match'BoolLiteral bool)) = pure (\ var_map -> [ANFIR.MatchClause'Match (var_map Map.! binding) (ANFIR.Match'BoolLiteral bool)])
        convert_clause (RIR.MatchClause'Match c RIR.Match'Tuple) = pure (\ var_map -> [ANFIR.MatchClause'Match (var_map Map.! c) ANFIR.Match'Tuple])
        convert_clause (RIR.MatchClause'Match scrutinee (RIR.Match'AnonADTVariant m_variant_index)) = pure (\ var_map -> [ANFIR.MatchClause'Match (var_map Map.! scrutinee) (ANFIR.Match'AnonADTVariant m_variant_index)])

        convert_clause (RIR.MatchClause'Assign target rhs) =
            convert_assign_rhs rhs >>= \ rhs_binding ->
            map_variable target rhs_binding >>
            pure (\ _ -> [ANFIR.MatchClause'Binding rhs_binding])

        convert_assign_rhs (RIR.MatchAssignRHS'OtherVar other) =
            get_var other >>= \ (RIR.Variable _ other_ty _) ->
            new_expr_id >>= \ id ->
            -- second element is all the bindings made, but because there is only one call to new_binding in this WriterT, the only binding ever made is this one, so we do not need to keep track of the second variable
            runWriterT (new_binding (\ var_map -> AlmostExpr'Refer (ANFIR.ExprID id) other_ty (var_map Map.! other))) >>= \ (binding, _) ->
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'TupleDestructure1 ty tup) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> AlmostExpr'TupleDestructure1 (ANFIR.ExprID id) ty (var_map Map.! tup))) >>= \ (binding, _) -> -- same note about all the bindings made as above
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'TupleDestructure2 ty tup) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> AlmostExpr'TupleDestructure2 (ANFIR.ExprID id) ty (var_map Map.! tup))) >>= \ (binding, _) -> -- same note as above
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'AnonADTVariantField ty base field_idx) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> AlmostExpr'ADTDestructure (ANFIR.ExprID id) ty (var_map Map.! base) field_idx)) >>= \ (binding, _) -> -- same note as above
            pure binding

convert_expr m_varid expr@(RIR.Expr'Forall id _ vars e) =
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    lift (runWriterT (convert_expr Nothing e)) >>= \ (e, e_involved_bindings) ->
    new_binding (\ _ -> AlmostExpr'Forall (choose_id m_varid id) ty vars e_involved_bindings e)
convert_expr m_varid (RIR.Expr'TypeApply id ty _ e arg) = convert_expr Nothing e >>= \ e -> new_binding (\ _ -> AlmostExpr'TypeApply (choose_id m_varid id) ty e arg)

convert_expr m_varid expr@(RIR.Expr'MakeADT id _ variant tyargs args) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in mapM (convert_expr Nothing) args >>= \ args -> new_binding (\ _ -> AlmostExpr'MakeADT (choose_id m_varid id) ty variant tyargs args)

convert_expr m_varid (RIR.Expr'Poison id ty _) = new_binding (\ _ -> AlmostExpr'Poison (choose_id m_varid id) ty)
-- second step: converting from almost anfir to actual anfir {{{1
convert_step_2 :: BindingArena AlmostExpr -> Reader (BindingArena AlmostExpr) ANFIR.CU -> (BindingArena ANFIR.Binding, ANFIR.CU)
convert_step_2 bindings cu =
    let bindings' = runReader (Arena.transformM (\ x -> ANFIR.Binding <$> convert_almost_expr x) bindings) bindings
        cu' = runReader cu bindings
    in (bindings', cu')

convert_almost_expr :: AlmostExpr -> Reader (BindingArena AlmostExpr) ANFIR.Expr
convert_almost_expr (AlmostExpr'Refer id ty bk) = pure $ ANFIR.Expr'Refer id ty bk
convert_almost_expr (AlmostExpr'Int id ty i) = pure $ ANFIR.Expr'Int id ty i
convert_almost_expr (AlmostExpr'Float id ty f) = pure $ ANFIR.Expr'Float id ty f
convert_almost_expr (AlmostExpr'Bool id ty b) = pure $ ANFIR.Expr'Bool id ty b
convert_almost_expr (AlmostExpr'Char id ty c) = pure $ ANFIR.Expr'Char id ty c
convert_almost_expr (AlmostExpr'String id ty s) = pure $ ANFIR.Expr'String id ty s
convert_almost_expr (AlmostExpr'Tuple id ty a b) = pure $ ANFIR.Expr'Tuple id ty a b
convert_almost_expr (AlmostExpr'MakeADT id ty var_idx tyargs args) = pure $ ANFIR.Expr'MakeADT id ty var_idx tyargs args
convert_almost_expr (AlmostExpr'Lambda id ty param bindings result) = ANFIR.Expr'Lambda id ty param <$> get_dependencies_of_binding_list_and_expr bindings result <*> make_binding_group bindings <*> pure result
convert_almost_expr (AlmostExpr'Param id ty param) = pure $ ANFIR.Expr'Param id ty param
convert_almost_expr (AlmostExpr'Call id ty callee arg) = pure $ ANFIR.Expr'Call id ty callee arg
convert_almost_expr (AlmostExpr'Match id ty tree) = ANFIR.Expr'Match id ty <$> convert_tree tree
    where
        convert_tree (AlmostMatchTree arms) =
            ANFIR.MatchTree
                <$> mapM
                    (\ (clauses, result) ->
                        (case result of
                            Left subtree -> Left <$> convert_tree subtree
                            Right (bindings, res) -> Right <$> ((, res) <$> make_binding_group bindings)) >>= \ result ->
                        pure (clauses, result)
                    )
                    arms

convert_almost_expr (AlmostExpr'TupleDestructure1 id ty tup) = pure $ ANFIR.Expr'TupleDestructure1 id ty tup
convert_almost_expr (AlmostExpr'TupleDestructure2 id ty tup) = pure $ ANFIR.Expr'TupleDestructure2 id ty tup
convert_almost_expr (AlmostExpr'ADTDestructure id ty base field_idx) = pure $ ANFIR.Expr'ADTDestructure id ty base field_idx
convert_almost_expr (AlmostExpr'Forall id ty tys bindings result) = ANFIR.Expr'Forall id ty tys <$> make_binding_group bindings <*> pure result
convert_almost_expr (AlmostExpr'TypeApply id ty e tyarg) = pure $ ANFIR.Expr'TypeApply id ty e tyarg
convert_almost_expr (AlmostExpr'Poison id ty) = pure $ ANFIR.Expr'Poison id ty

get_dependencies_of_binding_list_and_expr :: [ANFIR.BindingKey] -> ANFIR.BindingKey -> Reader (BindingArena AlmostExpr) (Set ANFIR.BindingKey)
get_dependencies_of_binding_list_and_expr bindings e =
    Set.unions <$> mapM get_dependencies_of_almost_expr bindings >>= \ bindings_dependencies ->
    pure ((bindings_dependencies <> [e]) `Set.difference` Set.fromList bindings)

get_dependencies_of_almost_expr :: ANFIR.BindingKey -> Reader (BindingArena AlmostExpr) (Set.Set ANFIR.BindingKey)
get_dependencies_of_almost_expr bk =
    ask >>= \ binding_arena ->
    case Arena.get binding_arena bk of
        AlmostExpr'Refer _ _ i -> pure [i]
        AlmostExpr'Char _ _ _ -> pure []
        AlmostExpr'String _ _ _ -> pure []
        AlmostExpr'Int _ _ _ -> pure []
        AlmostExpr'Float _ _ _ -> pure []
        AlmostExpr'Bool _ _ _ -> pure []
        AlmostExpr'Tuple _ _ a b -> pure [a, b]
        AlmostExpr'Lambda _ _ _ bindings result -> get_dependencies_of_binding_list_and_expr bindings result
        AlmostExpr'Param _ _ _ -> pure []
        AlmostExpr'Call _ _ callee arg -> pure [callee, arg]
        AlmostExpr'Match _ _ tree -> go_through_tree tree
            where
                go_through_tree (AlmostMatchTree arms) =
                    arms
                        & mapM
                            (\ (clauses, result) ->
                                let (referenced_in_clauses, bindings_defined_in_clauses) = clauses
                                        & map go_through_clause
                                        & unzip
                                in

                                (case result of
                                    Left subtree -> go_through_tree subtree
                                    Right (bindings, e) -> get_dependencies_of_binding_list_and_expr bindings e) >>= \ result_dependencies ->
                                pure ((Set.unions referenced_in_clauses <> result_dependencies) `Set.difference` Set.unions bindings_defined_in_clauses)
                            )
                        <&> Set.unions

                -- first element is bindings referenced, second element is bindings defined
                go_through_clause (ANFIR.MatchClause'Match binding _) = ([binding], [])
                go_through_clause (ANFIR.MatchClause'Binding b) = ([], [b])

        AlmostExpr'TupleDestructure1 _ _ tup -> pure [tup]
        AlmostExpr'TupleDestructure2 _ _ tup -> pure [tup]
        AlmostExpr'ADTDestructure _ _ base _ -> pure [base]
        AlmostExpr'Forall _ _ _ bindings e -> get_dependencies_of_binding_list_and_expr bindings e
        AlmostExpr'TypeApply _ _ e _ -> pure [e]
        AlmostExpr'MakeADT _ _ _ _ args -> pure $ Set.fromList args
        AlmostExpr'Poison _ _ -> pure []

make_binding_group :: [ANFIR.BindingKey] -> Reader (BindingArena AlmostExpr) ANFIRBindingGroup
make_binding_group bindings =
    Map.fromList <$> mapM (\ b -> (b,) <$> get_dependencies_of_almost_expr b) bindings >>= \ binding_dependencies ->
    let binding_dependencies_only_here = Map.map (Set.filter (`elem` bindings)) binding_dependencies
    in topological_sort binding_dependencies_only_here [] bindings >>= \ bindings_sorted ->
    pure (ANFIR.BindingGroup bindings_sorted)
    where
        topological_sort _ done [] = pure done
        topological_sort binding_dependencies done left =
            case List.partition dependencies_satisfied left of
                ([], waiting) ->
                    let (loops, not_loop) = find_loops waiting
                    in mapM deal_with_loop loops >>= \ loops ->
                    topological_sort binding_dependencies (done ++ loops) not_loop

                (ready, waiting) -> topological_sort binding_dependencies (done ++ map ANFIR.SingleBinding ready) waiting
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

                deal_with_loop loop =
                    ask >>= \ binding_arena ->
                    if all (allowed_in_loop . Arena.get binding_arena) loop
                        then pure $ ANFIR.MutuallyRecursiveBindings loop
                        else error "illegal loop" -- TODO: proper error message for this
                    where
                        -- TODO: fix this because this allows loops where with only foralls
                        -- eg x = #(A) x#(A) is allowed
                        allowed_in_loop (AlmostExpr'Lambda _ _ _ _ _) = True
                        allowed_in_loop (AlmostExpr'Forall _ _ _ _ _) = True
                        allowed_in_loop _ = False
