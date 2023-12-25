{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module UHF.Parts.ToANFIR (convert) where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.RIR as RIR
import qualified UHF.Util.Arena as Arena
import qualified UHF.Util.IDGen as IDGen

-- TODO: remove these type synonyms
type RIRExpr = RIR.Expr
type RIRBinding = RIR.Binding

-- TODO: remove these type synonyms
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

data Dependency = NeedsExecuted ANFIR.BindingKey | NeedsCallable ANFIR.BindingKey deriving (Eq, Ord)
get_dependency_bk :: Dependency -> ANFIR.BindingKey
-- TODO: rename Executed to Initialized?
get_dependency_bk (NeedsExecuted bk) = bk
get_dependency_bk (NeedsCallable bk) = bk

newtype AlmostBindingGroup = AlmostBindingGroup [ANFIR.BindingKey]
-- the same thing as ANFIR.Expr except lambdas dont have captures and all the binding groups are AlmostBindingGroup
data AlmostExpr
    = AlmostExpr'Refer ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey

    | AlmostExpr'Int ANFIR.ID (Maybe Type.Type) Integer
    | AlmostExpr'Float ANFIR.ID (Maybe Type.Type) Rational
    | AlmostExpr'Bool ANFIR.ID (Maybe Type.Type) Bool
    | AlmostExpr'Char ANFIR.ID (Maybe Type.Type) Char
    | AlmostExpr'String ANFIR.ID (Maybe Type.Type) Text
    | AlmostExpr'Tuple ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey ANFIR.BindingKey
    | AlmostExpr'MakeADT ANFIR.ID (Maybe Type.Type) Type.ADT.VariantIndex [Maybe Type.Type] [ANFIR.BindingKey]

    | AlmostExpr'Lambda ANFIR.ID (Maybe Type.Type) ANFIR.ParamKey AlmostBindingGroup ANFIR.BindingKey
    | AlmostExpr'Param ANFIR.ID (Maybe Type.Type) ANFIR.ParamKey

    | AlmostExpr'Call ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey ANFIR.BindingKey

    | AlmostExpr'Match ANFIR.ID (Maybe Type.Type) AlmostMatchTree

    | AlmostExpr'TupleDestructure1 ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey
    | AlmostExpr'TupleDestructure2 ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey
    | AlmostExpr'ADTDestructure ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey (Maybe Type.ADT.FieldIndex)

    | AlmostExpr'Forall ANFIR.ID (Maybe Type.Type) Type.QuantVarKey AlmostBindingGroup ANFIR.BindingKey
    | AlmostExpr'TypeApply ANFIR.ID (Maybe Type.Type) ANFIR.BindingKey (Maybe Type.Type)

    | AlmostExpr'Poison ANFIR.ID (Maybe Type.Type)

data AlmostMatchTree
    = AlmostMatchTree [([ANFIR.MatchClause], Either AlmostMatchTree (AlmostBindingGroup, ANFIR.BindingKey))]

convert :: RIR.RIR -> ANFIR
convert (RIR.RIR modules adts type_synonyms type_vars variables mod) =
    let (bindings_step_1, params, cu_step_1) = convert_step_1 variables (Arena.get modules mod)
        (bindings_step_2, cu_step_2) = convert_step_2 bindings_step_1 cu_step_1
    in ANFIR.ANFIR adts type_synonyms type_vars bindings_step_2 params cu_step_2

-- step 1: converting from rir to almost anfir {{{1
convert_step_1 :: VariableArena -> RIR.Module -> (BindingArena AlmostExpr, ANFIRParamArena, NeedsTopoSort ANFIR.CU)
convert_step_1 variables mod =
    let ((cu_needs_deps, var_map), (bindings_needs_var_map, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (make_cu mod)) (Arena.new, Arena.new))) variables
        bindings_needs_deps = Arena.transform ($ var_map) bindings_needs_var_map
    in (bindings_needs_deps, params, cu_needs_deps)

make_cu :: RIR.Module -> MakeGraphState (NeedsVarMap AlmostExpr) (NeedsTopoSort ANFIR.CU)
make_cu (RIR.Module _ bindings adts type_synonyms) = concat <$> mapM convert_binding bindings >>= \ bindings -> pure (make_binding_group (AlmostBindingGroup bindings) >>= \ group -> pure (ANFIR.CU group adts type_synonyms))

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

    new_binding (\ _ -> AlmostExpr'Lambda (choose_id m_varid id) ty anfir_param (AlmostBindingGroup body_included_bindings) body)

convert_expr m_varid (RIR.Expr'Let id _ bindings adts type_synonyms result) = do
    var_arena <- lift $ lift $ lift $ lift ask
    let result_ty = RIR.expr_type var_arena result
    bindings <- mapM (lift . convert_binding) bindings
    tell (concat bindings)
    -- TODO: deal with adts and type synonyms properly
    convert_expr Nothing result

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
                            pure (\ _ -> Right (AlmostBindingGroup result_involved_bindings, result))
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

convert_expr m_varid expr@(RIR.Expr'Forall id _ vars e) = go (toList vars) e
    where
        go [] result = convert_expr Nothing e
        go (var:vars) result =
            lift (lift $ lift $ lift ask) >>= \ var_arena ->
            let result_ty = RIR.expr_type var_arena result
            in lift (runWriterT (go vars result)) >>= \ (result, result_bindings) ->
            lift new_expr_id >>= \ eid ->
            new_binding (\ _ -> AlmostExpr'Forall (ANFIR.ExprID eid) (Type.Type'Forall (var:|[]) <$> result_ty) var (AlmostBindingGroup result_bindings) result)
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
convert_almost_expr (AlmostExpr'Lambda id ty param bindings result) = ANFIR.Expr'Lambda id ty param <$> (Set.map get_dependency_bk <$> (get_execute_dependencies_of_almost_binding_group_and_expr bindings result)) <*> make_binding_group bindings <*> pure result
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
convert_almost_expr (AlmostExpr'Forall id ty qvar bindings result) = ANFIR.Expr'Forall id ty qvar <$> make_binding_group bindings <*> pure result
convert_almost_expr (AlmostExpr'TypeApply id ty e tyarg) = pure $ ANFIR.Expr'TypeApply id ty e tyarg
convert_almost_expr (AlmostExpr'Poison id ty) = pure $ ANFIR.Expr'Poison id ty

get_execute_dependencies_of_almost_binding_group_and_expr :: AlmostBindingGroup -> ANFIR.BindingKey -> Reader (BindingArena AlmostExpr) (Set Dependency)
get_execute_dependencies_of_almost_binding_group_and_expr (AlmostBindingGroup bindings) e = do
    binding_dependencies <- Set.unions <$> mapM get_execute_dependencies_of_almost_expr bindings
    pure $
        Set.filter
            -- exclude all dependencies that are defined in the binding group because those are not dependencies of the binding group and expr as a whole
            (\dep -> not $ Set.member (get_dependency_bk dep) (Set.fromList bindings))
            (binding_dependencies <> [NeedsExecuted e])

get_execute_dependencies_of_almost_expr :: ANFIR.BindingKey -> Reader (BindingArena AlmostExpr) (Set.Set Dependency)
get_execute_dependencies_of_almost_expr bk =
    ask >>= \ binding_arena ->
    case Arena.get binding_arena bk of
        AlmostExpr'Refer _ _ i -> pure [NeedsExecuted i]
        AlmostExpr'Char _ _ _ -> pure []
        AlmostExpr'String _ _ _ -> pure []
        AlmostExpr'Int _ _ _ -> pure []
        AlmostExpr'Float _ _ _ -> pure []
        AlmostExpr'Bool _ _ _ -> pure []
        AlmostExpr'Tuple _ _ a b -> pure [NeedsExecuted a, NeedsExecuted b]
        AlmostExpr'Lambda _ _ _ bindings result ->
            -- 'get_execute_dependencies_of_almost_binding_group_and_expr bindings result' returns the dependencies needed to execute the body of the lambda
            -- but to execute the lambda (not execute its body, just define the body), we dont have any dependencies
            -- we dont require that the captures have been executed because that prevents recursive functions from working (because recursive functions capture themselves)
            pure []
        AlmostExpr'Param _ _ _ -> pure []
        AlmostExpr'Call _ _ callee arg -> pure [NeedsCallable callee, NeedsExecuted arg]
        AlmostExpr'Match _ _ tree -> go_through_tree tree
            where
                go_through_tree (AlmostMatchTree arms) =
                    arms
                        & mapM
                            (\ (clauses, result) ->
                                let (referenced_in_clauses, defined_in_clauses) = clauses
                                        & map go_through_clause
                                        & unzip
                                    remove_defined_in_clauses = Set.filter (\ dep -> not $ Set.member (get_dependency_bk dep) (Set.unions defined_in_clauses))
                                in

                                (case result of
                                    Left subtree -> go_through_tree subtree
                                    Right (bindings, e) -> get_execute_dependencies_of_almost_binding_group_and_expr bindings e) >>= \ result_dependencies ->

                                pure (remove_defined_in_clauses $ Set.unions referenced_in_clauses <> result_dependencies)
                            )
                        <&> Set.unions

                -- first element is execute dependencies, second element is bindings defined
                go_through_clause (ANFIR.MatchClause'Match binding _) = ([NeedsExecuted binding], [])
                go_through_clause (ANFIR.MatchClause'Binding b) = ([], [b])

        AlmostExpr'TupleDestructure1 _ _ tup -> pure [NeedsExecuted tup]
        AlmostExpr'TupleDestructure2 _ _ tup -> pure [NeedsExecuted tup]
        AlmostExpr'ADTDestructure _ _ base _ -> pure [NeedsExecuted base]
        AlmostExpr'Forall _ _ _ bindings e -> get_execute_dependencies_of_almost_binding_group_and_expr bindings e
        AlmostExpr'TypeApply _ _ e _ -> pure [NeedsExecuted e]
        AlmostExpr'MakeADT _ _ _ _ args -> pure $ Set.fromList $ map NeedsExecuted args
        AlmostExpr'Poison _ _ -> pure []

get_call_dependencies_of_almost_expr :: ANFIR.BindingKey -> Reader (BindingArena AlmostExpr) (Set.Set Dependency)
get_call_dependencies_of_almost_expr bk =
    ask >>= \ binding_arena ->
    case Arena.get binding_arena bk of
        -- these expressions are all atomic, so they can be called anywhere that they have been executed (i.e. their call dependencies are the same as their execute dependencies)
        AlmostExpr'Char _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'String _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'Int _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'Float _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'Bool _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'Param _ _ _ -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'Poison _ _ -> get_execute_dependencies_of_almost_expr bk

        -- these expressions dont store code themselves, but do refer to store other expressions, so they are only callable if the other ones are also callable
        AlmostExpr'Refer _ _ r -> pure [NeedsCallable r]
        AlmostExpr'Tuple _ _ a b -> pure [NeedsCallable a, NeedsCallable b]
        AlmostExpr'Call _ _ callee arg -> pure [NeedsCallable callee, NeedsCallable arg]
        AlmostExpr'Match _ _ tree -> get_execute_dependencies_of_almost_expr bk
        AlmostExpr'TupleDestructure1 _ _ tup -> pure [NeedsCallable tup]
        AlmostExpr'TupleDestructure2 _ _ tup -> pure [NeedsCallable tup]
        AlmostExpr'ADTDestructure _ _ v _ -> pure [NeedsCallable v]
        AlmostExpr'Forall _ _ _ bindings e -> pure [NeedsCallable e] -- TODO: reconsider if this is correct
        AlmostExpr'TypeApply _ _ e _ -> pure [NeedsCallable e]
        AlmostExpr'MakeADT _ _ _ _ args -> pure $ Set.fromList $ map NeedsCallable args

        -- lambdas store code
        AlmostExpr'Lambda _ _ _ bindings result ->
            -- because calling a lambda executes bindings and result, the call dependencies of this lambda are the execute dependencies of bindings and result unmodified
            get_execute_dependencies_of_almost_binding_group_and_expr bindings result

make_binding_group :: AlmostBindingGroup -> Reader (BindingArena AlmostExpr) ANFIRBindingGroup
make_binding_group (AlmostBindingGroup bindings) =
    Map.fromList <$> mapM (\ b -> (b,) <$> get_execute_dependencies_of_almost_expr b) bindings >>= \ exec_dependencies ->
    Map.fromList <$> mapM (\ b -> (b,) <$> get_call_dependencies_of_almost_expr b) bindings >>= \ call_dependencies ->
    let exec_dependencies_only_here = Map.map (Set.filter ((`elem` bindings) . get_dependency_bk)) exec_dependencies
        call_dependencies_only_here = Map.map (Set.filter ((`elem` bindings) . get_dependency_bk)) call_dependencies

    in topological_sort call_dependencies_only_here exec_dependencies_only_here [] bindings >>= \ bindings_sorted ->
    pure (ANFIR.BindingGroup bindings_sorted)
    where
        topological_sort _ _ done [] = pure done
        topological_sort call_dependencies exec_dependencies done left =
            case List.partition exec_dependencies_satisfied left of
                ([], waiting) ->
                    let (loops, not_loop) = find_loops waiting
                    in mapM deal_with_loop loops >>= \ loops ->
                    topological_sort call_dependencies exec_dependencies (done ++ loops) not_loop

                (ready, waiting) -> topological_sort call_dependencies exec_dependencies (done ++ map ANFIR.SingleBinding ready) waiting
            where
                exec_dependencies_satisfied bk = and $ Set.map dependency_satisfied (exec_dependencies Map.! bk)
                dependency_satisfied (NeedsExecuted dep) = dep `List.elem` concatMap ANFIR.chunk_bindings done -- dep needs to have been executed, so check if it is in done
                dependency_satisfied (NeedsCallable dep) = and $ Set.map dependency_satisfied (call_dependencies Map.! dep) -- dep needs to be callable, so make sure that all of its call dependencies are satisfied

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
                                            let current_dependencies = exec_dependencies Map.! current
                                            in headMay $
                                                mapMaybe
                                                    (\ neighbor -> trace_single_loop (current:visited_stack) neighbor (filter (/=neighbor) unvisited))
                                                    (toList $ Set.filter (`elem` searching_for_loops_in) $ Set.map get_dependency_bk current_dependencies) -- TODO: figure out a better error for this

                deal_with_loop loop =
                    ask >>= \ binding_arena ->
                    if all (allowed_in_loop . Arena.get binding_arena) loop
                        then error "aa" -- pure $ ANFIR.MutuallyRecursiveBindings loop
                        else error "illegal loop" -- TODO: proper error message for this
                    where
                        -- TODO: fix this because this allows loops where with only foralls
                        -- eg x = #(A) x#(A) is allowed
                        allowed_in_loop (AlmostExpr'Lambda _ _ _ _ _) = True
                        allowed_in_loop (AlmostExpr'Forall _ _ _ _ _) = True
                        allowed_in_loop _ = False
