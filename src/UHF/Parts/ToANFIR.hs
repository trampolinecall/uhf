{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module UHF.Parts.ToANFIR (convert) where

import UHF.Prelude

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.RIR as RIR
import qualified UHF.Util.Arena as Arena
import qualified UHF.Util.IDGen as IDGen

type VariableArena = Arena.Arena RIR.Variable RIR.VariableKey

type BindingArena b = Arena.Arena b ANFIR.BindingKey
-- TODO: remove this
type ParamArena = Arena.Arena ANFIR.Param ANFIR.ParamKey

-- TODO: turn this into a monad transformer like ReaderT?
type NeedsVarMap e = VariableMap -> e

type VariableMap = Map.Map RIR.VariableKey ANFIR.BindingKey

type MakeGraphState binding = WriterT VariableMap (StateT (BindingArena binding, ParamArena) (IDGen.IDGenT ID.ExprID (Reader VariableArena)))

newtype AccumBindingGroup = AccumBindingGroup { un_accum_bg :: ANFIR.BindingGroup }
instance Semigroup AccumBindingGroup where
    AccumBindingGroup (ANFIR.BindingGroup ANFIR.TopologicallySorted bindings1) <> AccumBindingGroup (ANFIR.BindingGroup ANFIR.TopologicallySorted bindings2) = AccumBindingGroup (ANFIR.BindingGroup ANFIR.TopologicallySorted (bindings1 <> bindings2))
    AccumBindingGroup (ANFIR.BindingGroup _ bindings1) <> AccumBindingGroup (ANFIR.BindingGroup _ bindings2) = AccumBindingGroup (ANFIR.BindingGroup ANFIR.HasLoops (bindings1 <> bindings2))
instance Monoid AccumBindingGroup where
    mempty = AccumBindingGroup $ ANFIR.BindingGroup ANFIR.TopologicallySorted []

convert :: RIR.RIR -> ANFIR.ANFIR
convert (RIR.RIR adts type_synonyms type_vars variables cu) =
    let ((cu', var_map), (bindings_needs_var_map, params)) = runReader (IDGen.run_id_gen_t ID.ExprID'ANFIRGen (runStateT (runWriterT (make_cu cu)) (Arena.new, Arena.new))) variables
        cu'' = cu' var_map
        bindings = Arena.transform (ANFIR.Binding . ($ var_map)) bindings_needs_var_map
    in ANFIR.ANFIR adts type_synonyms type_vars bindings params cu''

make_cu :: RIR.CU -> MakeGraphState (NeedsVarMap ANFIR.Expr) (NeedsVarMap ANFIR.CU)
make_cu (RIR.CU bindings adts type_synonyms main_function) = convert_bindings bindings >>= \ group -> pure (\ var_map -> ANFIR.CU ((var_map Map.!) <$> main_function) group adts type_synonyms)

map_variable :: RIR.VariableKey -> ANFIR.BindingKey -> MakeGraphState binding ()
map_variable k binding = tell $ Map.singleton k binding

get_var :: RIR.VariableKey -> MakeGraphState binding RIR.Variable
get_var k = lift $ lift $ lift $ reader (\ a -> Arena.get a k)

convert_bindings :: RIR.Bindings -> MakeGraphState (NeedsVarMap ANFIR.Expr) ANFIR.BindingGroup
convert_bindings (RIR.Bindings topological_sort_status bindings) = do
    -- the resulting accum_status is based on whether or not the binding groups within bindings have unsorted binding groups
    -- but if the source rir binding group was not topologically sorted then it doesnt matter whether or not the binding groups within it were and this resulting binding group should not be marked as sorted anwyays
    (AccumBindingGroup (ANFIR.BindingGroup accum_status accum_bindings)) <- execWriterT $ mapM convert_binding bindings
    pure (ANFIR.BindingGroup (convert_topological_sort_status accum_status topological_sort_status) accum_bindings)
    where
        convert_topological_sort_status ANFIR.TopologicallySorted RIR.TopologicallySorted = ANFIR.TopologicallySorted
        convert_topological_sort_status _ _ = ANFIR.HasLoops

convert_binding :: RIR.Binding -> WriterT AccumBindingGroup (MakeGraphState (NeedsVarMap ANFIR.Expr)) ANFIR.BindingKey
convert_binding (RIR.Binding target expr) =
    lift (get_var target) >>= \ (RIR.Variable varid _ _) ->
    convert_expr (Just varid) expr >>= \ expr_result_binding ->
    lift (map_variable target expr_result_binding) >>
    pure expr_result_binding

new_binding :: binding -> WriterT AccumBindingGroup (MakeGraphState binding) ANFIR.BindingKey
new_binding binding = lift (lift $ state $ \ (bindings, params) -> let (i, bindings') = Arena.put binding bindings in (i, (bindings', params))) >>= \ binding_key -> tell (AccumBindingGroup $ ANFIR.BindingGroup ANFIR.TopologicallySorted [binding_key]) >> pure binding_key
new_param :: ANFIR.Param -> WriterT AccumBindingGroup (MakeGraphState binding) ANFIR.ParamKey
new_param param = lift (lift $ state $ \ (bindings, params) -> let (i, params') = Arena.put param params in (i, (bindings, params')))

new_expr_id :: MakeGraphState binding ID.ExprID
new_expr_id = lift $ lift IDGen.gen_id

choose_id :: Maybe ID.VariableID -> ID.ExprID -> ANFIR.ID
choose_id (Just varid) _ = ANFIR.VarID varid
choose_id Nothing eid = ANFIR.ExprID eid

convert_expr :: Maybe ID.VariableID -> RIR.Expr -> WriterT AccumBindingGroup (MakeGraphState (NeedsVarMap ANFIR.Expr)) ANFIR.BindingKey
convert_expr m_varid expr@(RIR.Expr'Refer id _ _ varkey) =
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    case varkey of
        Just varkey -> new_binding $ \ var_map -> ANFIR.Expr'Refer (choose_id m_varid id) ty (var_map Map.! varkey)
        Nothing -> new_binding $ \ _ -> ANFIR.Expr'Poison (choose_id m_varid id) ty
convert_expr m_varid expr@(RIR.Expr'Intrinsic id _ _ i) =
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    new_binding $ \ var_map -> ANFIR.Expr'Intrinsic (choose_id m_varid id) ty i
convert_expr m_varid expr@(RIR.Expr'Char id _ c) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> ANFIR.Expr'Char (choose_id m_varid id) ty c)

convert_expr m_varid expr@(RIR.Expr'String id _ s) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> ANFIR.Expr'String (choose_id m_varid id) ty s)
convert_expr m_varid expr@(RIR.Expr'Int id _ i) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> ANFIR.Expr'Int (choose_id m_varid id) ty i)
convert_expr m_varid expr@(RIR.Expr'Float id _ f) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> ANFIR.Expr'Float (choose_id m_varid id) ty f)
convert_expr m_varid expr@(RIR.Expr'Bool id _ b) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in new_binding (\ _ -> ANFIR.Expr'Bool (choose_id m_varid id) ty b)

convert_expr m_varid expr@(RIR.Expr'Tuple id _ a b) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in convert_expr Nothing a >>= \ a -> convert_expr Nothing b >>= \ b -> new_binding (\ _ -> ANFIR.Expr'Tuple (choose_id m_varid id) ty a b)

convert_expr m_varid expr@(RIR.Expr'Lambda id _ param_var captures body) = -- TODO: use captures
    lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in
    lift (get_var param_var) >>= \ (RIR.Variable param_id param_ty _) ->
    new_param (ANFIR.Param param_id param_ty) >>= \ anfir_param ->
    lift (runWriterT $ -- lambda bodies should not be included in the parent included bindings because they do not need to be evaluated to create the lambda object
        lift new_expr_id >>= \ param_binding_id ->
        new_binding (\ _ -> ANFIR.Expr'Param (ANFIR.ExprID param_binding_id) param_ty anfir_param) >>= \ param_binding ->
        lift (map_variable param_var param_binding) >>
        convert_expr Nothing body
    ) >>= \ (body, body_included_bindings) ->

    new_binding (\ var_map -> ANFIR.Expr'Lambda (choose_id m_varid id) ty anfir_param (Set.map (var_map Map.!) captures) (un_accum_bg body_included_bindings) body)

convert_expr m_varid (RIR.Expr'Let id _ bindings adts type_synonyms result) = do
    var_arena <- lift $ lift $ lift $ lift ask
    let result_ty = RIR.expr_type var_arena result
    bindings <- lift $ convert_bindings bindings
    tell (AccumBindingGroup bindings)
    -- TODO: deal with adts and type synonyms properly
    convert_expr Nothing result

convert_expr m_varid expr@(RIR.Expr'Call id _ callee arg) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in convert_expr Nothing callee >>= \ callee -> convert_expr Nothing arg >>= \ arg -> new_binding (\ _ -> ANFIR.Expr'Call (choose_id m_varid id) ty callee arg)

convert_expr m_varid (RIR.Expr'Match id ty _ tree) =
    lift (convert_tree tree) >>= \ tree ->

    new_binding (\ var_map -> ANFIR.Expr'Match (choose_id m_varid id) ty (tree var_map))
    where
        convert_tree (RIR.MatchTree arms) =
            mapM
                (\ (clauses, result) ->
                    mapM convert_clause clauses >>= \ clauses ->
                    (case result of
                        Right e ->
                            runWriterT (convert_expr Nothing e) >>= \ (result, result_involved_bindings) ->
                            pure (\ _ -> Right (un_accum_bg result_involved_bindings, result))
                        Left subtree ->
                            convert_tree subtree >>= \ subtree ->
                            pure (\ var_map -> Left (subtree var_map))) >>= \ result ->
                    pure (\ var_map -> (concatMap ($ var_map) clauses, result var_map)))
                arms >>= \ arms ->
            pure (\ var_map -> ANFIR.MatchTree (map ($ var_map) arms))

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
            runWriterT (new_binding (\ var_map -> ANFIR.Expr'Refer (ANFIR.ExprID id) other_ty (var_map Map.! other))) >>= \ (binding, _) ->
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'TupleDestructure1 ty tup) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> ANFIR.Expr'TupleDestructure1 (ANFIR.ExprID id) ty (var_map Map.! tup))) >>= \ (binding, _) -> -- same note about all the bindings made as above
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'TupleDestructure2 ty tup) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> ANFIR.Expr'TupleDestructure2 (ANFIR.ExprID id) ty (var_map Map.! tup))) >>= \ (binding, _) -> -- same note as above
            pure binding
        convert_assign_rhs (RIR.MatchAssignRHS'AnonADTVariantField ty base field_idx) =
            new_expr_id >>= \ id ->
            runWriterT (new_binding (\ var_map -> ANFIR.Expr'ADTDestructure (ANFIR.ExprID id) ty (var_map Map.! base) field_idx)) >>= \ (binding, _) -> -- same note as above
            pure binding

convert_expr m_varid expr@(RIR.Expr'Forall id _ vars e) = go (toList vars) e
    where
        go [] result = convert_expr Nothing result
        go (var:vars) result =
            lift (lift $ lift $ lift ask) >>= \ var_arena ->
            let result_ty = RIR.expr_type var_arena result
            in lift (runWriterT (go vars result)) >>= \ (result, result_bindings) ->
            lift new_expr_id >>= \ eid ->
            new_binding (\ _ -> ANFIR.Expr'Forall (ANFIR.ExprID eid) (Type.Type'Forall (var:|[]) <$> result_ty) var (un_accum_bg result_bindings) result)
convert_expr m_varid (RIR.Expr'TypeApply id ty _ e arg) = convert_expr Nothing e >>= \ e -> new_binding (\ _ -> ANFIR.Expr'TypeApply (choose_id m_varid id) ty e arg)

convert_expr m_varid expr@(RIR.Expr'MakeADT id _ variant tyargs args) = lift (lift $ lift $ lift ask) >>= \ var_arena -> let ty = RIR.expr_type var_arena expr in mapM (convert_expr Nothing) args >>= \ args -> new_binding (\ _ -> ANFIR.Expr'MakeADT (choose_id m_varid id) ty variant tyargs args)

convert_expr m_varid (RIR.Expr'Poison id ty _) = new_binding (\ _ -> ANFIR.Expr'Poison (choose_id m_varid id) ty)
