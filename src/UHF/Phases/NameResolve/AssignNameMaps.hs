module UHF.Phases.NameResolve.AssignNameMaps
    ( assign
    ) where

import UHF.Prelude

import UHF.Phases.NameResolve.ExtractIdentifiers (Extracted)
import UHF.Phases.NameResolve.Keys
import UHF.Phases.NameResolve.NameMaps
import UHF.Source.Located (Located (Located))
import qualified Data.Map as Map
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Phases.NameResolve.DeclAt as DeclAt
import qualified UHF.Phases.NameResolve.Error as Error
import qualified UHF.Phases.NameResolve.NRReader as NRReader
import qualified UHF.Phases.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

-- TODO: remove these type aliases

type SIR = SIR.SIR Extracted
type Module = SIR.Module Extracted
type ADT = Type.ADT (TypeExpr, ())
type TypeSynonym = Type.TypeSynonym (TypeExpr, ())
type TypeExpr = SIR.TypeExpr Extracted

type ModuleArena = Arena.Arena Module SIR.ModuleKey
type ADTArena = Arena.Arena ADT Type.ADTKey
type TypeSynonymArena = Arena.Arena TypeSynonym Type.TypeSynonymKey
type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey
type VariableArena = Arena.Arena (SIR.Variable Extracted) SIR.VariableKey

type DIdenMNMArena = Arena.Arena (Maybe NameMapStack) DIdenStartKey
type VIdenMNMArena = Arena.Arena (Maybe NameMapStack) VIdenStartKey
type PIdenMNMArena = Arena.Arena (Maybe NameMapStack) PIdenStartKey
type DIdenNMArena = Arena.Arena NameMapStack DIdenStartKey
type VIdenNMArena = Arena.Arena NameMapStack VIdenStartKey
type PIdenNMArena = Arena.Arena NameMapStack PIdenStartKey

-- monad {{{
type AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps = StateT (DIdenMNMArena, VIdenMNMArena, PIdenMNMArena) (NRReader.NRReader adt_arena variable_arena quant_var_arena sir_child_maps Error.WithErrors)

assign_for_d_iden :: DIdenStartKey -> NameMapStack -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
assign_for_v_iden :: VIdenStartKey -> NameMapStack -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
assign_for_p_iden :: PIdenStartKey -> NameMapStack -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
(assign_for_d_iden, assign_for_v_iden, assign_for_p_iden) =
    ( \ k nms -> state $ \ (d_arena, v_arena, p_arena) ->
        let d_arena' = Arena.modify d_arena k (assign nms)
        in ((), (d_arena', v_arena, p_arena))
    , \ k nms -> state $ \ (d_arena, v_arena, p_arena) ->
        let v_arena' = Arena.modify v_arena k (assign nms)
        in ((), (d_arena, v_arena', p_arena))
    , \ k nms -> state $ \ (d_arena, v_arena, p_arena) ->
        let p_arena' = Arena.modify p_arena k (assign nms)
        in ((), (d_arena, v_arena, p_arena'))
    )
    where
        assign nms Nothing = Just nms
        assign _ (Just _) = error "assigning multiple name maps to one identifier"
-- }}}

assign :: Arena.Arena () DIdenStartKey -> Arena.Arena () VIdenStartKey -> Arena.Arena () PIdenStartKey -> SIRChildMaps -> SIR -> Error.WithErrors (DIdenNMArena, VIdenNMArena, PIdenNMArena)
assign d_iden_arena v_iden_arena p_iden_arena sir_child_maps (SIR.SIR mods adts type_synonyms quant_vars variables _) = do
    -- TODO: put name maps into arena
    ((), (d_iden_nm_arena, v_iden_nm_arena, p_iden_nm_arena)) <-
        runReaderT (
            runStateT
                ( do
                    (adt_parents, type_synonym_parents) <- assign_for_mods mods
                    assign_for_adts adt_parents adts
                    assign_for_type_synonyms type_synonym_parents type_synonyms
                )
                (Arena.transform (const Nothing) d_iden_arena, Arena.transform (const Nothing) v_iden_arena, Arena.transform (const Nothing) p_iden_arena)
        ) (adts, variables, quant_vars, sir_child_maps)
    pure (convert_nm_arena d_iden_nm_arena, convert_nm_arena v_iden_nm_arena, convert_nm_arena p_iden_nm_arena)
    where
        convert_nm_arena :: Arena.Key k => Arena.Arena (Maybe b) k -> Arena.Arena b k
        convert_nm_arena = Arena.transform convert
            where
                convert (Just nm) = nm
                convert Nothing = error "identifier was not assigned name map"

assign_for_mods :: ModuleArena -> AssignMonad ADTArena VariableArena QuantVarArena SIRChildMaps (Map.Map Type.ADTKey NameMaps.NameMapStack, Map.Map Type.TypeSynonymKey NameMaps.NameMapStack)
assign_for_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM assign_for_module module_arena) >>= \ ((_, adt_parents), type_synonym_parents) ->
    pure (adt_parents, type_synonym_parents)

assign_for_adts :: Map.Map Type.ADTKey NameMaps.NameMapStack -> ADTArena -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_adts adt_parent_name_maps adt_arena = Arena.transform_with_keyM (assign_for_adt adt_parent_name_maps) adt_arena >> pure ()

assign_for_type_synonyms :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> TypeSynonymArena -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_type_synonyms synonym_parent_name_maps type_synonym_arena = Arena.transform_with_keyM (assign_for_type_synonym synonym_parent_name_maps) type_synonym_arena >> pure ()

assign_for_module :: SIR.ModuleKey -> SIR.Module Extracted -> WriterT (Map Type.ADTKey NameMaps.NameMapStack) (WriterT (Map Type.TypeSynonymKey NameMaps.NameMapStack) (AssignMonad ADTArena VariableArena QuantVarArena SIRChildMaps)) ()
assign_for_module mod_key (SIR.Module _ bindings adts type_synonyms) =
    lift (lift $ lift NRReader.ask_sir_child_maps) >>= \ sir_child_maps ->
    let cur_map = NameMaps.get_module_child_maps sir_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) type_synonyms >>
    mapM_ (lift . lift . assign_for_binding (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) bindings

assign_for_adt :: Map.Map Type.ADTKey NameMaps.NameMapStack -> Type.ADTKey -> ADT -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_adt adt_parent_name_maps adt_key (Type.ADT _ _ type_vars variants) =
    let parent = adt_parent_name_maps Map.! adt_key
    in
    mapM
        (\ var ->
            lift (NRReader.ask_quant_var_arena) >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        type_vars >>= \ type_vars' ->
    lift (lift $ NameMaps.make_name_maps type_vars' [] []) >>= \ new_nc ->
    mapM_ (assign_for_variant (NameMaps.NameMapStack new_nc (Just parent))) variants
    where
        assign_for_variant nc_stack (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> assign_for_type_expr nc_stack ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        assign_for_variant nc_stack (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> assign_for_type_expr nc_stack ty >>= \ ty -> pure (id, (ty, ()))) fields

assign_for_type_synonym :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> Type.TypeSynonymKey -> TypeSynonym -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_type_synonym parent_maps synonym_key (Type.TypeSynonym _ _ (expansion, ())) =
    let parent = parent_maps Map.! synonym_key
    in assign_for_type_expr parent expansion

assign_for_binding :: NameMaps.NameMapStack -> SIR.Binding Extracted -> AssignMonad ADTArena VariableArena QuantVarArena sir_child_maps ()
assign_for_binding nc_stack (SIR.Binding target _ expr) = assign_for_pat nc_stack target >> assign_for_expr nc_stack expr
assign_for_binding _ (SIR.Binding'ADTVariant _ _ _ _) = pure ()

assign_for_type_expr :: NameMaps.NameMapStack -> SIR.TypeExpr Extracted -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_type_expr nc_stack (SIR.TypeExpr'Refer _ _ id) = assign_type_iden nc_stack id
assign_for_type_expr nc_stack (SIR.TypeExpr'Get _ _ parent _) = assign_for_type_expr nc_stack parent
assign_for_type_expr nc_stack (SIR.TypeExpr'Tuple _ _ a b) = assign_for_type_expr nc_stack a >> assign_for_type_expr nc_stack b
assign_for_type_expr _ (SIR.TypeExpr'Hole _ _ _ _) = pure ()
assign_for_type_expr nc_stack (SIR.TypeExpr'Function _ _ arg res) = assign_for_type_expr nc_stack arg >> assign_for_type_expr nc_stack res
assign_for_type_expr nc_stack (SIR.TypeExpr'Forall _ _ vars ty) =
    mapM
        (\ var ->
            lift (NRReader.ask_quant_var_arena) >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (lift $ NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    assign_for_type_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) ty
assign_for_type_expr nc_stack (SIR.TypeExpr'Apply _ _ ty args) = assign_for_type_expr nc_stack ty >> assign_for_type_expr nc_stack args
assign_for_type_expr _ (SIR.TypeExpr'Wild _ _) = pure ()
assign_for_type_expr _ (SIR.TypeExpr'Poison _ _) = pure ()

assign_for_pat :: NameMaps.NameMapStack -> SIR.Pattern Extracted -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_for_pat _ (SIR.Pattern'Identifier _ _ _) = pure ()
assign_for_pat _ (SIR.Pattern'Wildcard _ _) = pure ()
assign_for_pat nc_stack (SIR.Pattern'Tuple _ _ a b) = assign_for_pat nc_stack a >> assign_for_pat nc_stack b
assign_for_pat nc_stack (SIR.Pattern'Named _ _ _ _ subpat) = assign_for_pat nc_stack subpat
assign_for_pat nc_stack (SIR.Pattern'AnonADTVariant _ _ variant_iden_split _ _ subpat) = do
    assign_split_iden assign_pat_iden nc_stack variant_iden_split
    mapM_ (assign_for_pat nc_stack) subpat
assign_for_pat nc_stack (SIR.Pattern'NamedADTVariant _ _ variant_iden_split _ _ subpat) = do
    assign_split_iden assign_pat_iden nc_stack variant_iden_split
    mapM_ (\ (_, field_pat) -> assign_for_pat nc_stack field_pat) subpat
assign_for_pat _ (SIR.Pattern'Poison _ _) = pure ()

assign_for_expr :: NameMaps.NameMapStack -> SIR.Expr Extracted -> AssignMonad ADTArena VariableArena QuantVarArena sir_child_maps ()
assign_for_expr nc_stack (SIR.Expr'Identifier _ _ _ iden_split _) = assign_split_iden assign_expr_iden nc_stack iden_split
assign_for_expr _ (SIR.Expr'Char _ _ _ _) = pure $ ()
assign_for_expr _ (SIR.Expr'String _ _ _ _) = pure ()
assign_for_expr _ (SIR.Expr'Int _ _ _ _) = pure ()
assign_for_expr _ (SIR.Expr'Float _ _ _ _) = pure ()
assign_for_expr _ (SIR.Expr'Bool _ _ _ _) = pure ()

assign_for_expr nc_stack (SIR.Expr'Tuple _ _ _ a b) = do
    assign_for_expr nc_stack a
    assign_for_expr nc_stack b

assign_for_expr nc_stack (SIR.Expr'Lambda _ _ _ param body) = do
    assign_for_pat nc_stack param
    param_vars <- lift $ NameMaps.pattern_vars param
    new_nc <- lift $ lift $ NameMaps.make_name_maps [] param_vars []
    assign_for_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) body

assign_for_expr nc_stack (SIR.Expr'Let _ _ _ bindings body) = do
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    new_nm <- lift $ NameMaps.make_name_maps_from_decls todo bindings [] []
    mapM_ (assign_for_binding nc_stack) bindings
    assign_for_expr (NameMaps.NameMapStack new_nm (Just nc_stack)) body

assign_for_expr nc_stack (SIR.Expr'LetRec _ _ _ bindings body) = do
    new_nm <- lift $ NameMaps.make_name_maps_from_decls todo bindings [] []
    let new_nc_stack = NameMaps.NameMapStack new_nm (Just nc_stack)
    mapM_ (assign_for_binding new_nc_stack) bindings
    assign_for_expr new_nc_stack body

assign_for_expr nc_stack (SIR.Expr'BinaryOps _ _ _ _ first ops) = do
    assign_for_expr nc_stack first
    mapM_
        (\ (_, iden, _, rhs) -> do
            assign_split_iden assign_expr_iden nc_stack iden
            assign_for_expr nc_stack rhs
        )
        ops

assign_for_expr nc_stack (SIR.Expr'Call _ _ _ callee arg) = do
    assign_for_expr nc_stack callee
    assign_for_expr nc_stack arg

assign_for_expr nc_stack (SIR.Expr'If _ _ _ _ cond t f) = do
    assign_for_expr nc_stack cond
    assign_for_expr nc_stack t
    assign_for_expr nc_stack f
assign_for_expr nc_stack (SIR.Expr'Match _ _ _ _ e arms) = do
    assign_for_expr nc_stack e
    mapM_
            (\ (pat, expr) ->
                assign_for_pat nc_stack pat >>
                lift (NameMaps.pattern_vars pat) >>= \ var_children ->
                lift (lift $ NameMaps.make_name_maps [] var_children []) >>= \ arm_nc ->
                assign_for_expr (NameMaps.NameMapStack arm_nc (Just nc_stack)) expr
            )
            arms

assign_for_expr nc_stack (SIR.Expr'TypeAnnotation _ _ _ (ty, _) e) =
    assign_for_type_expr nc_stack ty >>
    assign_for_expr nc_stack e >>
    pure ()

assign_for_expr nc_stack (SIR.Expr'Forall _ _ _ vars e) =
    mapM
        (\ var ->
            lift (NRReader.ask_quant_var_arena) >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (lift $ NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    assign_for_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) e >>
    pure ()
assign_for_expr nc_stack (SIR.Expr'TypeApply _ _ _ e (arg, _)) = do
    assign_for_expr nc_stack e
    assign_for_type_expr nc_stack arg
    pure ()

assign_for_expr _ (SIR.Expr'Hole _ _ _ _) = pure ()

assign_for_expr _ (SIR.Expr'Poison _ _ _) = pure ()

-- TODO: figure this out
assign_split_iden :: (NameMaps.NameMapStack -> key -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()) -> NameMaps.NameMapStack -> SIR.SplitIdentifier Extracted key -> AssignMonad adt_arena variable_arena QuantVarArena sir_child_maps ()
assign_split_iden _ name_map_stack (SIR.SplitIdentifier'Get texpr _) = assign_for_type_expr name_map_stack texpr
assign_split_iden assign name_map_stack (SIR.SplitIdentifier'Single i) = assign name_map_stack i

-- TODO: remove these
assign_type_iden :: NameMaps.NameMapStack -> DIdenStartKey -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
assign_type_iden = flip assign_for_d_iden

assign_expr_iden :: NameMaps.NameMapStack -> VIdenStartKey -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
assign_expr_iden = flip assign_for_v_iden

assign_pat_iden :: NameMaps.NameMapStack -> PIdenStartKey -> AssignMonad adt_arena variable_arena quant_var_arena sir_child_maps ()
assign_pat_iden = flip assign_for_p_iden
