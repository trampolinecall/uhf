module UHF.Parts.NameResolve.AssignNameMaps
    ( Unassigned
    , Assigned
    , assign
    ) where

import UHF.Prelude

import UHF.Source.Located (Located (Located))
import qualified Data.Map as Map
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Parts.NameResolve.DeclAt as DeclAt
import qualified UHF.Parts.NameResolve.Error as Error
import qualified UHF.Parts.NameResolve.NRReader as NRReader
import qualified UHF.Parts.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents
-- TODO: put name maps into arena

type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type IdenStart = Located Text
type AssignedIdenStart = (NameMaps.NameMapStack, IdenStart)

type Unassigned = (IdenStart, (), (), IdenStart, (), IdenStart, (), (), ())

type UnassignedModuleArena = Arena.Arena (SIR.Module Unassigned) SIR.ModuleKey
type UnassignedADTArena = Arena.Arena (SIR.ADT Unassigned) Type.ADTKey
type UnassignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Unassigned) Type.TypeSynonymKey
type UnassignedVariableArena = Arena.Arena (SIR.Variable Unassigned) SIR.VariableKey

type Assigned = (AssignedIdenStart, (), (), AssignedIdenStart, (), AssignedIdenStart, (), (), ())

type AssignedModuleArena = Arena.Arena (SIR.Module Assigned) SIR.ModuleKey
type AssignedADTArena = Arena.Arena (SIR.ADT Assigned) Type.ADTKey
type AssignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Assigned) Type.TypeSynonymKey

-- assign entry point {{{1
assign :: NameMaps.SIRChildMaps -> SIR.SIR Unassigned -> Error.WithErrors (SIR.SIR Assigned)
assign sir_child_maps (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    runReaderT (assign_in_mods mods) (adts, variables, type_vars, sir_child_maps) >>= \ (mods, adt_parents, type_synonym_parents) ->
    runReaderT (assign_in_adts adt_parents adts) ((), (), type_vars, sir_child_maps) >>= \ adts ->
    runReaderT (assign_in_type_synonyms type_synonym_parents type_synonyms) ((), (), type_vars, sir_child_maps) >>= \ synonyms ->
    pure (SIR.SIR mods adts synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

-- resolving through sir {{{1
assign_in_mods :: UnassignedModuleArena -> (NRReader.NRReader UnassignedADTArena UnassignedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (AssignedModuleArena, Map.Map Type.ADTKey NameMaps.NameMapStack, Map.Map Type.TypeSynonymKey NameMaps.NameMapStack)
assign_in_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM assign_in_module module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

assign_in_adts :: Map.Map Type.ADTKey NameMaps.NameMapStack -> UnassignedADTArena -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) AssignedADTArena
assign_in_adts adt_parent_name_maps = Arena.transform_with_keyM (assign_in_adt adt_parent_name_maps)

assign_in_type_synonyms :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> UnassignedTypeSynonymArena -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) AssignedTypeSynonymArena
assign_in_type_synonyms synonym_parent_name_maps = Arena.transform_with_keyM (assign_in_type_synonym synonym_parent_name_maps)

assign_in_module :: SIR.ModuleKey -> SIR.Module Unassigned -> WriterT (Map Type.ADTKey NameMaps.NameMapStack) (WriterT (Map Type.TypeSynonymKey NameMaps.NameMapStack) (NRReader.NRReader UnassignedADTArena UnassignedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors)) (SIR.Module Assigned)
assign_in_module mod_key (SIR.Module id bindings adts type_synonyms) =
    lift (lift NRReader.ask_sir_child_maps) >>= \ sir_child_maps ->
    let cur_map = NameMaps.get_module_child_maps sir_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) type_synonyms >>
    SIR.Module id <$> mapM (lift . lift . assign_in_binding (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) bindings <*> pure adts <*> pure type_synonyms

assign_in_adt :: Map.Map Type.ADTKey NameMaps.NameMapStack -> Type.ADTKey -> SIR.ADT Unassigned -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.ADT Assigned)
assign_in_adt adt_parent_name_maps adt_key (Type.ADT id name type_vars variants) =
    let parent = adt_parent_name_maps Map.! adt_key
    in
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        type_vars >>= \ type_vars' ->
    lift (NameMaps.make_name_maps type_vars' [] []) >>= \ new_nc ->
    Type.ADT id name type_vars <$> mapM (assign_in_variant (NameMaps.NameMapStack new_nc (Just parent))) variants
    where
        assign_in_variant nc_stack (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> assign_in_type_expr nc_stack ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        assign_in_variant nc_stack (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> assign_in_type_expr nc_stack ty >>= \ ty -> pure (id, (ty, ()))) fields

assign_in_type_synonym :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> Type.TypeSynonymKey -> SIR.TypeSynonym Unassigned -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.TypeSynonym Assigned)
assign_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, ())) =
    let parent = parent_maps Map.! synonym_key
    in assign_in_type_expr parent expansion >>= \ expansion ->
    pure (Type.TypeSynonym id name (expansion, ()))

assign_in_binding :: NameMaps.NameMapStack -> SIR.Binding Unassigned -> (NRReader.NRReader UnassignedADTArena UnassignedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Binding Assigned)
assign_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> assign_in_pat nc_stack target <*> pure eq_sp <*> assign_in_expr nc_stack expr

assign_in_type_expr :: NameMaps.NameMapStack -> SIR.TypeExpr Unassigned -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.TypeExpr Assigned)
assign_in_type_expr nc_stack (SIR.TypeExpr'Refer assigned sp id) = SIR.TypeExpr'Refer assigned sp <$> lift (assign_iden nc_stack id)
assign_in_type_expr nc_stack (SIR.TypeExpr'Get assigned sp parent name) = SIR.TypeExpr'Get assigned sp <$> assign_in_type_expr nc_stack parent <*> pure name
assign_in_type_expr nc_stack (SIR.TypeExpr'Tuple assigned sp a b) = SIR.TypeExpr'Tuple assigned sp <$> assign_in_type_expr nc_stack a <*> assign_in_type_expr nc_stack b
assign_in_type_expr _ (SIR.TypeExpr'Hole assigned type_info sp hid) = pure $ SIR.TypeExpr'Hole assigned type_info sp hid
assign_in_type_expr nc_stack (SIR.TypeExpr'Function assigned sp arg res) = SIR.TypeExpr'Function assigned sp <$> assign_in_type_expr nc_stack arg <*> assign_in_type_expr nc_stack res
assign_in_type_expr nc_stack (SIR.TypeExpr'Forall assigned sp vars ty) =
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    SIR.TypeExpr'Forall assigned sp vars <$> assign_in_type_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) ty
assign_in_type_expr nc_stack (SIR.TypeExpr'Apply assigned sp ty args) = SIR.TypeExpr'Apply assigned sp <$> assign_in_type_expr nc_stack ty <*> assign_in_type_expr nc_stack args
assign_in_type_expr _ (SIR.TypeExpr'Wild assigned sp) = pure $ SIR.TypeExpr'Wild assigned sp
assign_in_type_expr _ (SIR.TypeExpr'Poison assigned sp) = pure $ SIR.TypeExpr'Poison assigned sp

assign_in_pat :: NameMaps.NameMapStack -> SIR.Pattern Unassigned -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Pattern Assigned)
assign_in_pat _ (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
assign_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
assign_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> assign_in_pat nc_stack a <*> assign_in_pat nc_stack b
assign_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> assign_in_pat nc_stack subpat
assign_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant_iden_split variant_assigned tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> assign_split_iden assign_iden nc_stack variant_iden_split <*> pure variant_assigned <*> pure tyargs <*> mapM (assign_in_pat nc_stack) subpat
assign_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant_iden_split variant_assigned tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> assign_split_iden assign_iden nc_stack variant_iden_split <*> pure variant_assigned <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> assign_in_pat nc_stack field_pat) subpat
assign_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

assign_in_expr :: NameMaps.NameMapStack -> SIR.Expr Unassigned -> (NRReader.NRReader UnassignedADTArena UnassignedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Expr Assigned)
assign_in_expr nc_stack (SIR.Expr'Refer id type_info sp iden_split ()) = SIR.Expr'Refer id type_info sp <$> assign_split_iden assign_iden nc_stack iden_split <*> pure ()
assign_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
assign_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
assign_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
assign_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
assign_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

assign_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> assign_in_expr nc_stack a <*> assign_in_expr nc_stack b

assign_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    NameMaps.pattern_vars param >>= \ param_vars ->
    lift (NameMaps.make_name_maps [] param_vars []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> assign_in_pat nc_stack param <*> assign_in_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) body

assign_in_expr nc_stack (SIR.Expr'Let id type_info sp bindings adts type_synonyms body) =
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    NameMaps.make_name_maps_from_decls [] [] [] todo bindings adts type_synonyms >>= \ new_nm ->
    SIR.Expr'Let id type_info sp <$> mapM (assign_in_binding nc_stack) bindings <*> pure adts <*> pure type_synonyms <*> assign_in_expr (NameMaps.NameMapStack new_nm (Just nc_stack)) body

assign_in_expr nc_stack (SIR.Expr'LetRec id type_info sp bindings adts type_synonyms body) =
    NameMaps.make_name_maps_from_decls [] [] [] todo bindings adts type_synonyms >>= \ new_nm ->
    let new_nc_stack = NameMaps.NameMapStack new_nm (Just nc_stack)
    in SIR.Expr'LetRec id type_info sp <$> mapM (assign_in_binding new_nc_stack) bindings <*> pure adts <*> pure type_synonyms <*> assign_in_expr new_nc_stack body

assign_in_expr nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> assign_in_expr nc_stack first
        <*> mapM
            (\ (sp, iden, (), rhs) ->
                (sp,,(),)
                    <$> assign_split_iden assign_iden nc_stack iden
                    <*> assign_in_expr nc_stack rhs)
            ops

assign_in_expr nc_stack (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> assign_in_expr nc_stack callee <*> assign_in_expr nc_stack arg

assign_in_expr nc_stack (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> assign_in_expr nc_stack cond <*> assign_in_expr nc_stack t <*> assign_in_expr nc_stack f
assign_in_expr nc_stack (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> assign_in_expr nc_stack e
        <*> mapM
                (\ (pat, expr) ->
                    assign_in_pat nc_stack pat >>= \ pat' ->
                    NameMaps.pattern_vars pat >>= \ var_children ->
                    lift (NameMaps.make_name_maps [] var_children []) >>= \ arm_nc ->
                    (pat',) <$> assign_in_expr (NameMaps.NameMapStack arm_nc (Just nc_stack)) expr
                )
                arms

assign_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> assign_in_type_expr nc_stack ty) <*> assign_in_expr nc_stack e

assign_in_expr nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> assign_in_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) e
assign_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> assign_in_expr nc_stack e <*> ((, arg_ty) <$> assign_in_type_expr nc_stack arg)

assign_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

assign_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- assigning identifiers {{{1
assign_split_iden :: (NameMaps.NameMapStack -> IdenStart -> Error.WithErrors assigned_iden) -> NameMaps.NameMapStack -> SIR.SplitIdentifier IdenStart Unassigned -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.SplitIdentifier assigned_iden Assigned)
assign_split_iden _ name_map_stack (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> assign_in_type_expr name_map_stack texpr <*> pure next
assign_split_iden assign_single name_map_stack (SIR.SplitIdentifier'Single i) = SIR.SplitIdentifier'Single <$> lift (assign_single name_map_stack i)

assign_iden :: NameMaps.NameMapStack -> IdenStart -> Error.WithErrors AssignedIdenStart
assign_iden nms i = pure (nms, i)
