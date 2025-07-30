module UHF.Parts.UnifiedFrontendSolver.NameResolve.AssignNameMaps
    ( Unassigned
    , Assigned
    , assign
    ) where

import UHF.Prelude

import Data.Functor.Const (Const (Const))
import qualified Data.Map as Map
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NRReader as NRReader
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena
import qualified UHF.Compiler as Compiler
import qualified UHF.Parts.UnifiedFrontendSolver.Error as Solve.Error
import UHF.Parts.UnifiedFrontendSolver.Error (Error)

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey
type NameMapStackArena = Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey

type Unassigned = ((), Const () (), (), (), (), (), ())

type UnassignedModuleArena = Arena.Arena (SIR.Module Unassigned) SIR.ModuleKey
type UnassignedADTArena = Arena.Arena (SIR.ADT Unassigned) Type.ADTKey
type UnassignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Unassigned) Type.TypeSynonymKey
type UnassignedVariableArena = Arena.Arena (SIR.Variable Unassigned) SIR.VariableKey

type Assigned = (NameMaps.NameMapStackKey, Const () (), TypeWithInferVar.Type, (), (), (), ())

type AssignedModuleArena = Arena.Arena (SIR.Module Assigned) SIR.ModuleKey
type AssignedADTArena = Arena.Arena (SIR.ADT Assigned) Type.ADTKey
type AssignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Assigned) Type.TypeSynonymKey

-- helper functions {{{1
new_name_map_stack_end :: Monad m => StateT (NameMapStackArena, NameMaps.SIRChildMaps) m NameMaps.NameMapStackKey
new_name_map_stack_end = state $ \(arena, sir_child_maps) ->
    let (key, arena') = Arena.put (NameMaps.NameMapStack NameMaps.empty_name_maps Nothing) arena
    in (key, (arena', sir_child_maps))

new_name_map_stack_with_parent :: Monad m => NameMaps.NameMapStackKey -> StateT (NameMapStackArena, NameMaps.SIRChildMaps) m NameMaps.NameMapStackKey
new_name_map_stack_with_parent parent = state $ \(arena, sir_child_maps) ->
    let (key, arena') = Arena.put (NameMaps.NameMapStack NameMaps.empty_name_maps (Just parent)) arena
    in (key, (arena', sir_child_maps))

modify_name_map ::
    Monad m => NameMaps.NameMapStackKey -> (NameMaps.NameMaps -> m NameMaps.NameMaps) -> StateT (NameMapStackArena, NameMaps.SIRChildMaps) m ()
modify_name_map key modification = StateT $ \(arena, sir_child_maps) -> do
    modified <- Arena.modifyM arena key (\(NameMaps.NameMapStack name_maps parent) -> NameMaps.NameMapStack <$> modification name_maps <*> pure parent)
    pure ((), (modified, sir_child_maps))

modify_sir_child_maps :: Monad m => (NameMaps.SIRChildMaps -> m NameMaps.SIRChildMaps) -> StateT (NameMapStackArena, NameMaps.SIRChildMaps) m ()
modify_sir_child_maps modification = StateT $ \(arena, sir_child_maps) -> do
    modified <- modification sir_child_maps
    pure ((), (arena, modified))

-- assign entry point {{{1
assign :: SIR.SIR Unassigned -> Compiler.WithDiagnostics Solve.Error.Error Void (SIR.SIR Assigned, NameMapStackArena, NameMaps.SIRChildMaps)
assign sir@(SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    (sir', (name_maps_arena, sir_child_maps)) <-
        runStateT
            ( do
                (mods, adt_parents, type_synonym_parents) <- runReaderT (assign_in_mods mods) (adts, type_synonyms, variables, type_vars, ())
                adts <- runReaderT (assign_in_adts adt_parents adts) ((), (), (), type_vars, ())
                type_synonyms <- runReaderT (assign_in_type_synonyms type_synonym_parents type_synonyms) ((), (), (), type_vars, ())
                pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
            )
            (Arena.new, NameMaps.empty_sir_child_maps sir)
    pure (sir', name_maps_arena, sir_child_maps)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

-- assigning through sir {{{1
type ADTParentAndTypeSynonymParentWriter m =
    WriterT (Map Type.ADTKey NameMaps.NameMapStackKey) (WriterT (Map Type.TypeSynonymKey NameMaps.NameMapStackKey) m)
assign_in_mods ::
    UnassignedModuleArena ->
    NRReader.NRReader
        UnassignedADTArena
        UnassignedTypeSynonymArena
        UnassignedVariableArena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (AssignedModuleArena, Map.Map Type.ADTKey NameMaps.NameMapStackKey, Map.Map Type.TypeSynonymKey NameMaps.NameMapStackKey)
assign_in_mods module_arena = do
    ((module_arena, adt_parents), type_synonym_parents) <- runWriterT $ runWriterT $ Arena.transform_with_keyM assign_in_module module_arena
    pure (module_arena, adt_parents, type_synonym_parents)

assign_in_module ::
    SIR.ModuleKey ->
    SIR.Module Unassigned ->
    ADTParentAndTypeSynonymParentWriter
        ( NRReader.NRReader
            UnassignedADTArena
            UnassignedTypeSynonymArena
            UnassignedVariableArena
            QuantVarArena
            sir_child_maps
            (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        )
        (SIR.Module Assigned)
assign_in_module module_key (SIR.Module id () bindings adts type_synonyms) = do
    module_name_map <- lift $ lift $ lift new_name_map_stack_end

    lift $ lift $ lift $ todo $ modify_name_map module_name_map $ NameMaps.add_to_name_maps primitive_decls primitive_vals [] -- TODO: convert from nr error to unified solver error
    lift $ lift $ lift $ todo $ modify_sir_child_maps $ NameMaps.add_to_module_child_maps primitive_decls primitive_vals [] module_key
    children <- lift $ lift $ NameMaps.decls_to_children bindings adts type_synonyms
    lift $ lift $ lift $ todo $ modify_name_map module_name_map $ NameMaps.add_tuple_to_name_maps children
    lift $ lift $ lift $ todo $ modify_sir_child_maps $ NameMaps.add_tuple_to_module_child_maps children module_key

    mapM_ (\adt -> tell $ Map.singleton adt module_name_map) adts
    mapM_ (\synonym -> lift $ tell $ Map.singleton synonym module_name_map) type_synonyms

    SIR.Module id module_name_map
        <$> mapM (lift . lift . assign_in_binding module_name_map) bindings
        <*> pure adts
        <*> pure type_synonyms
    where
        primitive_decls =
            [ ("int", NameMaps.ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Int)
            , ("float", NameMaps.ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Float)
            , ("char", NameMaps.ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Char)
            , ("string", NameMaps.ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'String)
            , ("bool", NameMaps.ImplicitPrim, SIR.DeclRef'Type TypeWithInferVar.Type'Bool)
            , ("uhf_intrinsics", NameMaps.ImplicitPrim, SIR.DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage)
            ]
        primitive_vals = []

assign_in_adts ::
    Map.Map Type.ADTKey NameMaps.NameMapStackKey ->
    UnassignedADTArena ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        AssignedADTArena
assign_in_adts adt_parent_name_maps = Arena.transform_with_keyM (assign_in_adt adt_parent_name_maps)

assign_in_adt ::
    Map.Map Type.ADTKey NameMaps.NameMapStackKey ->
    Type.ADTKey ->
    SIR.ADT Unassigned ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (SIR.ADT Assigned)
assign_in_adt adt_parent_name_maps adt_key (SIR.ADT id name type_vars variants) = do
    let parent = adt_parent_name_maps Map.! adt_key
    new_name_map_stack <- lift $ new_name_map_stack_with_parent parent

    children <- NameMaps.quant_vars_to_children type_vars
    lift $ todo $ modify_name_map new_name_map_stack $ NameMaps.add_to_name_maps children [] []
    -- TODO: also populate child map (when child maps for adts are implemented)

    SIR.ADT id name type_vars <$> mapM (assign_in_variant new_name_map_stack) variants
    where
        assign_in_variant nc_stack (SIR.ADTVariant'Named name id fields) = SIR.ADTVariant'Named name id <$> mapM (\(id, name, ty) -> assign_in_type_expr nc_stack ty >>= \ty -> pure (id, name, ty)) fields
        assign_in_variant nc_stack (SIR.ADTVariant'Anon name id fields) = SIR.ADTVariant'Anon name id <$> mapM (\(id, ty) -> assign_in_type_expr nc_stack ty >>= \ty -> pure (id, ty)) fields

assign_in_type_synonyms ::
    Map.Map Type.TypeSynonymKey NameMaps.NameMapStackKey ->
    UnassignedTypeSynonymArena ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        AssignedTypeSynonymArena
assign_in_type_synonyms type_synonym_parent_name_maps = Arena.transform_with_keyM (assign_in_type_synonym type_synonym_parent_name_maps)

assign_in_type_synonym ::
    Map.Map Type.TypeSynonymKey NameMaps.NameMapStackKey ->
    Type.TypeSynonymKey ->
    SIR.TypeSynonym Unassigned ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (SIR.TypeSynonym Assigned)
assign_in_type_synonym parent_maps synonym_key (SIR.TypeSynonym id name expansion) = do
    let parent = parent_maps Map.! synonym_key
    expansion <- assign_in_type_expr parent expansion
    pure (SIR.TypeSynonym id name expansion)

assign_in_binding ::
    NameMaps.NameMapStackKey ->
    SIR.Binding Unassigned ->
    NRReader.NRReader
        UnassignedADTArena
        UnassignedTypeSynonymArena
        UnassignedVariableArena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (SIR.Binding Assigned)
assign_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> assign_in_pat nc_stack target <*> pure eq_sp <*> assign_in_expr nc_stack expr

assign_in_type_expr ::
    NameMaps.NameMapStackKey ->
    SIR.TypeExpr Unassigned ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (SIR.TypeExpr Assigned)
assign_in_type_expr nc_stack (SIR.TypeExpr'Refer evaled (Const ()) sp () id) = pure $ SIR.TypeExpr'Refer evaled (Const ()) sp nc_stack id
assign_in_type_expr nc_stack (SIR.TypeExpr'Get evaled (Const ()) sp parent name) = SIR.TypeExpr'Get evaled (Const ()) sp <$> assign_in_type_expr nc_stack parent <*> pure name
assign_in_type_expr nc_stack (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp <$> assign_in_type_expr nc_stack a <*> assign_in_type_expr nc_stack b
assign_in_type_expr _ (SIR.TypeExpr'Hole evaled type_info sp hid) = pure $ SIR.TypeExpr'Hole evaled type_info sp hid
assign_in_type_expr nc_stack (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp <$> assign_in_type_expr nc_stack arg <*> assign_in_type_expr nc_stack res
assign_in_type_expr nc_stack (SIR.TypeExpr'Forall evaled sp () vars ty) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.quant_vars_to_children $ toList vars
    lift $ todo $ modify_name_map new_name_map_stack $ NameMaps.add_to_name_maps children [] []

    SIR.TypeExpr'Forall evaled sp new_name_map_stack vars <$> assign_in_type_expr new_name_map_stack ty
assign_in_type_expr nc_stack (SIR.TypeExpr'Apply assigned sp ty args) = SIR.TypeExpr'Apply assigned sp <$> assign_in_type_expr nc_stack ty <*> assign_in_type_expr nc_stack args
assign_in_type_expr _ (SIR.TypeExpr'Wild assigned sp) = pure $ SIR.TypeExpr'Wild assigned sp
assign_in_type_expr _ (SIR.TypeExpr'Poison assigned sp) = pure $ SIR.TypeExpr'Poison assigned sp

assign_in_expr ::
    NameMaps.NameMapStackKey ->
    SIR.Expr Unassigned ->
    ( NRReader.NRReader
        UnassignedADTArena
        UnassignedTypeSynonymArena
        UnassignedVariableArena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
    )
        (SIR.Expr Assigned)
assign_in_expr nc_stack (SIR.Expr'Refer id type_info sp iden ) = SIR.Expr'Refer id type_info sp <$> assign_split_iden nc_stack iden
assign_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
assign_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
assign_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
assign_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
assign_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
assign_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> assign_in_expr nc_stack a <*> assign_in_expr nc_stack b
assign_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) = do
    body_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.pattern_to_children param
    lift $ todo $ modify_name_map body_name_map_stack $ NameMaps.add_to_name_maps [] children []

    SIR.Expr'Lambda id type_info sp <$> assign_in_pat nc_stack param <*> assign_in_expr body_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'Let id type_info sp () bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.decls_to_children bindings adts type_synonyms
    lift $ todo $ modify_name_map new_name_map_stack $ NameMaps.add_tuple_to_name_maps children

    SIR.Expr'Let id type_info sp new_name_map_stack
        <$> mapM (assign_in_binding nc_stack) bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'LetRec id type_info sp () bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.decls_to_children bindings adts type_synonyms
    lift $ todo $ modify_name_map new_name_map_stack $ NameMaps.add_tuple_to_name_maps children

    SIR.Expr'LetRec id type_info sp new_name_map_stack
        <$> mapM (assign_in_binding new_name_map_stack) bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> assign_in_expr nc_stack first
        <*> mapM
            ( \(sp, iden, rhs) ->
                (sp,,)
                    <$> assign_split_iden nc_stack iden
                    <*> assign_in_expr nc_stack rhs
            )
            ops
assign_in_expr nc_stack (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> assign_in_expr nc_stack callee <*> assign_in_expr nc_stack arg
assign_in_expr nc_stack (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> assign_in_expr nc_stack cond <*> assign_in_expr nc_stack t <*> assign_in_expr nc_stack f
assign_in_expr nc_stack (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> assign_in_expr nc_stack e
        <*> mapM
            ( \((), pat, expr) -> do
                arm_ncs <- lift $ new_name_map_stack_with_parent nc_stack
                children <- NameMaps.pattern_to_children pat
                lift $ todo $ modify_name_map arm_ncs $ NameMaps.add_to_name_maps [] children []

                pat' <- assign_in_pat nc_stack pat
                expr' <- assign_in_expr arm_ncs expr
                pure (arm_ncs, pat', expr')
            )
            arms
assign_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> assign_in_type_expr nc_stack ty) <*> assign_in_expr nc_stack e
assign_in_expr nc_stack (SIR.Expr'Forall id type_info sp () vars e) = do
    new_ncs <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.quant_vars_to_children $ toList vars
    lift $ todo $ modify_name_map new_ncs $ NameMaps.add_to_name_maps children [] []

    SIR.Expr'Forall id type_info sp new_ncs vars <$> assign_in_expr new_ncs e
assign_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> assign_in_expr nc_stack e <*> ((,arg_ty) <$> assign_in_type_expr nc_stack arg)
assign_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
assign_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

assign_in_pat ::
    NameMaps.NameMapStackKey ->
    SIR.Pattern Unassigned ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Error Void))
        (SIR.Pattern Assigned)
assign_in_pat _ (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
assign_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
assign_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> assign_in_pat nc_stack a <*> assign_in_pat nc_stack b
assign_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> assign_in_pat nc_stack subpat
assign_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant type_info sp
        <$> assign_split_iden nc_stack variant_iden
        <*> pure tyargs
        <*> mapM (assign_in_pat nc_stack) subpat
assign_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant type_info sp
        <$> assign_split_iden nc_stack variant_iden
        <*> pure tyargs
        <*> mapM (\(field_name, field_pat) -> (field_name,) <$> assign_in_pat nc_stack field_pat) subpat
assign_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

-- assigning identifiers {{{1
assign_split_iden ::
    NameMaps.NameMapStackKey ->
    SIR.SplitIdentifier resolved Unassigned ->
    NRReader.NRReader
        adt_arena
        type_synonym_arena
        var_arena
        QuantVarArena
        sir_child_maps
        (StateT (NameMapStackArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))
        (SIR.SplitIdentifier resolved Assigned)
assign_split_iden name_map_stack (SIR.SplitIdentifier'Get texpr next (Const ())) = SIR.SplitIdentifier'Get <$> assign_in_type_expr name_map_stack texpr <*> pure next <*> pure (Const ())
assign_split_iden name_map_stack (SIR.SplitIdentifier'Single () i (Const ())) = pure $ SIR.SplitIdentifier'Single name_map_stack i (Const ())
