module UHF.Parts.UnifiedFrontendSolver.NameResolve.OtherPreparation.AssignNameMaps
    ( Unassigned
    , Assigned
    , assign
    ) where

import UHF.Prelude

import Data.Functor.Const (Const)
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.Error as Solve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps.Utils as NameMaps.Utils
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef(..))

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type NameContextArena = Arena.Arena NameMaps.NameContext NameMaps.NameContextKey

type Unassigned = ((), Const () (), (), (), (), (), ())

type UnassignedModuleArena = Arena.Arena (SIR.Module Unassigned) SIR.ModuleKey
type UnassignedADTArena = Arena.Arena (SIR.ADT Unassigned) Type.ADTKey
type UnassignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Unassigned) Type.TypeSynonymKey

type Assigned = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())

type AssignedModuleArena = Arena.Arena (SIR.Module Assigned) SIR.ModuleKey
type AssignedADTArena = Arena.Arena (SIR.ADT Assigned) Type.ADTKey
type AssignedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Assigned) Type.TypeSynonymKey

type AssignMonad = ReaderT (SIR.SIR Unassigned) (StateT (NameContextArena, NameMaps.SIRChildMaps) (Compiler.WithDiagnostics Solve.Error.Error Void))

-- helper functions {{{1
new_name_map_stack_end :: Monad m => StateT (NameContextArena, NameMaps.SIRChildMaps) m NameMaps.NameContextKey
new_name_map_stack_end = state $ \(arena, sir_child_maps) ->
    let (key, arena') = Arena.put (NameMaps.NameContext NameMaps.empty_name_maps Nothing) arena
    in (key, (arena', sir_child_maps))

new_name_map_stack_with_parent :: Monad m => NameMaps.NameContextKey -> StateT (NameContextArena, NameMaps.SIRChildMaps) m NameMaps.NameContextKey
new_name_map_stack_with_parent parent = state $ \(arena, sir_child_maps) ->
    let (key, arena') = Arena.put (NameMaps.NameContext NameMaps.empty_name_maps (Just parent)) arena
    in (key, (arena', sir_child_maps))

modify_name_map ::
    Monad m => NameMaps.NameContextKey -> (NameMaps.NameMaps -> m NameMaps.NameMaps) -> StateT (NameContextArena, NameMaps.SIRChildMaps) m ()
modify_name_map key modification = StateT $ \(arena, sir_child_maps) -> do
    modified <- Arena.modifyM arena key (\(NameMaps.NameContext name_maps parent) -> NameMaps.NameContext <$> modification name_maps <*> pure parent)
    pure ((), (modified, sir_child_maps))

modify_sir_child_maps :: Monad m => (NameMaps.SIRChildMaps -> m NameMaps.SIRChildMaps) -> StateT (NameContextArena, NameMaps.SIRChildMaps) m ()
modify_sir_child_maps modification = StateT $ \(arena, sir_child_maps) -> do
    modified <- modification sir_child_maps
    pure ((), (arena, modified))

-- TODO: this is a very ad hoc solution and should probably be refactored somehow
convert_add_to_name_maps :: (thing -> (thing, [NameResolve.Error.Error])) -> thing -> Compiler.WithDiagnostics Solve.Error.Error Void thing
convert_add_to_name_maps fn thing =
    let (res, errs) = fn thing
    in Compiler.tell_errors (map Solve.Error.NRError errs) >> pure res

-- assign entry point {{{1
assign :: SIR.SIR Unassigned -> Compiler.WithDiagnostics Solve.Error.Error Void (SIR.SIR Assigned, NameContextArena, NameMaps.SIRChildMaps)
assign sir@(SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    (sir', (name_maps_arena, sir_child_maps)) <-
        runStateT
            ( runReaderT
                ( do
                    (mods, adt_parents, type_synonym_parents) <- assign_in_mods mods
                    adts <- assign_in_adts adt_parents adts
                    type_synonyms <- assign_in_type_synonyms type_synonym_parents type_synonyms
                    pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
                )
                sir
            )
            (Arena.new, NameMaps.empty_sir_child_maps sir)
    pure (sir', name_maps_arena, sir_child_maps)
    where
        change_variable (SIR.Variable id varid tyinfo n) = SIR.Variable id varid tyinfo n

-- assigning through sir {{{1
type ADTParentAndTypeSynonymParentWriter m =
    WriterT (Map Type.ADTKey NameMaps.NameContextKey) (WriterT (Map Type.TypeSynonymKey NameMaps.NameContextKey) m)
assign_in_mods ::
    UnassignedModuleArena ->
    AssignMonad (AssignedModuleArena, Map.Map Type.ADTKey NameMaps.NameContextKey, Map.Map Type.TypeSynonymKey NameMaps.NameContextKey)
assign_in_mods module_arena = do
    ((module_arena, adt_parents), type_synonym_parents) <- runWriterT $ runWriterT $ Arena.transform_with_keyM assign_in_module module_arena
    pure (module_arena, adt_parents, type_synonym_parents)

assign_in_module :: SIR.ModuleKey -> SIR.Module Unassigned -> ADTParentAndTypeSynonymParentWriter AssignMonad (SIR.Module Assigned)
-- TODO: rename mid to id
assign_in_module module_key (SIR.Module mid id () bindings adts type_synonyms) = do
    module_name_map <- lift $ lift $ lift new_name_map_stack_end

    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_to_name_maps primitive_decls primitive_vals [] -- TODO: convert from nr error to unified solver error
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_to_module_child_maps primitive_decls primitive_vals [] module_key
    children <- lift $ lift $ NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_tuple_to_module_child_maps children module_key

    mapM_ (\adt -> tell $ Map.singleton adt module_name_map) adts
    mapM_ (\synonym -> lift $ tell $ Map.singleton synonym module_name_map) type_synonyms

    SIR.Module mid id module_name_map
        <$> mapM (lift . lift . assign_in_binding module_name_map) bindings
        <*> pure adts
        <*> pure type_synonyms
    where
        primitive_decls =
            [ ("int", NameMaps.ImplicitPrim, DeclRef'Type TypeWithInferVar.Type'Int)
            , ("float", NameMaps.ImplicitPrim, DeclRef'Type TypeWithInferVar.Type'Float)
            , ("char", NameMaps.ImplicitPrim, DeclRef'Type TypeWithInferVar.Type'Char)
            , ("string", NameMaps.ImplicitPrim, DeclRef'Type TypeWithInferVar.Type'String)
            , ("bool", NameMaps.ImplicitPrim, DeclRef'Type TypeWithInferVar.Type'Bool)
            , ("uhf_intrinsics", NameMaps.ImplicitPrim, DeclRef'ExternPackage SIR.ExternPackage'IntrinsicsPackage)
            ]
        primitive_vals = []

assign_in_adts :: Map.Map Type.ADTKey NameMaps.NameContextKey -> UnassignedADTArena -> AssignMonad AssignedADTArena
assign_in_adts adt_parent_name_maps = Arena.transform_with_keyM (assign_in_adt adt_parent_name_maps)

assign_in_adt :: Map.Map Type.ADTKey NameMaps.NameContextKey -> Type.ADTKey -> SIR.ADT Unassigned -> AssignMonad (SIR.ADT Assigned)
assign_in_adt adt_parent_name_maps adt_key (Type.ADT id name type_vars variants) = do
    let parent = adt_parent_name_maps Map.! adt_key
    new_name_map_stack <- lift $ new_name_map_stack_with_parent parent

    children <- NameMaps.Utils.quant_vars_to_children type_vars
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []
    -- TODO: also populate child map (when child maps for adts are implemented)

    Type.ADT id name type_vars <$> mapM (assign_in_variant new_name_map_stack) variants
    where
        assign_in_variant nc_stack (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\(id, name, (ty, ())) -> assign_in_type_expr nc_stack ty >>= \ty -> pure (id, name, (ty, ()))) fields
        assign_in_variant nc_stack (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\(id, (ty, ())) -> assign_in_type_expr nc_stack ty >>= \ty -> pure (id, (ty, ()))) fields

assign_in_type_synonyms :: Map.Map Type.TypeSynonymKey NameMaps.NameContextKey -> UnassignedTypeSynonymArena -> AssignMonad AssignedTypeSynonymArena
assign_in_type_synonyms type_synonym_parent_name_maps = Arena.transform_with_keyM (assign_in_type_synonym type_synonym_parent_name_maps)

assign_in_type_synonym ::
    Map.Map Type.TypeSynonymKey NameMaps.NameContextKey -> Type.TypeSynonymKey -> SIR.TypeSynonym Unassigned -> AssignMonad (SIR.TypeSynonym Assigned)
assign_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, ())) = do
    let parent = parent_maps Map.! synonym_key
    expansion <- assign_in_type_expr parent expansion
    pure (Type.TypeSynonym id name (expansion, ()))

assign_in_binding :: NameMaps.NameContextKey -> SIR.Binding Unassigned -> AssignMonad (SIR.Binding Assigned)
assign_in_binding nc_stack (SIR.Binding id target eq_sp expr) = SIR.Binding id <$> assign_in_pat nc_stack target <*> pure eq_sp <*> assign_in_expr nc_stack expr

assign_in_type_expr :: NameMaps.NameContextKey -> SIR.TypeExpr Unassigned -> AssignMonad (SIR.TypeExpr Assigned)
assign_in_type_expr nc_stack (SIR.TypeExpr'Refer id evaled sp () iden) = pure $ SIR.TypeExpr'Refer id evaled sp nc_stack iden
assign_in_type_expr nc_stack (SIR.TypeExpr'Get id evaled sp parent name) = SIR.TypeExpr'Get id evaled sp <$> assign_in_type_expr nc_stack parent <*> pure name
assign_in_type_expr nc_stack (SIR.TypeExpr'Tuple id evaled sp a b) = SIR.TypeExpr'Tuple id evaled sp <$> assign_in_type_expr nc_stack a <*> assign_in_type_expr nc_stack b
assign_in_type_expr _ (SIR.TypeExpr'Hole id evaled type_info sp hid) = pure $ SIR.TypeExpr'Hole id evaled type_info sp hid
assign_in_type_expr nc_stack (SIR.TypeExpr'Function id evaled sp arg res) = SIR.TypeExpr'Function id evaled sp <$> assign_in_type_expr nc_stack arg <*> assign_in_type_expr nc_stack res
assign_in_type_expr nc_stack (SIR.TypeExpr'Forall id evaled sp () vars ty) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.quant_vars_to_children $ toList vars
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []

    SIR.TypeExpr'Forall id evaled sp new_name_map_stack vars <$> assign_in_type_expr new_name_map_stack ty
assign_in_type_expr nc_stack (SIR.TypeExpr'Apply id assigned sp ty args) = SIR.TypeExpr'Apply id assigned sp <$> assign_in_type_expr nc_stack ty <*> assign_in_type_expr nc_stack args
assign_in_type_expr _ (SIR.TypeExpr'Wild id assigned sp) = pure $ SIR.TypeExpr'Wild id assigned sp
assign_in_type_expr _ (SIR.TypeExpr'Poison id assigned sp) = pure $ SIR.TypeExpr'Poison id assigned sp

assign_in_expr :: NameMaps.NameContextKey -> SIR.Expr Unassigned -> AssignMonad (SIR.Expr Assigned)
-- TODO: rename eid to id
assign_in_expr nc_stack (SIR.Expr'Refer eid id type_info sp iden) = SIR.Expr'Refer eid id type_info sp <$> assign_split_iden nc_stack iden
assign_in_expr _ (SIR.Expr'Char eid id type_info sp c) = pure $ SIR.Expr'Char eid id type_info sp c
assign_in_expr _ (SIR.Expr'String eid id type_info sp s) = pure $ SIR.Expr'String eid id type_info sp s
assign_in_expr _ (SIR.Expr'Int eid id type_info sp i) = pure $ SIR.Expr'Int eid id type_info sp i
assign_in_expr _ (SIR.Expr'Float eid id type_info sp f) = pure $ SIR.Expr'Float eid id type_info sp f
assign_in_expr _ (SIR.Expr'Bool eid id type_info sp b) = pure $ SIR.Expr'Bool eid id type_info sp b
assign_in_expr nc_stack (SIR.Expr'Tuple eid id type_info sp a b) = SIR.Expr'Tuple eid id type_info sp <$> assign_in_expr nc_stack a <*> assign_in_expr nc_stack b
assign_in_expr nc_stack (SIR.Expr'Lambda eid id type_info sp param body) = do
    body_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.pattern_to_children param
    lift $ modify_name_map body_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []

    SIR.Expr'Lambda eid id type_info sp <$> assign_in_pat nc_stack param <*> assign_in_expr body_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'Let eid id type_info sp () bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children

    SIR.Expr'Let eid id type_info sp new_name_map_stack
        <$> mapM (assign_in_binding nc_stack) bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'LetRec eid id type_info sp () bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children

    SIR.Expr'LetRec eid id type_info sp new_name_map_stack
        <$> mapM (assign_in_binding new_name_map_stack) bindings
        <*> pure adts
        <*> pure type_synonyms
        <*> assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'BinaryOps eid id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps eid id allowed type_info sp
        <$> assign_in_expr nc_stack first
        <*> mapM
            ( \(sp, iden, rhs) ->
                (sp,,)
                    <$> assign_split_iden nc_stack iden
                    <*> assign_in_expr nc_stack rhs
            )
            ops
assign_in_expr nc_stack (SIR.Expr'Call eid id type_info sp callee arg) = SIR.Expr'Call eid id type_info sp <$> assign_in_expr nc_stack callee <*> assign_in_expr nc_stack arg
assign_in_expr nc_stack (SIR.Expr'If eid id type_info sp if_sp cond t f) = SIR.Expr'If eid id type_info sp if_sp <$> assign_in_expr nc_stack cond <*> assign_in_expr nc_stack t <*> assign_in_expr nc_stack f
assign_in_expr nc_stack (SIR.Expr'Match eid id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match eid id type_info sp match_tok_sp
        <$> assign_in_expr nc_stack e
        <*> mapM
            ( \((), pat, expr) -> do
                arm_ncs <- lift $ new_name_map_stack_with_parent nc_stack
                children <- NameMaps.Utils.pattern_to_children pat
                lift $ modify_name_map arm_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []

                pat' <- assign_in_pat nc_stack pat
                expr' <- assign_in_expr arm_ncs expr
                pure (arm_ncs, pat', expr')
            )
            arms
assign_in_expr nc_stack (SIR.Expr'TypeAnnotation eid id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation eid id type_info sp <$> ((,tye_ty) <$> assign_in_type_expr nc_stack ty) <*> assign_in_expr nc_stack e
assign_in_expr nc_stack (SIR.Expr'Forall eid id type_info sp () vars e) = do
    new_ncs <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.quant_vars_to_children $ toList vars
    lift $ modify_name_map new_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []

    SIR.Expr'Forall eid id type_info sp new_ncs vars <$> assign_in_expr new_ncs e
assign_in_expr nc_stack (SIR.Expr'TypeApply eid id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply eid id type_info sp <$> assign_in_expr nc_stack e <*> ((,arg_ty) <$> assign_in_type_expr nc_stack arg)
assign_in_expr _ (SIR.Expr'Hole eid id type_info sp hid) = pure $ SIR.Expr'Hole eid id type_info sp hid
assign_in_expr _ (SIR.Expr'Poison eid id type_info sp) = pure $ SIR.Expr'Poison eid id type_info sp

assign_in_pat :: NameMaps.NameContextKey -> SIR.Pattern Unassigned -> AssignMonad (SIR.Pattern Assigned)
assign_in_pat _ (SIR.Pattern'Variable id type_info sp bnk) = pure $ SIR.Pattern'Variable id type_info sp bnk
assign_in_pat _ (SIR.Pattern'Wildcard id type_info sp) = pure $ SIR.Pattern'Wildcard id type_info sp
assign_in_pat nc_stack (SIR.Pattern'Tuple id type_info sp a b) = SIR.Pattern'Tuple id type_info sp <$> assign_in_pat nc_stack a <*> assign_in_pat nc_stack b
assign_in_pat nc_stack (SIR.Pattern'Named id type_info sp at_sp bnk subpat) = SIR.Pattern'Named id type_info sp at_sp bnk <$> assign_in_pat nc_stack subpat
assign_in_pat nc_stack (SIR.Pattern'AnonADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant id type_info sp
        <$> assign_split_iden nc_stack variant_iden
        <*> pure tyargs
        <*> mapM (assign_in_pat nc_stack) subpat
assign_in_pat nc_stack (SIR.Pattern'NamedADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant id type_info sp
        <$> assign_split_iden nc_stack variant_iden
        <*> pure tyargs
        <*> mapM (\(field_name, field_pat) -> (field_name,) <$> assign_in_pat nc_stack field_pat) subpat
assign_in_pat _ (SIR.Pattern'Poison id type_info sp) = pure $ SIR.Pattern'Poison id type_info sp

-- assigning identifiers {{{1
assign_split_iden :: NameMaps.NameContextKey -> SIR.SplitIdentifier id_name Unassigned -> AssignMonad (SIR.SplitIdentifier id_name Assigned)
assign_split_iden name_context (SIR.SplitIdentifier'Get id texpr next) = SIR.SplitIdentifier'Get id <$> assign_in_type_expr name_context texpr <*> pure next
assign_split_iden name_context (SIR.SplitIdentifier'Single id () i) = pure $ SIR.SplitIdentifier'Single id name_context i
