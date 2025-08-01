{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.OtherPreparation.AssignNameMaps
    ( assign
    ) where

import UHF.Prelude

import Data.Functor.Const (Const)
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.SIR.ID as SIR.ID
import qualified UHF.Parts.UnifiedFrontendSolver.Error as Solve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps.Utils as NameMaps.Utils
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef (..))
import qualified UHF.Util.Arena as Arena

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type NameContextArena = Arena.Arena NameMaps.NameContext NameMaps.NameContextKey

-- TODO: rename these
type UnassignedModuleArena = Arena.Arena SIR.Module SIR.ModuleKey
type UnassignedADTArena = Arena.Arena SIR.ADT Type.ADTKey
type UnassignedTypeSynonymArena = Arena.Arena SIR.TypeSynonym Type.TypeSynonymKey

type AssignMonad =
    ReaderT
        SIR.SIR
        ( StateT
            ( NameContextArena
            , NameMaps.SIRChildMaps
            , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
            , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
            )
            (Compiler.WithDiagnostics Solve.Error.Error Void)
        )

-- helper functions {{{1
new_name_map_stack_end ::
    Monad m =>
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        NameMaps.NameContextKey
new_name_map_stack_end = state $ \(arena, sir_child_maps, hcncid_map, hencid_map) ->
    let (key, arena') = Arena.put (NameMaps.NameContext NameMaps.empty_name_maps Nothing) arena
    in (key, (arena', sir_child_maps, hcncid_map, hencid_map))

new_name_map_stack_with_parent ::
    Monad m =>
    NameMaps.NameContextKey ->
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        NameMaps.NameContextKey
new_name_map_stack_with_parent parent = state $ \(arena, sir_child_maps, hcncid_map, hencid_map) ->
    let (key, arena') = Arena.put (NameMaps.NameContext NameMaps.empty_name_maps (Just parent)) arena
    in (key, (arena', sir_child_maps, hcncid_map, hencid_map))

modify_name_map ::
    Monad m =>
    NameMaps.NameContextKey ->
    (NameMaps.NameMaps -> m NameMaps.NameMaps) ->
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        ()
modify_name_map key modification = StateT $ \(arena, sir_child_maps, hcncid_map, hencid_map) -> do
    modified <- Arena.modifyM arena key (\(NameMaps.NameContext name_maps parent) -> NameMaps.NameContext <$> modification name_maps <*> pure parent)
    pure ((), (modified, sir_child_maps, hcncid_map, hencid_map))

modify_sir_child_maps ::
    Monad m =>
    (NameMaps.SIRChildMaps -> m NameMaps.SIRChildMaps) ->
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        ()
modify_sir_child_maps modification = StateT $ \(arena, sir_child_maps, hcncid_map, hencid_map) -> do
    modified <- modification sir_child_maps
    pure ((), (arena, modified, hcncid_map, hencid_map))

put_hcncid ::
    Monad m =>
    SIR.ID.ID "HasChildNameContext" ->
    NameMaps.NameContextKey ->
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        ()
put_hcncid id nk = StateT $ \(arena, sir_child_maps, hcncid_map, hencid_map) -> do
    let hcncid_map' = Map.insert id nk hcncid_map
    pure ((), (arena, sir_child_maps, hcncid_map', hencid_map))

put_hencid ::
    Monad m =>
    SIR.ID.ID "HasEnclosingNameContext" ->
    NameMaps.NameContextKey ->
    StateT
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        m
        ()
put_hencid id nk = StateT $ \(arena, sir_child_maps, hcncid_map, hencid_map) -> do
    let hencid_map' = Map.insert id nk hencid_map
    pure ((), (arena, sir_child_maps, hcncid_map, hencid_map'))

-- TODO: this is a very ad hoc solution and should probably be refactored somehow
convert_add_to_name_maps :: (thing -> (thing, [NameResolve.Error.Error])) -> thing -> Compiler.WithDiagnostics Solve.Error.Error Void thing
convert_add_to_name_maps fn thing =
    let (res, errs) = fn thing
    in Compiler.tell_errors (map Solve.Error.NRError errs) >> pure res

-- assign entry point {{{1
assign ::
    SIR.SIR ->
    Compiler.WithDiagnostics
        Solve.Error.Error
        Void
        ( NameContextArena
        , NameMaps.SIRChildMaps
        , Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
assign sir@(SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    ((), (name_maps_arena, sir_child_maps, hencid_map, hcncid_map)) <-
        runStateT
            ( runReaderT
                ( do
                    (adt_parents, type_synonym_parents) <- assign_in_mods mods
                    _ <- assign_in_adts adt_parents adts -- TODO: this is also not a good use of Arena.transform because it also just discards the result
                    assign_in_type_synonyms type_synonym_parents type_synonyms -- TODO: this is also not a good use of Arena.transform because it also just discards the result
                    pure ()
                )
                sir
            )
            (Arena.new, NameMaps.empty_sir_child_maps sir, Map.empty, Map.empty)
    pure (name_maps_arena, sir_child_maps, hencid_map, hcncid_map)

-- assigning through sir {{{1
type ADTParentAndTypeSynonymParentWriter m =
    WriterT (Map Type.ADTKey NameMaps.NameContextKey) (WriterT (Map Type.TypeSynonymKey NameMaps.NameContextKey) m)
assign_in_mods ::
    UnassignedModuleArena ->
    AssignMonad (Map.Map Type.ADTKey NameMaps.NameContextKey, Map.Map Type.TypeSynonymKey NameMaps.NameContextKey)
assign_in_mods module_arena = do
    ((_, adt_parents), type_synonym_parents) <- runWriterT $ runWriterT $ Arena.transform_with_keyM assign_in_module module_arena -- TODO: this is not a good use of Arena.transform because it just discards the result
    pure (adt_parents, type_synonym_parents)

assign_in_module :: SIR.ModuleKey -> SIR.Module -> ADTParentAndTypeSynonymParentWriter AssignMonad ()
-- TODO: rename mid to id
assign_in_module module_key (SIR.Module mid hcncid id bindings adts type_synonyms) = do
    module_name_map <- lift $ lift $ lift new_name_map_stack_end
    lift $ lift $ lift $ put_hcncid hcncid module_name_map

    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_to_name_maps primitive_decls primitive_vals []
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_to_module_child_maps primitive_decls primitive_vals [] module_key
    children <- lift $ lift $ NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_tuple_to_module_child_maps children module_key

    mapM_ (\adt -> tell $ Map.singleton adt module_name_map) adts
    mapM_ (\synonym -> lift $ tell $ Map.singleton synonym module_name_map) type_synonyms

    mapM_ (lift . lift . assign_in_binding module_name_map) bindings
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

assign_in_adts :: Map.Map Type.ADTKey NameMaps.NameContextKey -> UnassignedADTArena -> AssignMonad (Arena.Arena () Type.ADTKey)
assign_in_adts adt_parent_name_maps = Arena.transform_with_keyM (assign_in_adt adt_parent_name_maps) -- TODO: this is not a good use of Arena.transform because it is just an arena of () which isn't useful and is just discarded later

assign_in_adt :: Map.Map Type.ADTKey NameMaps.NameContextKey -> Type.ADTKey -> SIR.ADT -> AssignMonad ()
assign_in_adt adt_parent_name_maps adt_key (Type.ADT id name type_vars variants) = do
    let parent = adt_parent_name_maps Map.! adt_key
    new_name_map_stack <- lift $ new_name_map_stack_with_parent parent

    children <- NameMaps.Utils.quant_vars_to_children type_vars
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []
    -- TODO: also populate child map (when child maps for adts are implemented)

    _ <- mapM (assign_in_variant new_name_map_stack) variants -- TODO: this should not discard the result (?)
    pure ()
    where
        assign_in_variant nc_stack (Type.ADT.Variant'Named name id fields) = mapM (\(id, name, (ty, teeatid)) -> assign_in_type_expr nc_stack ty) fields
        assign_in_variant nc_stack (Type.ADT.Variant'Anon name id fields) = mapM (\(id, (ty, teeatid)) -> assign_in_type_expr nc_stack ty) fields

assign_in_type_synonyms ::
    Map.Map Type.TypeSynonymKey NameMaps.NameContextKey -> UnassignedTypeSynonymArena -> AssignMonad (Arena.Arena () Type.TypeSynonymKey)
assign_in_type_synonyms type_synonym_parent_name_maps = Arena.transform_with_keyM (assign_in_type_synonym type_synonym_parent_name_maps)

assign_in_type_synonym ::
    Map.Map Type.TypeSynonymKey NameMaps.NameContextKey -> Type.TypeSynonymKey -> SIR.TypeSynonym -> AssignMonad ()
assign_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, teeatid)) = do
    let parent = parent_maps Map.! synonym_key
    assign_in_type_expr parent expansion

assign_in_binding :: NameMaps.NameContextKey -> SIR.Binding -> AssignMonad ()
assign_in_binding nc_stack (SIR.Binding id target eq_sp expr) = assign_in_pat nc_stack target >> assign_in_expr nc_stack expr

assign_in_type_expr :: NameMaps.NameContextKey -> SIR.TypeExpr -> AssignMonad ()
assign_in_type_expr nc_stack (SIR.TypeExpr'Refer id nrid hencid sp iden) = lift $ put_hencid hencid nc_stack
assign_in_type_expr nc_stack (SIR.TypeExpr'Get id nrid sp parent name) = assign_in_type_expr nc_stack parent
assign_in_type_expr nc_stack (SIR.TypeExpr'Tuple id sp a b) = assign_in_type_expr nc_stack a >> assign_in_type_expr nc_stack b
assign_in_type_expr _ (SIR.TypeExpr'Hole id type_info sp hid) = pure ()
assign_in_type_expr nc_stack (SIR.TypeExpr'Function id sp arg res) = assign_in_type_expr nc_stack arg >> assign_in_type_expr nc_stack res
assign_in_type_expr nc_stack (SIR.TypeExpr'Forall id hcncid sp vars ty) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
    lift $ put_hcncid hcncid new_name_map_stack

    children <- NameMaps.Utils.quant_vars_to_children $ toList vars
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []

    assign_in_type_expr new_name_map_stack ty
assign_in_type_expr nc_stack (SIR.TypeExpr'Apply id sp ty args) = assign_in_type_expr nc_stack ty >> assign_in_type_expr nc_stack args
assign_in_type_expr _ (SIR.TypeExpr'Wild id sp) = pure ()
assign_in_type_expr _ (SIR.TypeExpr'Poison id sp) = pure ()

assign_in_expr :: NameMaps.NameContextKey -> SIR.Expr -> AssignMonad ()
-- TODO: rename eid to id
assign_in_expr nc_stack (SIR.Expr'Refer eid id sp iden) = assign_split_iden nc_stack iden
assign_in_expr _ (SIR.Expr'Char eid id sp c) = pure ()
assign_in_expr _ (SIR.Expr'String eid id sp s) = pure ()
assign_in_expr _ (SIR.Expr'Int eid id sp i) = pure ()
assign_in_expr _ (SIR.Expr'Float eid id sp f) = pure ()
assign_in_expr _ (SIR.Expr'Bool eid id sp b) = pure ()
assign_in_expr nc_stack (SIR.Expr'Tuple eid id sp a b) = assign_in_expr nc_stack a >> assign_in_expr nc_stack b
assign_in_expr nc_stack (SIR.Expr'Lambda eid id sp param body) = do
    body_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack

    children <- NameMaps.Utils.pattern_to_children param
    lift $ modify_name_map body_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []

    assign_in_pat nc_stack param
    assign_in_expr body_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'Let eid hcncid id sp bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
    lift $ put_hcncid hcncid new_name_map_stack

    children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children

    mapM_ (assign_in_binding nc_stack) bindings
    assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'LetRec eid hcncid id sp bindings adts type_synonyms body) = do
    new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
    lift $ put_hcncid hcncid new_name_map_stack

    children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children

    mapM_ (assign_in_binding new_name_map_stack) bindings
    assign_in_expr new_name_map_stack body
assign_in_expr nc_stack (SIR.Expr'BinaryOps eid id allowed sp first ops) = do
    assign_in_expr nc_stack first
    mapM_ (\(sp, iden, rhs) -> assign_split_iden nc_stack iden >> assign_in_expr nc_stack rhs) ops
assign_in_expr nc_stack (SIR.Expr'Call eid id sp callee arg) = assign_in_expr nc_stack callee >> assign_in_expr nc_stack arg
assign_in_expr nc_stack (SIR.Expr'If eid id sp if_sp cond t f) = assign_in_expr nc_stack cond >> assign_in_expr nc_stack t >> assign_in_expr nc_stack f
assign_in_expr nc_stack (SIR.Expr'Match eid id sp match_tok_sp e arms) = do
    assign_in_expr nc_stack e
    mapM_
        ( \(hcncid, pat, expr) -> do
            arm_ncs <- lift $ new_name_map_stack_with_parent nc_stack
            lift $ put_hcncid hcncid arm_ncs
            children <- NameMaps.Utils.pattern_to_children pat
            lift $ modify_name_map arm_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []

            pat' <- assign_in_pat nc_stack pat
            expr' <- assign_in_expr arm_ncs expr
            pure (hcncid, pat', expr')
        )
        arms
assign_in_expr nc_stack (SIR.Expr'TypeAnnotation eid id sp (ty, tye_ty) e) = assign_in_type_expr nc_stack ty >> assign_in_expr nc_stack e
assign_in_expr nc_stack (SIR.Expr'Forall eid hcncid id sp vars e) = do
    new_ncs <- lift $ new_name_map_stack_with_parent nc_stack
    lift $ put_hcncid hcncid new_ncs

    children <- NameMaps.Utils.quant_vars_to_children $ toList vars
    lift $ modify_name_map new_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []

    assign_in_expr new_ncs e
assign_in_expr nc_stack (SIR.Expr'TypeApply eid id sp e (arg, arg_ty)) = assign_in_expr nc_stack e >> assign_in_type_expr nc_stack arg
assign_in_expr _ (SIR.Expr'Hole eid id sp hid) = pure ()
assign_in_expr _ (SIR.Expr'Poison eid id sp) = pure ()

assign_in_pat :: NameMaps.NameContextKey -> SIR.Pattern -> AssignMonad ()
assign_in_pat _ (SIR.Pattern'Variable id sp bnk) = pure ()
assign_in_pat _ (SIR.Pattern'Wildcard id sp) = pure ()
assign_in_pat nc_stack (SIR.Pattern'Tuple id sp a b) = assign_in_pat nc_stack a >> assign_in_pat nc_stack b
assign_in_pat nc_stack (SIR.Pattern'Named id sp at_sp bnk subpat) = assign_in_pat nc_stack subpat
assign_in_pat nc_stack (SIR.Pattern'AnonADTVariant id variant_id sp variant_iden subpat) = do
    assign_split_iden nc_stack variant_iden
    mapM_ (assign_in_pat nc_stack) subpat
assign_in_pat nc_stack (SIR.Pattern'NamedADTVariant id variant_id sp variant_iden subpat) = do
    assign_split_iden nc_stack variant_iden
    mapM_ (\(field_name, field_pat) -> assign_in_pat nc_stack field_pat) subpat
assign_in_pat _ (SIR.Pattern'Poison id sp) = pure ()

-- assigning identifiers {{{1
assign_split_iden :: NameMaps.NameContextKey -> SIR.SplitIdentifier id_name -> AssignMonad ()
assign_split_iden name_context (SIR.SplitIdentifier'Get id texpr next) = assign_in_type_expr name_context texpr
assign_split_iden name_context (SIR.SplitIdentifier'Single id hencid i) = do
    lift $ put_hencid hencid name_context
