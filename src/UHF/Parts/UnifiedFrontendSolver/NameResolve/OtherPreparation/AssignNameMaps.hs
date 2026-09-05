{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.OtherPreparation.AssignNameMaps
    ( Unassigned
    , Assigned
    , assign
    ) where

import UHF.Prelude

import Data.Functor.Const (Const (Const))
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
import Data.Data (Proxy (..))
import UHF.Data.SIR.Visitor

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type NameContextArena = Arena.Arena NameMaps.NameContext NameMaps.NameContextKey

type Unassigned = ((), Const () (), (), (), (), (), ())
type Assigned = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())

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

-- assign {{{1
assign :: SIR.SIR Unassigned -> Compiler.WithDiagnostics Solve.Error.Error Void (SIR.SIR Assigned, NameContextArena, NameMaps.SIRChildMaps)
assign sir@(SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    (sir', (name_maps_arena, sir_child_maps)) <-
        runStateT
            ( runReaderT
                ( do
                    ((mods, adt_parents), type_synonym_parents) <- runWriterT $ runWriterT $ Arena.transform_with_keyM assign_in_module mods
                    adts <- Arena.transform_with_keyM (assign_in_adt adt_parents) adts
                    type_synonyms <- Arena.transform_with_keyM (assign_in_type_synonym type_synonym_parents) type_synonyms
                    pure (SIR.SIR mods adts type_synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
                )
                sir
            )
            (Arena.new, NameMaps.empty_sir_child_maps sir)
    pure (sir', name_maps_arena, sir_child_maps)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

type ADTParentAndTypeSynonymParentWriter m = WriterT (Map Type.ADTKey NameMaps.NameContextKey) (WriterT (Map Type.TypeSynonymKey NameMaps.NameContextKey) m)
assign_in_module :: SIR.ModuleKey -> SIR.Module Unassigned -> ADTParentAndTypeSynonymParentWriter AssignMonad (SIR.Module Assigned)
assign_in_module module_key (SIR.Module id () bindings adts type_synonyms) = do
    module_name_map <- lift $ lift $ lift new_name_map_stack_end

    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_to_name_maps primitive_decls primitive_vals [] -- TODO: convert from nr error to unified solver error
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_to_module_child_maps primitive_decls primitive_vals [] module_key
    children <- lift $ lift $ NameMaps.Utils.decls_to_children bindings adts type_synonyms
    lift $ lift $ lift $ modify_name_map module_name_map $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children
    lift $ lift $ lift $ modify_sir_child_maps $ convert_add_to_name_maps $ NameMaps.add_tuple_to_module_child_maps children module_key

    mapM_ (\adt -> tell $ Map.singleton adt module_name_map) adts
    mapM_ (\synonym -> lift $ tell $ Map.singleton synonym module_name_map) type_synonyms

    SIR.Module id module_name_map
        <$> mapM (lift . lift . fmap fst . visit_binding (Proxy :: Proxy AssignVisitor) module_name_map) bindings
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

assign_in_adt :: Map.Map Type.ADTKey NameMaps.NameContextKey -> Type.ADTKey -> SIR.ADT Unassigned -> AssignMonad (SIR.ADT Assigned)
assign_in_adt adt_parent_name_maps adt_key (Type.ADT id name type_vars variants) = do
    let parent = adt_parent_name_maps Map.! adt_key
    new_name_map_stack <- lift $ new_name_map_stack_with_parent parent

    children <- NameMaps.Utils.quant_vars_to_children type_vars
    lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []
    -- TODO: also populate child map (when child maps for adts are implemented)

    Type.ADT id name type_vars <$> mapM (assign_in_variant new_name_map_stack) variants
    where
        assign_in_variant nc_stack (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\(id, name, (ty, ())) -> visit_type_expr (Proxy :: Proxy AssignVisitor) nc_stack ty >>= \(ty, ()) -> pure (id, name, (ty, ()))) fields
        assign_in_variant nc_stack (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\(id, (ty, ())) -> visit_type_expr (Proxy :: Proxy AssignVisitor) nc_stack ty >>= \(ty, ()) -> pure (id, (ty, ()))) fields

assign_in_type_synonym ::
    Map.Map Type.TypeSynonymKey NameMaps.NameContextKey -> Type.TypeSynonymKey -> SIR.TypeSynonym Unassigned -> AssignMonad (SIR.TypeSynonym Assigned)
assign_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, ())) = do
    let parent = parent_maps Map.! synonym_key
    (expansion, ()) <- visit_type_expr (Proxy :: Proxy AssignVisitor) parent expansion
    pure (Type.TypeSynonym id name (expansion, ()))

data AssignVisitor

instance TransformsIdenResolvedKey Unassigned Assigned (SIR.DeclRef ()) (SIR.DeclRef TypeWithInferVar.Type) NameMaps.NameContextKey AssignMonad AssignVisitor where
    transform_iden_resolved_key _ _ (Const ()) = pure $ Const ()
instance TransformsIdenResolvedKey Unassigned Assigned SIR.ValueRef SIR.ValueRef NameMaps.NameContextKey AssignMonad AssignVisitor where
instance TransformsIdenResolvedKey Unassigned Assigned Type.ADT.VariantIndex Type.ADT.VariantIndex NameMaps.NameContextKey AssignMonad AssignVisitor where
instance TransformsTypeExprEvaledKey Unassigned Assigned NameMaps.NameContextKey AssignMonad AssignVisitor where
instance TransformsTypeExprEvaledAsTypeKey Unassigned Assigned NameMaps.NameContextKey AssignMonad AssignVisitor where
instance TransformsTypeInfo Unassigned Assigned NameMaps.NameContextKey AssignMonad AssignVisitor where
instance TransformsInfixGroupedKey Unassigned Assigned NameMaps.NameContextKey AssignMonad AssignVisitor where

instance BindingVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where

instance TypeExprVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
    visit_type_expr_refer _ nc_stack evaled (Const ()) sp () id = pure (SIR.TypeExpr'Refer evaled (Const ()) sp nc_stack id, ())
    visit_type_expr_forall proxy nc_stack evaled sp () vars ty = do
        new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
        children <- NameMaps.Utils.quant_vars_to_children $ toList vars
        lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []

        (ty, ()) <- visit_type_expr proxy new_name_map_stack ty

        pure (SIR.TypeExpr'Forall evaled sp new_name_map_stack vars ty, ())

instance ExprIdentifierRefVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
    visit_expr_identifier_ref_single _ nc_stack () i (Const ()) = pure (SIR.SplitIdentifier'Single nc_stack i (Const ()), ())

instance OperatorRefVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
    visit_operator_ref_single _ nc_stack () i (Const ()) = pure (SIR.SplitIdentifier'Single nc_stack i (Const ()), ())

instance ExprVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
    visit_expr_lambda proxy nc_stack id type_info sp param body = do
        body_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
        children <- NameMaps.Utils.pattern_to_children param
        lift $ modify_name_map body_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []
        (param, ()) <- visit_pattern proxy nc_stack param
        (body, ()) <- visit_expr proxy body_name_map_stack body

        pure (SIR.Expr'Lambda id type_info sp param body, ())

    visit_expr_let proxy nc_stack id type_info sp () bindings adts type_synonyms body = do
        new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
        children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
        lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children
        -- TODO: this also needs to tell parents of the adts and type_synonyms
        bindings <- mapM (fmap fst . visit_binding proxy nc_stack) bindings
        (body, ()) <- visit_expr proxy new_name_map_stack body

        pure (SIR.Expr'Let id type_info sp new_name_map_stack bindings adts type_synonyms body, ())

    visit_expr_let_rec proxy nc_stack id type_info sp () bindings adts type_synonyms body = do
        new_name_map_stack <- lift $ new_name_map_stack_with_parent nc_stack
        children <- NameMaps.Utils.decls_to_children bindings adts type_synonyms
        lift $ modify_name_map new_name_map_stack $ convert_add_to_name_maps $ NameMaps.add_tuple_to_name_maps children
        -- TODO: this also needs to tell parents of the adts and type_synonyms
        bindings <- mapM (fmap fst . visit_binding proxy new_name_map_stack) bindings
        (body, ()) <- visit_expr proxy new_name_map_stack body

        pure (SIR.Expr'LetRec id type_info sp new_name_map_stack bindings adts type_synonyms body, ())

    visit_expr_match proxy nc_stack id type_info sp match_tok_sp e arms = do
        (e, ()) <- visit_expr proxy nc_stack e
        arms <- mapM
            ( \((), pat, expr) -> do
                arm_ncs <- lift $ new_name_map_stack_with_parent nc_stack
                children <- NameMaps.Utils.pattern_to_children pat
                lift $ modify_name_map arm_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps [] children []

                (pat', ()) <- visit_pattern proxy nc_stack pat
                (expr', ()) <- visit_expr proxy arm_ncs expr

                pure (arm_ncs, pat', expr')
            )
            arms

        pure (SIR.Expr'Match id type_info sp match_tok_sp e arms, ())

    visit_expr_forall proxy nc_stack id type_info sp () vars e = do
        new_ncs <- lift $ new_name_map_stack_with_parent nc_stack
        children <- NameMaps.Utils.quant_vars_to_children $ toList vars
        lift $ modify_name_map new_ncs $ convert_add_to_name_maps $ NameMaps.add_to_name_maps children [] []
        (e, ()) <- visit_expr proxy new_ncs e

        pure (SIR.Expr'Forall id type_info sp new_ncs vars e, ())

instance PatternADTVariantRefVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
    visit_pattern_adt_variant_ref_single _ nc_stack () i (Const ()) = pure (SIR.SplitIdentifier'Single nc_stack i (Const ()), ())

instance PatternVisitor Unassigned Assigned NameMaps.NameContextKey AssignMonad () AssignVisitor where
