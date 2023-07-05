{-# LANGUAGE FlexibleContexts #-}

module UHF.Phases.NameResolve
    ( UnresolvedBinding
    , ResolvedBinding
    , UnresolvedExpr
    , ResolvedExpr
    , UnresolvedPattern
    , ResolvedPattern

    , UnresolvedADTArena
    , ResolvedADTArena

    , UnresolvedTypeSynonymArena
    , ResolvedTypeSynonymArena

    , DeclArena

    , resolve
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Span (Span)
import UHF.IO.Located (Located (Located))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map
import qualified Data.List as List

data ChildMaps = ChildMaps (Map.Map Text SIR.DeclKey) (Map.Map Text SIR.BoundValueKey) (Map.Map Text Type.ADTVariantIndex) deriving Show
data ChildMapStack = ChildMapStack ChildMaps (Maybe ChildMapStack)

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey
type BoundValueArena = Arena.Arena (SIR.BoundValue ()) SIR.BoundValueKey
type ModuleChildMaps = Arena.Arena ChildMaps SIR.ModuleKey
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

type UnresolvedDIden = [Located Text]
type UnresolvedVIden = [Located Text]
type UnresolvedPIden = [Located Text]

type UnresolvedSIR = SIR.SIR UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedModule = SIR.Module UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedADT = Type.ADT UnresolvedTypeExpr
type UnresolvedTypeSynonym = Type.TypeSynonym UnresolvedTypeExpr
type UnresolvedTypeExpr = SIR.TypeExpr UnresolvedDIden ()
type UnresolvedBinding = SIR.Binding UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedExpr = SIR.Expr UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedPattern = SIR.Pattern UnresolvedPIden ()

type UnresolvedModuleArena = Arena.Arena UnresolvedModule SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey

type ResolvedDIden = Maybe SIR.DeclKey
type ResolvedVIden = Located (Maybe SIR.BoundValueKey)
type ResolvedPIden = Maybe Type.ADTVariantIndex

type ResolvedSIR = SIR.SIR ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedModule = SIR.Module ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedADT = Type.ADT ResolvedTypeExpr
type ResolvedTypeSynonym = Type.TypeSynonym ResolvedTypeExpr
type ResolvedTypeExpr = SIR.TypeExpr ResolvedDIden ()
type ResolvedBinding = SIR.Binding ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedExpr = SIR.Expr ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedPattern = SIR.Pattern ResolvedPIden ()

type ResolvedModuleArena = Arena.Arena ResolvedModule SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

data Error
    = CouldNotFind (Maybe (Located Text)) (Located Text)
    | MultipleDecls Text [DeclAt]

instance Diagnostic.ToError Error where
    to_error (CouldNotFind prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) message [] []

    to_error (MultipleDecls name decl_ats) =
        let last_decl = last decl_ats
        in Diagnostic.Error
            Diagnostic.Codes.multiple_decls
            (decl_at_span last_decl)
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by implicit import of prelude')

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

make_child_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> Compiler.WithDiagnostics Error Void ChildMaps
make_child_maps decls bound_values adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (ChildMaps (make_map decls) (make_map bound_values) (make_map adt_variants))
    where
        -- separate finding duplicates from making maps so that if there is a duplicate the whole name contexet doesnt just disappear
        -- duplicates will just take the last bound name in the last, because of the how Map.fromList is implemented
        find_dups x =
            let grouped = List.groupBy ((==) `on` get_name) $ List.sortBy (compare `on` get_name) x -- compare names of bound names only
            in filter ((1/=) . length) grouped
            where
                get_name (n, _, _) = n

        make_map x = Map.fromList (map (\ (n, _, v) -> (n, v)) x)

        report_dups = mapM_
            (\ decls ->
                let (first_name, _, _) = head decls
                in Compiler.tell_error $ MultipleDecls first_name (map get_decl_at decls))
            where
                get_decl_at (_, d, _) = d

new_decl d = StateT (\ arena -> pure $ Arena.put d arena)

resolve :: UnresolvedSIR -> Compiler.WithDiagnostics Error Void ResolvedSIR
resolve (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            collect_child_maps mods adts type_synonyms bound_values >>= \ (mod_child_maps) ->
            resolve_in_mods adts bound_values type_vars mod_child_maps mods >>= \ (mods, adt_parents, type_synonym_parents) ->
            resolve_in_adts type_vars bound_values mod_child_maps adt_parents adts >>= \ adts ->
            resolve_in_type_synonyms type_vars bound_values mod_child_maps type_synonym_parents type_synonyms >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars bound_values mod)

-- TODO: clean up this module

collect_child_maps :: UnresolvedModuleArena -> UnresolvedADTArena -> UnresolvedTypeSynonymArena -> BoundValueArena -> StateT DeclArena (Compiler.WithDiagnostics Error Void) (Arena.Arena ChildMaps SIR.ModuleKey) -- TODO: rename UnresolvedDecl to just Decl
collect_child_maps mod_arena adt_arena type_synonym_arena bv_arena = Arena.transformM go mod_arena
    where
        primitive_decls =
            new_decl (SIR.Decl'Type Type.Type'Int) >>= \ int ->
            new_decl (SIR.Decl'Type Type.Type'Float) >>= \ float ->
            new_decl (SIR.Decl'Type Type.Type'Char) >>= \ char ->
            new_decl (SIR.Decl'Type Type.Type'String) >>= \ string ->
            new_decl (SIR.Decl'Type Type.Type'Bool) >>= \ bool ->
            pure
                [ ("int", ImplicitPrim, int)
                , ("float", ImplicitPrim, float)
                , ("char", ImplicitPrim, char)
                , ("string", ImplicitPrim, string)
                , ("bool", ImplicitPrim, bool)
                ]
        primitive_bvs = pure []

        go (SIR.Module _ bindings adts type_synonyms) =
            let (binding_decl_entries, binding_bv_entries, binding_variant_entries) = unzip3 $ map (binding_children adt_arena bv_arena) bindings
            in unzip3 <$>
                mapM
                    (\ adt ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                        in new_decl (SIR.Decl'Type $ Type.Type'ADT adt []) >>= \ adt_decl_key ->
                        pure ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                    adts >>= \ (adt_decl_entries, adt_bv_entries, adt_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                        in new_decl (SIR.Decl'Type $ Type.Type'Synonym synonym) >>= \ synonym_decl_key ->
                        pure ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                    type_synonyms >>= \ (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) ->
            primitive_decls >>= \ primitive_decls ->
            primitive_bvs >>= \ primitive_bvs ->
            lift
                (make_child_maps
                    (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                    (concat $ primitive_bvs : binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
                    (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))

binding_children adt_arena bv_arena (SIR.Binding pat _ _) = ([], pattern_bvs adt_arena bv_arena pat, [])
binding_children adt_arena bv_arena (SIR.Binding'ADTVariant sp bvk variant_index) = ([], [(bv_name adt_arena bv_arena bvk, DeclAt sp, bvk)], [(bv_name adt_arena bv_arena bvk, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs adt_arena bv_arena (SIR.Pattern'Identifier _ sp bvk) = [(bv_name adt_arena bv_arena bvk, DeclAt sp, bvk)]
pattern_bvs adt_arena bv_arena (SIR.Pattern'Wildcard _ sp) = []
pattern_bvs adt_arena bv_arena (SIR.Pattern'Tuple _ sp a b) = pattern_bvs adt_arena bv_arena a ++ pattern_bvs adt_arena bv_arena b
pattern_bvs adt_arena bv_arena (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = (bv_name adt_arena bv_arena bvk, DeclAt bv_span, bvk) : pattern_bvs adt_arena bv_arena subpat
pattern_bvs adt_arena bv_arena (SIR.Pattern'AnonADTVariant _ sp p_iden fields) = concatMap (pattern_bvs adt_arena bv_arena) fields
pattern_bvs adt_arena bv_arena (SIR.Pattern'NamedADTVariant _ sp p_iden fields) = concatMap (pattern_bvs adt_arena bv_arena . snd) fields
pattern_bvs adt_arena bv_arena (SIR.Pattern'Poison _ _) = []

bv_name adt_arena bv_arena bvk =
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> name
        SIR.BoundValue'ADTVariant _ variant_index _ _ ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in Type.variant_name variant

-- TODO: put all things in reader monad?
resolve_in_mods :: UnresolvedADTArena -> BoundValueArena -> TypeVarArena -> ModuleChildMaps -> UnresolvedModuleArena -> StateT DeclArena (Compiler.WithDiagnostics Error Void) (ResolvedModuleArena, Map.Map Type.ADTKey ChildMaps, Map.Map Type.TypeSynonymKey ChildMaps)
resolve_in_mods adt_arena bv_arena type_var_arena module_child_maps module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM (resolve_in_module adt_arena type_var_arena bv_arena module_child_maps) module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

resolve_in_adts :: TypeVarArena -> BoundValueArena -> ModuleChildMaps -> Map.Map Type.ADTKey ChildMaps -> UnresolvedADTArena -> StateT DeclArena (Compiler.WithDiagnostics Error Void) ResolvedADTArena
resolve_in_adts type_var_arena bv_arena module_child_maps adt_parent_child_maps adt_arena = Arena.transform_with_keyM (resolve_in_adt type_var_arena bv_arena module_child_maps adt_parent_child_maps) adt_arena

resolve_in_type_synonyms :: TypeVarArena -> BoundValueArena -> ModuleChildMaps -> Map.Map Type.TypeSynonymKey ChildMaps -> UnresolvedTypeSynonymArena -> StateT DeclArena (Compiler.WithDiagnostics Error Void) ResolvedTypeSynonymArena
resolve_in_type_synonyms type_var_arena bv_arena module_child_maps synonym_parent_child_maps type_synonym_arena = Arena.transform_with_keyM (resolve_in_type_synonym type_var_arena bv_arena module_child_maps synonym_parent_child_maps) type_synonym_arena

resolve_in_module adt_arena type_var_arena bv_arena module_child_maps mod_key (SIR.Module id bindings adts type_synonyms) =
    let cur_map = Arena.get module_child_maps mod_key
    in mapM (\ adt -> tell $ Map.singleton adt cur_map) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym cur_map) type_synonyms >>
    SIR.Module id <$> (mapM (lift . lift . resolve_in_binding adt_arena type_var_arena bv_arena module_child_maps (ChildMapStack cur_map Nothing)) bindings) <*> pure adts <*> pure type_synonyms

-- TODO: remember to make type parameters in scope
resolve_in_adt type_var_arena bv_arena module_child_maps adt_parent_child_maps adt_key (Type.ADT id name type_vars variants) =
    let parent = adt_parent_child_maps Map.! adt_key
    in Type.ADT id name type_vars <$> mapM (resolve_in_variant type_var_arena bv_arena module_child_maps (ChildMapStack parent Nothing)) variants
    where
        resolve_in_variant type_var_arena bv_arena module_child_maps nc_stack (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack ty) fields
        resolve_in_variant type_var_arena bv_arena module_child_maps nc_stack (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM (resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack) fields

resolve_in_type_synonym type_var_arena bv_arena module_child_maps parent_maps synonym_key (Type.TypeSynonym id name expansion) =
    let parent = parent_maps Map.! synonym_key
    in Type.TypeSynonym id name <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps (ChildMapStack parent Nothing) expansion

resolve_in_binding adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat module_child_maps nc_stack target <*> pure eq_sp <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack expr
resolve_in_binding adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Binding'ADTVariant bvk variant sp) = pure $ SIR.Binding'ADTVariant bvk variant sp

resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Identifier type_info sp id) = SIR.TypeExpr'Identifier type_info sp <$> lift (resolve_type_iden todo {- get -} module_child_maps nc_stack id)
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Tuple type_info a b) = SIR.TypeExpr'Tuple type_info <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack a <*> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack b
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Hole type_info sp hid) = pure $ SIR.TypeExpr'Hole type_info sp hid
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Function type_info sp arg res) = SIR.TypeExpr'Function type_info sp <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack arg <*> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack res
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Forall type_info vars ty) =
    mapM
        (\ var ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in new_decl (SIR.Decl'Type $ Type.Type'Variable var) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.TypeExpr'Forall type_info vars <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps (ChildMapStack new_nc (Just nc_stack)) ty
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Apply type_info sp ty args) = SIR.TypeExpr'Apply type_info sp <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack ty <*> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack args
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Wild type_info sp) = pure $ SIR.TypeExpr'Wild type_info sp
resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack (SIR.TypeExpr'Poison type_info sp) = pure $ SIR.TypeExpr'Poison type_info sp

resolve_in_pat module_child_maps nc_stack (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat module_child_maps nc_stack a <*> resolve_in_pat module_child_maps nc_stack b
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat module_child_maps nc_stack subpat
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> lift (resolve_pat_iden todo {- get -} module_child_maps nc_stack (split_iden variant)) <*> mapM (resolve_in_pat module_child_maps nc_stack) subpat
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> lift (resolve_pat_iden todo {- get -} module_child_maps nc_stack (split_iden variant)) <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat module_child_maps nc_stack field_pat) subpat
resolve_in_pat module_child_maps nc_stack (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Identifier id type_info sp i) = SIR.Expr'Identifier id type_info sp <$> lift (resolve_expr_iden todo {- get -} module_child_maps nc_stack (split_iden i))
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack a <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack b

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    lift (make_child_maps [] (pattern_bvs adt_arena bv_arena param) []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat module_child_maps nc_stack param <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps (ChildMapStack new_nc (Just nc_stack)) body

-- TODO: make Let only have 1 binding
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Let id type_info sp bindings body) =
    let (decl_children, bv_children, variant_children) = unzip3 $ map (binding_children adt_arena bv_arena) bindings
    in lift (make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    SIR.Expr'Let id type_info sp <$> mapM (resolve_in_binding adt_arena type_var_arena bv_arena module_child_maps nc_stack) bindings <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps (ChildMapStack new_nc (Just nc_stack)) body

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'LetRec id type_info sp bindings body) =
    let (decl_children, bv_children, variant_children) = unzip3 $ map (binding_children adt_arena bv_arena) bindings
    in lift (make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    let new_nc_stack = ChildMapStack new_nc (Just nc_stack)
    in SIR.Expr'LetRec id type_info sp <$> mapM (resolve_in_binding adt_arena type_var_arena bv_arena module_child_maps new_nc_stack) bindings <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps new_nc_stack body

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) = SIR.Expr'BinaryOps id allowed type_info sp <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack first <*> mapM (\ (iden, rhs) -> (,) <$> lift (resolve_expr_iden todo {- get -} module_child_maps nc_stack (split_iden iden)) <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack rhs) ops

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack callee <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack arg

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack cond <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack t <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack f
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Case id type_info sp case_sp e arms) =
    SIR.Expr'Case id type_info sp case_sp
        <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack e
        <*> mapM
                (\ (pat, expr) ->
                    resolve_in_pat module_child_maps nc_stack pat >>= \ pat' ->
                    let bv_children = pattern_bvs adt_arena bv_arena pat
                    in lift (make_child_maps [] bv_children []) >>= \ arm_nc ->
                    (pat',) <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps (ChildMapStack arm_nc (Just nc_stack)) expr
                )
                arms

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'TypeAnnotation id type_info sp ty e) = SIR.Expr'TypeAnnotation id type_info sp <$> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack ty <*> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack e

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in new_decl (SIR.Decl'Type $ Type.Type'Variable var) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps (ChildMapStack new_nc (Just nc_stack)) e
resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'TypeApply id type_info sp e args) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack e <*> resolve_in_type_expr type_var_arena bv_arena module_child_maps nc_stack args

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr adt_arena type_var_arena bv_arena module_child_maps nc_stack (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

split_iden :: [Located Text] -> (Maybe [Located Text], Located Text)
split_iden [] = error "empty identifier"
split_iden [x] = (Nothing, x)
split_iden x = (Just $ init x, last x)

-- TODO: factor out repeated code?
resolve_expr_iden :: DeclArena -> ModuleChildMaps -> ChildMapStack -> (Maybe [Located Text], Located Text) -> Compiler.WithDiagnostics Error Void (Located (Maybe SIR.BoundValueKey))
resolve_expr_iden decls mods child_map_stack (Just type_iden, last_segment) =
    let sp = Located.just_span (head type_iden) <> Located.just_span last_segment
    in resolve_type_iden decls mods child_map_stack type_iden >>= \ resolved_type ->
    case get_value_child decls mods <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Located sp (Just v)
        Just (Left e) -> Compiler.tell_error e >> pure (Located sp Nothing)
        Nothing -> pure $ Located sp Nothing

resolve_expr_iden _ _ child_map_stack (Nothing, last_segment@(Located last_segment_sp _)) =
    case resolve child_map_stack last_segment of
        Right v -> pure $ Located last_segment_sp (Just v)
        Left e -> Compiler.tell_error e >> pure (Located last_segment_sp Nothing)
    where
        resolve (ChildMapStack (ChildMaps _ bn_children _) parent) name =
            case Map.lookup (Located.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

resolve_type_iden :: DeclArena -> ModuleChildMaps -> ChildMapStack -> UnresolvedDIden -> Compiler.WithDiagnostics Error Void (Maybe SIR.DeclKey)
resolve_type_iden _ _ child_map_stack ([]) = error "empty identifier"
resolve_type_iden decls mods child_map_stack (first:more) =
    case resolve_first child_map_stack first of
        Right first_resolved ->
            case foldlM (get_decl_child decls mods) first_resolved more of
                Right r -> pure $ Just r
                Left e -> Compiler.tell_error e >> pure Nothing
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve_first (ChildMapStack (ChildMaps d_children _ _) parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

resolve_pat_iden :: DeclArena -> ModuleChildMaps -> ChildMapStack -> (Maybe [Located Text], Located Text) -> Compiler.WithDiagnostics Error Void (Maybe Type.ADTVariantIndex)
resolve_pat_iden decls mods child_map_stack (Just type_iden, last_segment) =
    resolve_type_iden decls mods child_map_stack type_iden >>= \ resolved_type ->
    case get_variant_child decls mods <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Just v
        Just (Left e) -> Compiler.tell_error e >> pure Nothing
        Nothing -> pure Nothing

resolve_pat_iden _ _ child_map_stack (Nothing, last_segment) =
    case resolve child_map_stack last_segment of
        Right v -> pure $ Just v
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve (ChildMapStack (ChildMaps _ _ adtv_children) parent) name =
            case Map.lookup (Located.unlocate name) adtv_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

get_decl_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error SIR.DeclKey
get_decl_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let ChildMaps d_children _ _ = Arena.get mods m
                in Map.lookup (Located.unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error SIR.BoundValueKey
get_value_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let ChildMaps _ v_children _ = Arena.get mods m
                in Map.lookup (Located.unlocate name) v_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_variant_child :: DeclArena -> ModuleChildMaps -> SIR.DeclKey -> Located Text -> Either Error Type.ADTVariantIndex
get_variant_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let ChildMaps _ _ adtv_children = Arena.get mods m
                in Map.lookup (Located.unlocate name) adtv_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
