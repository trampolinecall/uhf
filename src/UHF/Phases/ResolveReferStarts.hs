module UHF.Phases.ResolveReferStarts
    ( resolve
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Span (Span)
import UHF.IO.Located (Located (Located, unlocate))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map
import qualified Data.List as List

-- TODO: figure out what adt_parents and type_synonym_parents are

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey
type ModuleChildMaps = Arena.Arena ChildMaps SIR.ModuleKey
-- TODO: instead of decl arena, just have a data Decl = ADT ADTKey | TS TypeSynonymKey or something?, remove SIR.Decl?
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

type UnresolvedIdenStart = Located Text

type Unresolved = (UnresolvedIdenStart, (), (), UnresolvedIdenStart, (), UnresolvedIdenStart, (), (), ())

type UnresolvedADT = Type.ADT (SIR.TypeExpr Unresolved)
type UnresolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Unresolved)

type UnresolvedModuleArena = Arena.Arena (SIR.Module Unresolved) SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey
type UnresolvedBoundValueArena = Arena.Arena (SIR.BoundValue Unresolved) SIR.BoundValueKey

type ResolvedDIdenStart = Maybe SIR.DeclKey
type ResolvedVIdenStart = Maybe SIR.BoundValueKey
type ResolvedPIdenStart = Maybe Type.ADTVariantIndex

type Resolved = (ResolvedDIdenStart, (), (), ResolvedVIdenStart, (), ResolvedPIdenStart, (), (), ())

type ResolvedADT = Type.ADT (SIR.TypeExpr Resolved)
type ResolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Resolved)

type ResolvedModuleArena = Arena.Arena (SIR.Module Resolved) SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

-- error {{{1
data Error
    = CouldNotFind (Located Text)
    | MultipleDecls Text [DeclAt]

instance Diagnostic.ToError Error where
    to_error (CouldNotFind (Located sp name)) = Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) ("could not find name '" <> name <> "'") [] []

    to_error (MultipleDecls name decl_ats) =
        let span = headMay $ mapMaybe decl_at_span decl_ats -- take the first span of the decl_ats; if there are no decl_ats with a span, then this will be Ntohing
        in Diagnostic.Error
            Diagnostic.Codes.multiple_decls
            span
            (show (length decl_ats) <> " declarations of '" <> convert_str name <> "'")
            (map (\ at -> (decl_at_span at, Diagnostic.MsgError, decl_at_message name at)) decl_ats)
            []
        where
            decl_at_span (DeclAt sp) = Just sp
            decl_at_span ImplicitPrim = Nothing
            decl_at_message _ (DeclAt _) = Nothing
            decl_at_message n ImplicitPrim = Just $ "'" <> convert_str n <> "' is implicitly declared as a primitive" -- TODO: reword this message (ideally when it is declared through the prelude import the message would be something like 'implicitly declared by prelude')

-- monads {{{1
type CollectingErrors = Compiler.WithDiagnostics Error Void
type ResolveReader adt_arena bv_arena type_var_arena module_child_maps = ReaderT (adt_arena, bv_arena, type_var_arena, module_child_maps)
type MakeDeclState = StateT DeclArena

new_decl :: Monad under => SIR.Decl -> MakeDeclState under SIR.DeclKey
new_decl d = StateT (\ arena -> pure $ Arena.put d arena)

ask_adt_arena :: Applicative under => ResolveReader adt_arena bv_arena type_var_arena module_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _) -> pure adts
ask_bv_arena :: Applicative under => ResolveReader adt_arena bv_arena type_var_arena module_child_maps under bv_arena
ask_bv_arena = ReaderT $ \ (_, bvs, _, _) -> pure bvs
ask_type_var_arena :: Applicative under => ResolveReader adt_arena bv_arena type_var_arena module_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, tvars, _) -> pure tvars
ask_module_child_maps :: Applicative under => ResolveReader adt_arena bv_arena type_var_arena module_child_maps under module_child_maps
ask_module_child_maps = ReaderT $ \ (_, _, _, mcms) -> pure mcms
-- child maps {{{1
data ChildMaps = ChildMaps (Map.Map Text SIR.DeclKey) (Map.Map Text SIR.BoundValueKey) (Map.Map Text Type.ADTVariantIndex) deriving Show
data ChildMapStack = ChildMapStack ChildMaps (Maybe ChildMapStack)

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

binding_children :: Monad under => (SIR.Binding Unresolved) -> ResolveReader UnresolvedADTArena UnresolvedBoundValueArena type_var_arena module_child_maps under (DeclChildrenList, BoundValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_bvs pat
binding_children (SIR.Binding'ADTVariant sp bvk _ variant_index) = bv_name bvk >>= \ name -> pure ([], [(name, DeclAt sp, bvk)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs :: Monad under => (SIR.Pattern Unresolved) -> ResolveReader UnresolvedADTArena UnresolvedBoundValueArena type_var_arena module_child_maps under BoundValueList
pattern_bvs (SIR.Pattern'Identifier _ sp bvk) = bv_name bvk >>= \ name -> pure [(name, DeclAt sp, bvk)]
pattern_bvs (SIR.Pattern'Wildcard _ _) = pure []
pattern_bvs (SIR.Pattern'Tuple _ _ a b) = pattern_bvs a >>= \ a -> pattern_bvs b >>= \ b -> pure (a ++ b)
pattern_bvs (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = bv_name bvk >>= \ name -> pattern_bvs subpat >>= \ subpat -> pure ((name, DeclAt bv_span, bvk) : subpat)
pattern_bvs (SIR.Pattern'AnonADTVariant _ _ _ _ _ fields) = concat <$> mapM pattern_bvs fields
pattern_bvs (SIR.Pattern'NamedADTVariant _ _ _ _ _ fields) = concat <$> mapM (pattern_bvs . snd) fields
pattern_bvs (SIR.Pattern'Poison _ _) = pure []

bv_name :: Monad under => SIR.BoundValueKey -> ResolveReader UnresolvedADTArena UnresolvedBoundValueArena type_var_arena module_child_maps under Text
bv_name bvk =
    ask_bv_arena >>= \ bv_arena ->
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> pure name
        SIR.BoundValue'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in pure $ unlocate $ Type.variant_name variant

make_child_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> CollectingErrors ChildMaps
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

collect_child_maps :: UnresolvedModuleArena -> UnresolvedTypeSynonymArena -> (ResolveReader UnresolvedADTArena UnresolvedBoundValueArena type_var_arena module_child_maps (MakeDeclState CollectingErrors)) (Arena.Arena ChildMaps SIR.ModuleKey)
collect_child_maps mod_arena type_synonym_arena = Arena.transformM go mod_arena
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
            (unzip3 <$> mapM binding_children bindings) >>= \ (binding_decl_entries, binding_bv_entries, binding_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ adt ->
                        ask_adt_arena >>= \ adt_arena ->
                        let (Type.ADT _ (Located name_sp name) _ _) = Arena.get adt_arena adt
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'ADT adt [])) >>= \ adt_decl_key ->
                        pure ([(name, DeclAt name_sp, adt_decl_key)], [], []) -- constructor bvs and variants handled by adt variant bindings, TODO: make it not the case so that this can deal with named variants too
                    )
                    adts >>= \ (adt_decl_entries, adt_bv_entries, adt_variant_entries) ->
            unzip3 <$>
                mapM
                    (\ synonym ->
                        let (Type.TypeSynonym _ (Located name_sp name) _) = Arena.get type_synonym_arena synonym
                        in lift (new_decl (SIR.Decl'Type $ Type.Type'Synonym synonym)) >>= \ synonym_decl_key ->
                        pure ([(name, DeclAt name_sp, synonym_decl_key)], [], [])
                    )
                    type_synonyms >>= \ (type_synonym_decl_entries, type_synonym_bv_entries, type_synonym_variant_entries) ->
            lift primitive_decls >>= \ primitive_decls ->
            lift primitive_bvs >>= \ primitive_bvs ->
            lift (lift $ make_child_maps
                (concat $ primitive_decls : binding_decl_entries ++ adt_decl_entries ++ type_synonym_decl_entries)
                (concat $ primitive_bvs : binding_bv_entries ++ adt_bv_entries ++ type_synonym_bv_entries)
                (concat $ binding_variant_entries ++ adt_variant_entries ++ type_synonym_variant_entries))
-- resolve entry point {{{1
resolve :: (SIR.SIR Unresolved) -> CollectingErrors (SIR.SIR Resolved)
resolve (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runReaderT (collect_child_maps mods type_synonyms) (adts, bound_values, (), ()) >>= \ module_child_maps ->
            runReaderT (resolve_in_mods mods) (adts, bound_values, type_vars, module_child_maps) >>= \ (mods, adt_parents, type_synonym_parents) ->
            runReaderT (resolve_in_adts adt_parents adts) ((), (), type_vars, module_child_maps) >>= \ adts ->
            runReaderT (resolve_in_type_synonyms type_synonym_parents type_synonyms) ((), (), type_vars, module_child_maps) >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars (Arena.transform change_bound_value bound_values) mod)
    where
        change_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        change_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

-- resolving through sir {{{1
resolve_in_mods :: UnresolvedModuleArena -> (ResolveReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (ResolvedModuleArena, Map.Map Type.ADTKey ChildMaps, Map.Map Type.TypeSynonymKey ChildMaps)
resolve_in_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM resolve_in_module module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

resolve_in_adts :: Map.Map Type.ADTKey ChildMaps -> UnresolvedADTArena -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) ResolvedADTArena
resolve_in_adts adt_parent_child_maps adt_arena = Arena.transform_with_keyM (resolve_in_adt adt_parent_child_maps) adt_arena

resolve_in_type_synonyms :: Map.Map Type.TypeSynonymKey ChildMaps -> UnresolvedTypeSynonymArena -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) ResolvedTypeSynonymArena
resolve_in_type_synonyms synonym_parent_child_maps type_synonym_arena = Arena.transform_with_keyM (resolve_in_type_synonym synonym_parent_child_maps) type_synonym_arena

resolve_in_module :: SIR.ModuleKey -> (SIR.Module Unresolved) -> WriterT (Map Type.ADTKey ChildMaps) (WriterT (Map Type.TypeSynonymKey ChildMaps) (ResolveReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors))) (SIR.Module Resolved)
resolve_in_module mod_key (SIR.Module id bindings adts type_synonyms) =
    lift (lift ask_module_child_maps) >>= \ module_child_maps ->
    let cur_map = Arena.get module_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt cur_map) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym cur_map) type_synonyms >>
    SIR.Module id <$> mapM (lift . lift . resolve_in_binding (ChildMapStack cur_map Nothing)) bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: Map.Map Type.ADTKey ChildMaps -> Type.ADTKey -> UnresolvedADT -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) ResolvedADT
resolve_in_adt adt_parent_child_maps adt_key (Type.ADT id name type_vars variants) =
    let parent = adt_parent_child_maps Map.! adt_key
    in mapM
        (\ var ->
            ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        type_vars >>= \ type_vars' ->
    lift (lift $ make_child_maps type_vars' [] []) >>= \ new_nc ->
    Type.ADT id name type_vars <$> mapM (resolve_in_variant (ChildMapStack new_nc (Just $ ChildMapStack parent Nothing))) variants
    where
        resolve_in_variant nc_stack (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, ty) -> (id, name, ) <$> resolve_in_type_expr nc_stack ty) fields
        resolve_in_variant nc_stack (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, ty) -> (id,) <$> resolve_in_type_expr nc_stack ty) fields

resolve_in_type_synonym :: Map.Map Type.TypeSynonymKey ChildMaps -> Type.TypeSynonymKey -> UnresolvedTypeSynonym -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) ResolvedTypeSynonym
resolve_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name expansion) =
    let parent = parent_maps Map.! synonym_key
    in Type.TypeSynonym id name <$> resolve_in_type_expr (ChildMapStack parent Nothing) expansion

resolve_in_binding :: ChildMapStack -> (SIR.Binding Unresolved) -> (ResolveReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (SIR.Binding Resolved)
resolve_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat nc_stack target <*> pure eq_sp <*> resolve_in_expr nc_stack expr
resolve_in_binding _ (SIR.Binding'ADTVariant bvk variant vars sp) = pure $ SIR.Binding'ADTVariant bvk variant vars sp

resolve_in_type_expr :: ChildMapStack -> (SIR.TypeExpr Unresolved) -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (SIR.TypeExpr Resolved)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Refer resolved sp id) = SIR.TypeExpr'Refer resolved sp <$> lift (lift $ resolve_type_iden nc_stack id)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Get resolved sp parent name) = SIR.TypeExpr'Get resolved sp <$> resolve_in_type_expr nc_stack parent <*> pure name
resolve_in_type_expr nc_stack (SIR.TypeExpr'Tuple resolved a b) = SIR.TypeExpr'Tuple resolved <$> resolve_in_type_expr nc_stack a <*> resolve_in_type_expr nc_stack b
resolve_in_type_expr _ (SIR.TypeExpr'Hole resolved type_info sp hid) = pure $ SIR.TypeExpr'Hole resolved type_info sp hid
resolve_in_type_expr nc_stack (SIR.TypeExpr'Function resolved sp arg res) = SIR.TypeExpr'Function resolved sp <$> resolve_in_type_expr nc_stack arg <*> resolve_in_type_expr nc_stack res
resolve_in_type_expr nc_stack (SIR.TypeExpr'Forall resolved vars ty) =
    mapM
        (\ var ->
            ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.TypeExpr'Forall resolved vars <$> resolve_in_type_expr (ChildMapStack new_nc (Just nc_stack)) ty
resolve_in_type_expr nc_stack (SIR.TypeExpr'Apply resolved sp ty args) = SIR.TypeExpr'Apply resolved sp <$> resolve_in_type_expr nc_stack ty <*> resolve_in_type_expr nc_stack args
resolve_in_type_expr _ (SIR.TypeExpr'Wild resolved sp) = pure $ SIR.TypeExpr'Wild resolved sp
resolve_in_type_expr _ (SIR.TypeExpr'Poison resolved sp) = pure $ SIR.TypeExpr'Poison resolved sp

resolve_in_pat :: ChildMapStack -> (SIR.Pattern Unresolved) -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (SIR.Pattern Resolved)
resolve_in_pat _ (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat nc_stack a <*> resolve_in_pat nc_stack b
resolve_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat nc_stack subpat
resolve_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (resolve_in_pat nc_stack) subpat
resolve_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat nc_stack field_pat) subpat
resolve_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: ChildMapStack -> (SIR.Expr Unresolved) -> (ResolveReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (SIR.Expr Resolved)
resolve_in_expr nc_stack (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> (resolve_split_iden resolve_expr_iden nc_stack iden_split) <*> pure ()
resolve_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr nc_stack a <*> resolve_in_expr nc_stack b

resolve_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    pattern_bvs param >>= \ param_bvs ->
    lift (lift $ make_child_maps [] param_bvs []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat nc_stack param <*> resolve_in_expr (ChildMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'Let id type_info sp bindings body) =
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    (unzip3 <$> mapM binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    SIR.Expr'Let id type_info sp <$> mapM (resolve_in_binding nc_stack) bindings <*> resolve_in_expr (ChildMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'LetRec id type_info sp bindings body) =
    (unzip3 <$> mapM binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    let new_nc_stack = ChildMapStack new_nc (Just nc_stack)
    in SIR.Expr'LetRec id type_info sp <$> mapM (resolve_in_binding new_nc_stack) bindings <*> resolve_in_expr new_nc_stack body

resolve_in_expr nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr nc_stack first
        <*> mapM
            (\ (iden, (), rhs) ->
                (,(),)
                    <$> resolve_split_iden resolve_expr_iden nc_stack iden
                    <*> resolve_in_expr nc_stack rhs)
            ops

resolve_in_expr nc_stack (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr nc_stack callee <*> resolve_in_expr nc_stack arg

resolve_in_expr nc_stack (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr nc_stack cond <*> resolve_in_expr nc_stack t <*> resolve_in_expr nc_stack f
resolve_in_expr nc_stack (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> resolve_in_expr nc_stack e
        <*> mapM
                (\ (pat, expr) ->
                    resolve_in_pat nc_stack pat >>= \ pat' ->
                    pattern_bvs pat >>= \ bv_children ->
                    lift (lift $ make_child_maps [] bv_children []) >>= \ arm_nc ->
                    (pat',) <$> resolve_in_expr (ChildMapStack arm_nc (Just nc_stack)) expr
                )
                arms

resolve_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> resolve_in_type_expr nc_stack ty) <*> resolve_in_expr nc_stack e

resolve_in_expr nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr (ChildMapStack new_nc (Just nc_stack)) e
resolve_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr nc_stack e <*> ((, arg_ty) <$> resolve_in_type_expr nc_stack arg)

resolve_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: (ChildMapStack -> UnresolvedIdenStart -> CollectingErrors resolved_iden) -> ChildMapStack -> SIR.SplitIdentifier Unresolved UnresolvedIdenStart -> (ResolveReader adt_arena bv_arena TypeVarArena ModuleChildMaps (MakeDeclState CollectingErrors)) (SIR.SplitIdentifier Resolved resolved_iden)
resolve_split_iden _ child_map_stack (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> resolve_in_type_expr child_map_stack texpr <*> pure next
resolve_split_iden resolve_start child_map_stack (SIR.SplitIdentifier'Single i) = SIR.SplitIdentifier'Single <$> lift (lift $ resolve_start child_map_stack i)

resolve_iden_start :: (ChildMaps -> Map Text resolved) -> ChildMapStack -> UnresolvedIdenStart -> CollectingErrors (Maybe resolved)
resolve_iden_start which_map child_map_stack iden =
    case go child_map_stack of
        Right resolved -> pure $ Just resolved
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        go (ChildMapStack child_maps parent) =
            case Map.lookup (Located.unlocate iden) (which_map child_maps) of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Left $ CouldNotFind iden

resolve_type_iden :: ChildMapStack -> UnresolvedIdenStart -> CollectingErrors ResolvedDIdenStart
resolve_type_iden = resolve_iden_start (\ (ChildMaps d_children _ _) -> d_children)

resolve_expr_iden :: ChildMapStack -> UnresolvedIdenStart -> CollectingErrors ResolvedVIdenStart
resolve_expr_iden = resolve_iden_start (\ (ChildMaps _ bv_children _) -> bv_children)

resolve_pat_iden :: ChildMapStack -> UnresolvedIdenStart -> CollectingErrors ResolvedPIdenStart
resolve_pat_iden = resolve_iden_start (\ (ChildMaps _ _ adtv_children) -> adtv_children)
