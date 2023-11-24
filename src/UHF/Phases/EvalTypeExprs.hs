module UHF.Phases.EvalTypeExprs
    ( eval
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

-- TODO: change errors, clean up this whole module, clean up duplication with name resolution module

type DeclMap = Map.Map Text SIR.DeclKey
data DeclMapStack = DeclMapStack DeclMap (Maybe DeclMapStack)

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey
type ModuleDeclMap = Arena.Arena DeclMap SIR.ModuleKey
type DeclArena = Arena.Arena SIR.Decl SIR.DeclKey

type UnevaledDIden = [Located Text]
type UnevaledVIden = Located (Maybe [Located Text], Located Text)
type UnevaledPIden = (Maybe [Located Text], Located Text)

type Unevaled = (UnevaledDIden, UnevaledVIden, UnevaledPIden, (), (), ())

-- TODO: remove these type aliases
type UnevaledSIR = SIR.SIR Unevaled
type UnevaledModule = SIR.Module Unevaled
type UnevaledADT = Type.ADT UnevaledTypeExpr
type UnevaledTypeSynonym = Type.TypeSynonym UnevaledTypeExpr
type UnevaledTypeExpr = SIR.TypeExpr Unevaled
type UnevaledBinding = SIR.Binding Unevaled
type UnevaledExpr = SIR.Expr Unevaled
type UnevaledPattern = SIR.Pattern Unevaled

type UnevaledModuleArena = Arena.Arena UnevaledModule SIR.ModuleKey
type UnevaledADTArena = Arena.Arena UnevaledADT Type.ADTKey
type UnevaledTypeSynonymArena = Arena.Arena UnevaledTypeSynonym Type.TypeSynonymKey
type UnevaledBoundValueArena = Arena.Arena (SIR.BoundValue Unevaled) SIR.BoundValueKey

type EvaledDIden = Maybe SIR.DeclKey
type EvaledVIden = Located (Maybe (Either () SIR.DeclKey), Located Text)
type EvaledPIden = (Maybe (Either () SIR.DeclKey), Located Text)

type Evaled = (EvaledDIden, EvaledVIden, EvaledPIden, Maybe (Type.Type Void), (), ())

type EvaledSIR = SIR.SIR Evaled
type EvaledModule = SIR.Module Evaled
type EvaledADT = Type.ADT EvaledTypeExpr
type EvaledTypeSynonym = Type.TypeSynonym EvaledTypeExpr
type EvaledTypeExpr = SIR.TypeExpr Evaled
type EvaledBinding = SIR.Binding Evaled
type EvaledExpr = SIR.Expr Evaled
type EvaledPattern = SIR.Pattern Evaled

type EvaledModuleArena = Arena.Arena EvaledModule SIR.ModuleKey
type EvaledADTArena = Arena.Arena EvaledADT Type.ADTKey
type EvaledTypeSynonymArena = Arena.Arena EvaledTypeSynonym Type.TypeSynonymKey

type CollectingErrors = Compiler.WithDiagnostics Error Void

type MakeDeclState = StateT DeclArena
type EvalReader adt_arena bv_arena type_var_arena module_child_maps = ReaderT (DeclArena, adt_arena, bv_arena, type_var_arena, module_child_maps)

data Error
    = CouldNotFind (Maybe (Located Text)) (Located Text)
    | MultipleDecls Text [DeclAt]
    | NotAType Span Text

instance Diagnostic.ToError Error where
    to_error (CouldNotFind prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) message [] []

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

    to_error (NotAType sp instead) =
        Diagnostic.Error Diagnostic.Codes.not_a_type (Just sp) ("not a type: got " <> instead) [] []

eval :: UnevaledSIR -> Compiler.WithDiagnostics Error Void EvaledSIR
eval (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runReaderT (collect_child_maps mods type_synonyms) (decls, adts, bound_values, (), ()) >>= \ module_child_maps ->
            runReaderT (resolve_in_mods mods) (decls, adts, bound_values, type_vars, module_child_maps) >>= \ (mods, adt_parents, type_synonym_parents) ->
            runReaderT (resolve_in_adts adt_parents adts) (decls, (), (), type_vars, module_child_maps) >>= \ adts ->
            runReaderT (resolve_in_type_synonyms type_synonym_parents type_synonyms) (decls, (), (), type_vars, module_child_maps) >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars (Arena.transform change_bound_value bound_values) mod)
    where
        change_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        change_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

new_decl :: Monad under => SIR.Decl -> MakeDeclState under SIR.DeclKey
new_decl d = StateT (\ arena -> pure $ Arena.put d arena)

ask_decl_arena :: Applicative under => EvalReader adt_arena bv_arena type_var_arena module_child_maps under DeclArena
ask_decl_arena = ReaderT $ \ (decls, _, _, _, _) -> pure decls
ask_adt_arena :: Applicative under => EvalReader adt_arena bv_arena type_var_arena module_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (_, adts, _, _, _) -> pure adts
ask_bv_arena :: Applicative under => EvalReader adt_arena bv_arena type_var_arena module_child_maps under bv_arena
ask_bv_arena = ReaderT $ \ (_, _, bvs, _, _) -> pure bvs
ask_type_var_arena :: Applicative under => EvalReader adt_arena bv_arena type_var_arena module_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, _, tvars, _) -> pure tvars
ask_module_child_maps :: Applicative under => EvalReader adt_arena bv_arena type_var_arena module_child_maps under module_child_maps
ask_module_child_maps = ReaderT $ \ (_, _, _, _, mcms) -> pure mcms

data DeclAt = DeclAt Span | ImplicitPrim deriving Show

type DeclChildrenList = [(Text, DeclAt, SIR.DeclKey)]
type BoundValueList = [(Text, DeclAt, SIR.BoundValueKey)]
type ADTVariantList = [(Text, DeclAt, Type.ADTVariantIndex)]

binding_children :: Monad under => UnevaledBinding -> EvalReader UnevaledADTArena UnevaledBoundValueArena type_var_arena module_child_maps under (DeclChildrenList, BoundValueList, ADTVariantList)
binding_children (SIR.Binding pat _ _) = ([],, []) <$> pattern_bvs pat
binding_children (SIR.Binding'ADTVariant sp bvk _ variant_index) = bv_name bvk >>= \ name -> pure ([], [(name, DeclAt sp, bvk)], [(name, DeclAt sp, variant_index)]) -- TODO: move variants to inside their types, also dont handle adt variants here

pattern_bvs :: Monad under => UnevaledPattern -> EvalReader UnevaledADTArena UnevaledBoundValueArena type_var_arena module_child_maps under BoundValueList
pattern_bvs (SIR.Pattern'Identifier _ sp bvk) = bv_name bvk >>= \ name -> pure [(name, DeclAt sp, bvk)]
pattern_bvs (SIR.Pattern'Wildcard _ _) = pure []
pattern_bvs (SIR.Pattern'Tuple _ _ a b) = pattern_bvs a >>= \ a -> pattern_bvs b >>= \ b -> pure (a ++ b)
pattern_bvs (SIR.Pattern'Named _ _ _ (Located bv_span bvk) subpat) = bv_name bvk >>= \ name -> pattern_bvs subpat >>= \ subpat -> pure ((name, DeclAt bv_span, bvk) : subpat)
pattern_bvs (SIR.Pattern'AnonADTVariant _ _ _ _ fields) = concat <$> mapM pattern_bvs fields
pattern_bvs (SIR.Pattern'NamedADTVariant _ _ _ _ fields) = concat <$> mapM (pattern_bvs . snd) fields
pattern_bvs (SIR.Pattern'Poison _ _) = pure []

bv_name :: Monad under => SIR.BoundValueKey -> EvalReader UnevaledADTArena UnevaledBoundValueArena type_var_arena module_child_maps under Text
bv_name bvk =
    ask_bv_arena >>= \ bv_arena ->
    case Arena.get bv_arena bvk of
        SIR.BoundValue _ _ (Located _ name) -> pure name
        SIR.BoundValue'ADTVariant _ variant_index _ _ _ ->
            ask_adt_arena >>= \ adt_arena ->
            let variant = Type.get_adt_variant adt_arena variant_index
            in pure $ unlocate $ Type.variant_name variant

make_child_maps :: DeclChildrenList -> BoundValueList -> ADTVariantList -> CollectingErrors DeclMap
make_child_maps decls bound_values adt_variants =
    let decl_dups = find_dups decls
        bn_dups = find_dups bound_values
        variant_dups = find_dups adt_variants
    in report_dups decl_dups >> report_dups bn_dups >> report_dups variant_dups >>
    pure (make_map decls)
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

collect_child_maps :: UnevaledModuleArena -> UnevaledTypeSynonymArena -> (EvalReader UnevaledADTArena UnevaledBoundValueArena type_var_arena module_child_maps (MakeDeclState CollectingErrors)) (Arena.Arena DeclMap SIR.ModuleKey)
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

resolve_in_mods :: UnevaledModuleArena -> (EvalReader UnevaledADTArena UnevaledBoundValueArena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) (EvaledModuleArena, Map.Map Type.ADTKey DeclMap, Map.Map Type.TypeSynonymKey DeclMap)
resolve_in_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM resolve_in_module module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

resolve_in_adts :: Map.Map Type.ADTKey DeclMap -> UnevaledADTArena -> (EvalReader adt_arena bv_arena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledADTArena
resolve_in_adts adt_parent_child_maps adt_arena = Arena.transform_with_keyM (resolve_in_adt adt_parent_child_maps) adt_arena

resolve_in_type_synonyms :: Map.Map Type.TypeSynonymKey DeclMap -> UnevaledTypeSynonymArena -> (EvalReader adt_arena bv_arena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledTypeSynonymArena
resolve_in_type_synonyms synonym_parent_child_maps type_synonym_arena = Arena.transform_with_keyM (resolve_in_type_synonym synonym_parent_child_maps) type_synonym_arena

resolve_in_module :: SIR.ModuleKey -> UnevaledModule -> WriterT (Map Type.ADTKey DeclMap) (WriterT (Map Type.TypeSynonymKey DeclMap) (EvalReader UnevaledADTArena UnevaledBoundValueArena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors))) EvaledModule
resolve_in_module mod_key (SIR.Module id bindings adts type_synonyms) =
    lift (lift ask_module_child_maps) >>= \ module_child_maps ->
    let cur_map = Arena.get module_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt cur_map) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym cur_map) type_synonyms >>
    SIR.Module id <$> mapM (lift . lift . resolve_in_binding (DeclMapStack cur_map Nothing)) bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: Map.Map Type.ADTKey DeclMap -> Type.ADTKey -> UnevaledADT -> (EvalReader adt_arena bv_arena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledADT
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
    Type.ADT id name type_vars <$> mapM (resolve_in_variant (DeclMapStack new_nc (Just $ DeclMapStack parent Nothing))) variants
    where
        resolve_in_variant nc_stack (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, ty) -> (id, name, ) <$> resolve_in_type_expr nc_stack ty) fields
        resolve_in_variant nc_stack (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, ty) -> (id,) <$> resolve_in_type_expr nc_stack ty) fields

resolve_in_type_synonym :: Map.Map Type.TypeSynonymKey DeclMap -> Type.TypeSynonymKey -> UnevaledTypeSynonym -> (EvalReader adt_arena bv_arena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledTypeSynonym
resolve_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name expansion) =
    let parent = parent_maps Map.! synonym_key
    in Type.TypeSynonym id name <$> resolve_in_type_expr (DeclMapStack parent Nothing) expansion

resolve_in_binding :: DeclMapStack -> UnevaledBinding -> (EvalReader UnevaledADTArena UnevaledBoundValueArena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledBinding
resolve_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat nc_stack target <*> pure eq_sp <*> resolve_in_expr nc_stack expr
resolve_in_binding _ (SIR.Binding'ADTVariant bvk variant vars sp) = pure $ SIR.Binding'ADTVariant bvk variant vars sp

resolve_in_type_expr :: DeclMapStack -> UnevaledTypeExpr -> (EvalReader adt_arena bv_arena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledTypeExpr
resolve_in_type_expr nc_stack (SIR.TypeExpr'Identifier () sp iden) = do
    iden_resolved <- resolve_iden_in_monad resolve_type_iden nc_stack iden
    decls <- ask_decl_arena
    ty <- case iden_resolved of
        Just i -> case Arena.get decls i of
            SIR.Decl'Module _ -> lift (lift (Compiler.tell_error $ NotAType sp "a module")) >> pure Nothing
            SIR.Decl'Type ty -> pure $ Just ty
        Nothing -> pure Nothing
    pure (SIR.TypeExpr'Identifier ty sp iden_resolved)
    where
resolve_in_type_expr nc_stack (SIR.TypeExpr'Tuple () a b) =
    resolve_in_type_expr nc_stack a >>= \ a_conv ->
    resolve_in_type_expr nc_stack b >>= \ b_conv ->
    pure (SIR.TypeExpr'Tuple (Type.Type'Tuple <$> (SIR.type_expr_type_info a_conv) <*> (SIR.type_expr_type_info b_conv)) a_conv b_conv)
resolve_in_type_expr _ (SIR.TypeExpr'Hole () sp hid) = pure $ SIR.TypeExpr'Hole Nothing sp hid  -- TODO: also make this an unknown
resolve_in_type_expr nc_stack (SIR.TypeExpr'Function () sp arg res) =
    resolve_in_type_expr nc_stack arg >>= \ arg ->
    resolve_in_type_expr nc_stack res >>= \ res ->
    pure (SIR.TypeExpr'Function (Type.Type'Function <$> (SIR.type_expr_type_info arg) <*> (SIR.type_expr_type_info res)) sp arg res)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Forall () vars ty) =
    mapM
        (\ var ->
            ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ make_child_maps vars' [] []) >>= \ new_nc ->
    resolve_in_type_expr (DeclMapStack new_nc (Just nc_stack)) ty >>= \ ty ->
    pure (SIR.TypeExpr'Forall (SIR.type_expr_type_info ty) vars ty)
{- TODO (split-nr)
type_expr (SIR.TypeExpr'Apply () sp ty arg) =
    type_expr ty >>= \ ty ->
    type_expr arg >>= \ arg ->
    apply_type (TypeExpr sp) sp (SIR.type_expr_type_info ty) (SIR.type_expr_type_info arg) >>= \ result_ty ->
    pure (SIR.TypeExpr'Apply result_ty sp ty arg)
-}
resolve_in_type_expr nc_stack (SIR.TypeExpr'Apply () sp ty args) =
    SIR.TypeExpr'Apply todo sp <$> resolve_in_type_expr nc_stack ty <*> resolve_in_type_expr nc_stack args
resolve_in_type_expr _ (SIR.TypeExpr'Wild () sp) = pure $ SIR.TypeExpr'Wild Nothing sp -- TODO: make this an unknown to be inferred and not a Nothing
resolve_in_type_expr _ (SIR.TypeExpr'Poison () sp) = pure $ SIR.TypeExpr'Poison Nothing sp

resolve_in_pat :: DeclMapStack -> UnevaledPattern -> (EvalReader adt_arena bv_arena type_var_arena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledPattern
resolve_in_pat _ (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat nc_stack a <*> resolve_in_pat nc_stack b
resolve_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat nc_stack subpat
resolve_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_iden_in_monad resolve_pat_iden nc_stack variant <*> pure tyargs <*> mapM (resolve_in_pat nc_stack) subpat
resolve_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_iden_in_monad resolve_pat_iden nc_stack variant <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat nc_stack field_pat) subpat
resolve_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: DeclMapStack -> UnevaledExpr -> (EvalReader UnevaledADTArena UnevaledBoundValueArena TypeVarArena ModuleDeclMap (MakeDeclState CollectingErrors)) EvaledExpr
resolve_in_expr nc_stack (SIR.Expr'Identifier id type_info sp i) = SIR.Expr'Identifier id type_info sp <$> resolve_iden_in_monad resolve_expr_iden nc_stack i
resolve_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr nc_stack a <*> resolve_in_expr nc_stack b

resolve_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    pattern_bvs param >>= \ param_bvs ->
    lift (lift $ make_child_maps [] param_bvs []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat nc_stack param <*> resolve_in_expr (DeclMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'Let id type_info sp bindings body) =
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    (unzip3 <$> mapM binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    SIR.Expr'Let id type_info sp <$> mapM (resolve_in_binding nc_stack) bindings <*> resolve_in_expr (DeclMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'LetRec id type_info sp bindings body) =
    (unzip3 <$> mapM binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    let new_nc_stack = DeclMapStack new_nc (Just nc_stack)
    in SIR.Expr'LetRec id type_info sp <$> mapM (resolve_in_binding new_nc_stack) bindings <*> resolve_in_expr new_nc_stack body

resolve_in_expr nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) = SIR.Expr'BinaryOps id allowed type_info sp <$> resolve_in_expr nc_stack first <*> mapM (\ (iden, rhs) -> (,) <$> resolve_iden_in_monad resolve_expr_iden nc_stack iden <*> resolve_in_expr nc_stack rhs) ops

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
                    (pat',) <$> resolve_in_expr (DeclMapStack arm_nc (Just nc_stack)) expr
                )
                arms

resolve_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp ty e) = SIR.Expr'TypeAnnotation id type_info sp <$> resolve_in_type_expr nc_stack ty <*> resolve_in_expr nc_stack e

resolve_in_expr nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr (DeclMapStack new_nc (Just nc_stack)) e
resolve_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e args) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr nc_stack e <*> resolve_in_type_expr nc_stack args

resolve_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

resolve_iden_in_monad :: Monad under => (DeclArena -> ModuleDeclMap -> DeclMapStack -> iden -> under resolved) -> DeclMapStack -> iden -> EvalReader adt_arena bv_arena type_var_arena ModuleDeclMap (MakeDeclState under) resolved
resolve_iden_in_monad f nc_stack iden =
    lift get >>= \ decls ->
    ask_module_child_maps >>= \ mods ->
    lift (lift $ f decls mods nc_stack iden)

resolve_type_iden :: DeclArena -> ModuleDeclMap -> DeclMapStack -> UnevaledDIden -> CollectingErrors (Maybe SIR.DeclKey)
resolve_type_iden _ _ _ [] = error "empty identifier"
resolve_type_iden decls mods child_map_stack (first:more) =
    case resolve_first child_map_stack first of
        Right first_resolved ->
            case foldlM (get_decl_child decls mods) first_resolved more of
                Right r -> pure $ Just r
                Left e -> Compiler.tell_error e >> pure Nothing
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve_first (DeclMapStack d_children parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

-- TODO: factor out repeated code?
resolve_expr_iden :: DeclArena -> ModuleDeclMap -> DeclMapStack -> Located (Maybe [Located Text], Located Text) -> CollectingErrors EvaledVIden
resolve_expr_iden decls mods child_map_stack (Located sp (Just type_iden, last_segment)) =
    resolve_type_iden decls mods child_map_stack type_iden >>= \ resolved_type ->
    pure (Located sp (Just (maybe (Left ()) Right resolved_type), last_segment)) -- TODO: clean up this maybe call

resolve_expr_iden _ _ _ (Located sp (Nothing, last_segment)) = pure $ Located sp (Nothing, last_segment)

resolve_pat_iden :: DeclArena -> ModuleDeclMap -> DeclMapStack -> (Maybe [Located Text], Located Text) -> CollectingErrors EvaledPIden
resolve_pat_iden decls mods child_map_stack (Just type_iden, last_segment) =
    resolve_type_iden decls mods child_map_stack type_iden >>= \ resolved_type ->
    pure (Just (maybe (Left ()) Right resolved_type), last_segment) -- TODO: clean up this maybe call

resolve_pat_iden _ _ _ (Nothing, last_segment) = pure (Nothing, last_segment)

get_decl_child :: DeclArena -> ModuleDeclMap -> SIR.DeclKey -> Located Text -> Either Error SIR.DeclKey
get_decl_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let d_children = Arena.get mods m
                in Map.lookup (Located.unlocate name) d_children

            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
