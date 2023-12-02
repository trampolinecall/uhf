module UHF.Phases.ResolveReferStarts
    ( resolve
    ) where

import UHF.Util.Prelude

import UHF.IO.Located (Located (Located, unlocate))
import qualified Arena
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.NameResolve.Utils as Utils

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type TypeVarArena = Arena.Arena Type.Var Type.TypeVarKey

type UnresolvedIdenStart = Located Text

type ResolvedDIdenStart = Maybe SIR.DeclKey
type ResolvedVIdenStart = Maybe SIR.BoundValueKey
type ResolvedPIdenStart = Maybe Type.ADTVariantIndex

type Unresolved = (UnresolvedIdenStart, (), (), UnresolvedIdenStart, (), UnresolvedIdenStart, (), (), ())

type UnresolvedADT = Type.ADT (SIR.TypeExpr Unresolved, ())
type UnresolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Unresolved, ())

type UnresolvedModuleArena = Arena.Arena (SIR.Module Unresolved) SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey
type UnresolvedBoundValueArena = Arena.Arena (SIR.BoundValue Unresolved) SIR.BoundValueKey

type Resolved = (ResolvedDIdenStart, (), (), ResolvedVIdenStart, (), ResolvedPIdenStart, (), (), ())

type ResolvedADT = Type.ADT (SIR.TypeExpr Resolved, ())
type ResolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Resolved, ())

type ResolvedModuleArena = Arena.Arena (SIR.Module Resolved) SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

-- resolve entry point {{{1
resolve :: SIR.SIR Unresolved -> Utils.CollectingErrors (SIR.SIR Resolved)
resolve (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    runStateT
        (
            runReaderT (Utils.collect_child_maps mods type_synonyms) (adts, bound_values, (), ()) >>= \ child_maps ->
            runReaderT (resolve_in_mods mods) (adts, bound_values, type_vars, child_maps) >>= \ (mods, adt_parents, type_synonym_parents) ->
            runReaderT (resolve_in_adts adt_parents adts) ((), (), type_vars, child_maps) >>= \ adts ->
            runReaderT (resolve_in_type_synonyms type_synonym_parents type_synonyms) ((), (), type_vars, child_maps) >>= \ synonyms ->
            pure (mods, adts, synonyms)
        )
        decls >>= \ ((mods, adts, synonyms), decls) ->
    pure (SIR.SIR decls mods adts synonyms type_vars (Arena.transform change_bound_value bound_values) mod)
    where
        change_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        change_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

-- resolving through sir {{{1
resolve_in_mods :: UnresolvedModuleArena -> (Utils.NRReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (ResolvedModuleArena, Map.Map Type.ADTKey Utils.ChildMaps, Map.Map Type.TypeSynonymKey Utils.ChildMaps)
resolve_in_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM resolve_in_module module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

resolve_in_adts :: Map.Map Type.ADTKey Utils.ChildMaps -> UnresolvedADTArena -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) ResolvedADTArena
resolve_in_adts adt_parent_child_maps = Arena.transform_with_keyM (resolve_in_adt adt_parent_child_maps)

resolve_in_type_synonyms :: Map.Map Type.TypeSynonymKey Utils.ChildMaps -> UnresolvedTypeSynonymArena -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) ResolvedTypeSynonymArena
resolve_in_type_synonyms synonym_parent_child_maps = Arena.transform_with_keyM (resolve_in_type_synonym synonym_parent_child_maps)

resolve_in_module :: SIR.ModuleKey -> SIR.Module Unresolved -> WriterT (Map Type.ADTKey Utils.ChildMaps) (WriterT (Map Type.TypeSynonymKey Utils.ChildMaps) (Utils.NRReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors))) (SIR.Module Resolved)
resolve_in_module mod_key (SIR.Module id bindings adts type_synonyms) =
    lift (lift Utils.ask_module_child_maps) >>= \ module_child_maps ->
    let cur_map = Arena.get module_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt cur_map) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym cur_map) type_synonyms >>
    SIR.Module id <$> mapM (lift . lift . resolve_in_binding (Utils.ChildMapStack cur_map Nothing)) bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: Map.Map Type.ADTKey Utils.ChildMaps -> Type.ADTKey -> UnresolvedADT -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) ResolvedADT
resolve_in_adt adt_parent_child_maps adt_key (Type.ADT id name type_vars variants) =
    let parent = adt_parent_child_maps Map.! adt_key
    in mapM
        (\ var ->
            Utils.ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (Utils.new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, Utils.DeclAt name_sp, var_decl))
        type_vars >>= \ type_vars' ->
    lift (lift $ Utils.make_child_maps type_vars' [] []) >>= \ new_nc ->
    Type.ADT id name type_vars <$> mapM (resolve_in_variant (Utils.ChildMapStack new_nc (Just $ Utils.ChildMapStack parent Nothing))) variants
    where
        resolve_in_variant nc_stack (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id <$> mapM (\ (id, name, (ty, ())) -> resolve_in_type_expr nc_stack ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        resolve_in_variant nc_stack (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id <$> mapM (\ (id, (ty, ())) -> resolve_in_type_expr nc_stack ty >>= \ ty -> pure (id, (ty, ()))) fields

resolve_in_type_synonym :: Map.Map Type.TypeSynonymKey Utils.ChildMaps -> Type.TypeSynonymKey -> UnresolvedTypeSynonym -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) ResolvedTypeSynonym
resolve_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, ())) =
    let parent = parent_maps Map.! synonym_key
    in resolve_in_type_expr (Utils.ChildMapStack parent Nothing) expansion >>= \ expansion ->
    pure (Type.TypeSynonym id name (expansion, ()))

resolve_in_binding :: Utils.ChildMapStack -> SIR.Binding Unresolved -> (Utils.NRReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (SIR.Binding Resolved)
resolve_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat nc_stack target <*> pure eq_sp <*> resolve_in_expr nc_stack expr
resolve_in_binding _ (SIR.Binding'ADTVariant bvk variant vars sp) = pure $ SIR.Binding'ADTVariant bvk variant vars sp

resolve_in_type_expr :: Utils.ChildMapStack -> SIR.TypeExpr Unresolved -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (SIR.TypeExpr Resolved)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Refer resolved sp id) = SIR.TypeExpr'Refer resolved sp <$> lift (lift $ resolve_type_iden nc_stack id)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Get resolved sp parent name) = SIR.TypeExpr'Get resolved sp <$> resolve_in_type_expr nc_stack parent <*> pure name
resolve_in_type_expr nc_stack (SIR.TypeExpr'Tuple resolved sp a b) = SIR.TypeExpr'Tuple resolved sp <$> resolve_in_type_expr nc_stack a <*> resolve_in_type_expr nc_stack b
resolve_in_type_expr _ (SIR.TypeExpr'Hole resolved type_info sp hid) = pure $ SIR.TypeExpr'Hole resolved type_info sp hid
resolve_in_type_expr nc_stack (SIR.TypeExpr'Function resolved sp arg res) = SIR.TypeExpr'Function resolved sp <$> resolve_in_type_expr nc_stack arg <*> resolve_in_type_expr nc_stack res
resolve_in_type_expr nc_stack (SIR.TypeExpr'Forall resolved sp vars ty) =
    mapM
        (\ var ->
            Utils.ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (Utils.new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, Utils.DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ Utils.make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.TypeExpr'Forall resolved sp vars <$> resolve_in_type_expr (Utils.ChildMapStack new_nc (Just nc_stack)) ty
resolve_in_type_expr nc_stack (SIR.TypeExpr'Apply resolved sp ty args) = SIR.TypeExpr'Apply resolved sp <$> resolve_in_type_expr nc_stack ty <*> resolve_in_type_expr nc_stack args
resolve_in_type_expr _ (SIR.TypeExpr'Wild resolved sp) = pure $ SIR.TypeExpr'Wild resolved sp
resolve_in_type_expr _ (SIR.TypeExpr'Poison resolved sp) = pure $ SIR.TypeExpr'Poison resolved sp

resolve_in_pat :: Utils.ChildMapStack -> SIR.Pattern Unresolved -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (SIR.Pattern Resolved)
resolve_in_pat _ (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat nc_stack a <*> resolve_in_pat nc_stack b
resolve_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat nc_stack subpat
resolve_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (resolve_in_pat nc_stack) subpat
resolve_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat nc_stack field_pat) subpat
resolve_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: Utils.ChildMapStack -> SIR.Expr Unresolved -> (Utils.NRReader UnresolvedADTArena UnresolvedBoundValueArena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (SIR.Expr Resolved)
resolve_in_expr nc_stack (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> resolve_split_iden resolve_expr_iden nc_stack iden_split <*> pure ()
resolve_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr nc_stack a <*> resolve_in_expr nc_stack b

resolve_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    Utils.pattern_bvs param >>= \ param_bvs ->
    lift (lift $ Utils.make_child_maps [] param_bvs []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat nc_stack param <*> resolve_in_expr (Utils.ChildMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'Let id type_info sp bindings body) =
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    (unzip3 <$> mapM Utils.binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ Utils.make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    SIR.Expr'Let id type_info sp <$> mapM (resolve_in_binding nc_stack) bindings <*> resolve_in_expr (Utils.ChildMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'LetRec id type_info sp bindings body) =
    (unzip3 <$> mapM Utils.binding_children bindings) >>= \ (decl_children, bv_children, variant_children) ->
    lift (lift $ Utils.make_child_maps (concat decl_children) (concat bv_children) (concat variant_children)) >>= \ new_nc ->
    let new_nc_stack = Utils.ChildMapStack new_nc (Just nc_stack)
    in SIR.Expr'LetRec id type_info sp <$> mapM (resolve_in_binding new_nc_stack) bindings <*> resolve_in_expr new_nc_stack body

resolve_in_expr nc_stack (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr nc_stack first
        <*> mapM
            (\ (sp, iden, (), rhs) ->
                (sp,,(),)
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
                    Utils.pattern_bvs pat >>= \ bv_children ->
                    lift (lift $ Utils.make_child_maps [] bv_children []) >>= \ arm_nc ->
                    (pat',) <$> resolve_in_expr (Utils.ChildMapStack arm_nc (Just nc_stack)) expr
                )
                arms

resolve_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> resolve_in_type_expr nc_stack ty) <*> resolve_in_expr nc_stack e

resolve_in_expr nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            Utils.ask_type_var_arena >>= \ type_var_arena ->
            let (Type.Var (Located name_sp name)) = Arena.get type_var_arena var
            in lift (Utils.new_decl (SIR.Decl'Type $ Type.Type'Variable var)) >>= \ var_decl ->
            pure (name, Utils.DeclAt name_sp, var_decl))
        (toList vars) >>= \ vars' ->
    lift (lift $ Utils.make_child_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr (Utils.ChildMapStack new_nc (Just nc_stack)) e
resolve_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr nc_stack e <*> ((, arg_ty) <$> resolve_in_type_expr nc_stack arg)

resolve_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: (Utils.ChildMapStack -> UnresolvedIdenStart -> Utils.CollectingErrors resolved_iden) -> Utils.ChildMapStack -> SIR.SplitIdentifier Unresolved UnresolvedIdenStart -> (Utils.NRReader adt_arena bv_arena TypeVarArena Utils.ModuleChildMaps (Utils.MakeDeclState Utils.CollectingErrors)) (SIR.SplitIdentifier Resolved resolved_iden)
resolve_split_iden _ child_map_stack (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> resolve_in_type_expr child_map_stack texpr <*> pure next
resolve_split_iden resolve_start child_map_stack (SIR.SplitIdentifier'Single i) = SIR.SplitIdentifier'Single <$> lift (lift $ resolve_start child_map_stack i)

resolve_iden_start :: (Utils.ChildMaps -> Map Text resolved) -> Utils.ChildMapStack -> UnresolvedIdenStart -> Utils.CollectingErrors (Maybe resolved)
resolve_iden_start which_map child_map_stack iden =
    case go child_map_stack of
        Right resolved -> pure $ Just resolved
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        go (Utils.ChildMapStack child_maps parent) =
            case Map.lookup (unlocate iden) (which_map child_maps) of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Left $ Utils.Error'CouldNotFind iden

resolve_type_iden :: Utils.ChildMapStack -> UnresolvedIdenStart -> Utils.CollectingErrors ResolvedDIdenStart
resolve_type_iden = resolve_iden_start (\ (Utils.ChildMaps d_children _ _) -> d_children)

resolve_expr_iden :: Utils.ChildMapStack -> UnresolvedIdenStart -> Utils.CollectingErrors ResolvedVIdenStart
resolve_expr_iden = resolve_iden_start (\ (Utils.ChildMaps _ bv_children _) -> bv_children)

resolve_pat_iden :: Utils.ChildMapStack -> UnresolvedIdenStart -> Utils.CollectingErrors ResolvedPIdenStart
resolve_pat_iden = resolve_iden_start (\ (Utils.ChildMaps _ _ adtv_children) -> adtv_children)
