module UHF.Phases.NameResolve.ResolveReferStarts
    ( resolve
    , Unresolved
    , Resolved
    ) where

import UHF.Prelude

import UHF.Source.Located (Located (Located, unlocate))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.TypeSolver as TypeSolver
import qualified UHF.Phases.NameResolve.DeclAt as DeclAt
import qualified UHF.Phases.NameResolve.Error as Error
import qualified UHF.Phases.NameResolve.NRReader as NRReader
import qualified UHF.Phases.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

-- TODO: figure out a better solution than to have adt_parents and type_synonym_parents

type QuantVarArena = Arena.Arena Type.QuantVar Type.QuantVarKey

type UnresolvedIdenStart = Located Text

type ResolvedDIdenStart = Maybe (SIR.Decl TypeSolver.Type)
type ResolvedVIdenStart = Maybe SIR.VariableKey
type ResolvedPIdenStart = Maybe Type.ADT.VariantIndex

type Unresolved = (UnresolvedIdenStart, (), (), UnresolvedIdenStart, (), UnresolvedIdenStart, (), (), ())

type UnresolvedADT = Type.ADT (SIR.TypeExpr Unresolved, ())
type UnresolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Unresolved, ())

type UnresolvedModuleArena = Arena.Arena (SIR.Module Unresolved) SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey
type UnresolvedVariableArena = Arena.Arena (SIR.Variable Unresolved) SIR.VariableKey

type Resolved = (ResolvedDIdenStart, (), (), ResolvedVIdenStart, (), ResolvedPIdenStart, (), (), ())

type ResolvedADT = Type.ADT (SIR.TypeExpr Resolved, ())
type ResolvedTypeSynonym = Type.TypeSynonym (SIR.TypeExpr Resolved, ())

type ResolvedModuleArena = Arena.Arena (SIR.Module Resolved) SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

-- resolve entry point {{{1
resolve :: NameMaps.SIRChildMaps -> SIR.SIR Unresolved -> Error.WithErrors (SIR.SIR Resolved)
resolve sir_child_maps (SIR.SIR mods adts type_synonyms type_vars variables mod) =
    runReaderT (resolve_in_mods mods) (adts, variables, type_vars, sir_child_maps) >>= \ (mods, adt_parents, type_synonym_parents) ->
    runReaderT (resolve_in_adts adt_parents adts) ((), (), type_vars, sir_child_maps) >>= \ adts ->
    runReaderT (resolve_in_type_synonyms type_synonym_parents type_synonyms) ((), (), type_vars, sir_child_maps) >>= \ synonyms ->
    pure (SIR.SIR mods adts synonyms type_vars (Arena.transform change_variable variables) mod)
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n
        change_variable (SIR.Variable'ADTVariant varid id tyvars tyinfo sp) = SIR.Variable'ADTVariant varid id tyvars tyinfo sp

-- resolving through sir {{{1
resolve_in_mods :: UnresolvedModuleArena -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (ResolvedModuleArena, Map.Map Type.ADTKey NameMaps.NameMapStack, Map.Map Type.TypeSynonymKey NameMaps.NameMapStack)
resolve_in_mods module_arena =
    runWriterT (runWriterT $ Arena.transform_with_keyM resolve_in_module module_arena) >>= \ ((module_arena, adt_parents), type_synonym_parents) ->
    pure (module_arena, adt_parents, type_synonym_parents)

resolve_in_adts :: Map.Map Type.ADTKey NameMaps.NameMapStack -> UnresolvedADTArena -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) ResolvedADTArena
resolve_in_adts adt_parent_name_maps = Arena.transform_with_keyM (resolve_in_adt adt_parent_name_maps)

resolve_in_type_synonyms :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> UnresolvedTypeSynonymArena -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) ResolvedTypeSynonymArena
resolve_in_type_synonyms synonym_parent_name_maps = Arena.transform_with_keyM (resolve_in_type_synonym synonym_parent_name_maps)

resolve_in_module :: SIR.ModuleKey -> SIR.Module Unresolved -> WriterT (Map Type.ADTKey NameMaps.NameMapStack) (WriterT (Map Type.TypeSynonymKey NameMaps.NameMapStack) (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors)) (SIR.Module Resolved)
resolve_in_module mod_key (SIR.Module id bindings adts type_synonyms) =
    lift (lift NRReader.ask_sir_child_maps) >>= \ sir_child_maps ->
    let cur_map = NameMaps.get_module_child_maps sir_child_maps mod_key
    in mapM_ (\ adt -> tell $ Map.singleton adt (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) adts >>
    mapM (\ synonym -> lift $ tell $ Map.singleton synonym (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) type_synonyms >>
    SIR.Module id <$> mapM (lift . lift . resolve_in_binding (NameMaps.NameMapStack (NameMaps.child_maps_to_name_maps cur_map) Nothing)) bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: Map.Map Type.ADTKey NameMaps.NameMapStack -> Type.ADTKey -> UnresolvedADT -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) ResolvedADT
resolve_in_adt adt_parent_name_maps adt_key (Type.ADT id name type_vars variants) =
    let parent = adt_parent_name_maps Map.! adt_key
    in
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        type_vars >>= \ type_vars' ->
    lift (NameMaps.make_name_maps type_vars' [] []) >>= \ new_nc ->
    Type.ADT id name type_vars <$> mapM (resolve_in_variant (NameMaps.NameMapStack new_nc (Just parent))) variants
    where
        resolve_in_variant nc_stack (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> resolve_in_type_expr nc_stack ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        resolve_in_variant nc_stack (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> resolve_in_type_expr nc_stack ty >>= \ ty -> pure (id, (ty, ()))) fields

resolve_in_type_synonym :: Map.Map Type.TypeSynonymKey NameMaps.NameMapStack -> Type.TypeSynonymKey -> UnresolvedTypeSynonym -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) ResolvedTypeSynonym
resolve_in_type_synonym parent_maps synonym_key (Type.TypeSynonym id name (expansion, ())) =
    let parent = parent_maps Map.! synonym_key
    in resolve_in_type_expr parent expansion >>= \ expansion ->
    pure (Type.TypeSynonym id name (expansion, ()))

resolve_in_binding :: NameMaps.NameMapStack -> SIR.Binding Unresolved -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Binding Resolved)
resolve_in_binding nc_stack (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat nc_stack target <*> pure eq_sp <*> resolve_in_expr nc_stack expr
resolve_in_binding _ (SIR.Binding'ADTVariant var_key variant vars sp) = pure $ SIR.Binding'ADTVariant var_key variant vars sp

resolve_in_type_expr :: NameMaps.NameMapStack -> SIR.TypeExpr Unresolved -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.TypeExpr Resolved)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Refer resolved sp id) = SIR.TypeExpr'Refer resolved sp <$> lift (resolve_type_iden nc_stack id)
resolve_in_type_expr nc_stack (SIR.TypeExpr'Get resolved sp parent name) = SIR.TypeExpr'Get resolved sp <$> resolve_in_type_expr nc_stack parent <*> pure name
resolve_in_type_expr nc_stack (SIR.TypeExpr'Tuple resolved sp a b) = SIR.TypeExpr'Tuple resolved sp <$> resolve_in_type_expr nc_stack a <*> resolve_in_type_expr nc_stack b
resolve_in_type_expr _ (SIR.TypeExpr'Hole resolved type_info sp hid) = pure $ SIR.TypeExpr'Hole resolved type_info sp hid
resolve_in_type_expr nc_stack (SIR.TypeExpr'Function resolved sp arg res) = SIR.TypeExpr'Function resolved sp <$> resolve_in_type_expr nc_stack arg <*> resolve_in_type_expr nc_stack res
resolve_in_type_expr nc_stack (SIR.TypeExpr'Forall resolved sp vars ty) =
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    SIR.TypeExpr'Forall resolved sp vars <$> resolve_in_type_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) ty
resolve_in_type_expr nc_stack (SIR.TypeExpr'Apply resolved sp ty args) = SIR.TypeExpr'Apply resolved sp <$> resolve_in_type_expr nc_stack ty <*> resolve_in_type_expr nc_stack args
resolve_in_type_expr _ (SIR.TypeExpr'Wild resolved sp) = pure $ SIR.TypeExpr'Wild resolved sp
resolve_in_type_expr _ (SIR.TypeExpr'Poison resolved sp) = pure $ SIR.TypeExpr'Poison resolved sp

resolve_in_pat :: NameMaps.NameMapStack -> SIR.Pattern Unresolved -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Pattern Resolved)
resolve_in_pat _ (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
resolve_in_pat _ (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat nc_stack (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat nc_stack a <*> resolve_in_pat nc_stack b
resolve_in_pat nc_stack (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat nc_stack subpat
resolve_in_pat nc_stack (SIR.Pattern'AnonADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (resolve_in_pat nc_stack) subpat
resolve_in_pat nc_stack (SIR.Pattern'NamedADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden nc_stack variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat nc_stack field_pat) subpat
resolve_in_pat _ (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: NameMaps.NameMapStack -> SIR.Expr Unresolved -> (NRReader.NRReader UnresolvedADTArena UnresolvedVariableArena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.Expr Resolved)
resolve_in_expr nc_stack (SIR.Expr'Identifier id type_info sp iden_split ()) = SIR.Expr'Identifier id type_info sp <$> resolve_split_iden resolve_expr_iden nc_stack iden_split <*> pure ()
resolve_in_expr _ (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr _ (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr _ (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr _ (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr _ (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr nc_stack (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr nc_stack a <*> resolve_in_expr nc_stack b

resolve_in_expr nc_stack (SIR.Expr'Lambda id type_info sp param body) =
    NameMaps.pattern_vars param >>= \ param_vars ->
    lift (NameMaps.make_name_maps [] param_vars []) >>= \ new_nc ->
    SIR.Expr'Lambda id type_info sp <$> resolve_in_pat nc_stack param <*> resolve_in_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'Let id type_info sp bindings body) =
    -- do not need to do binding by binding because the ToSIR should have already desugared that into a sequence of lets
    -- so this let should only have 1 or 0 bindings
    NameMaps.make_name_maps_from_decls todo bindings [] [] >>= \ new_nm ->
    SIR.Expr'Let id type_info sp <$> mapM (resolve_in_binding nc_stack) bindings <*> resolve_in_expr (NameMaps.NameMapStack new_nm (Just nc_stack)) body

resolve_in_expr nc_stack (SIR.Expr'LetRec id type_info sp bindings body) =
    NameMaps.make_name_maps_from_decls todo bindings [] [] >>= \ new_nm ->
    let new_nc_stack = NameMaps.NameMapStack new_nm (Just nc_stack)
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
                    NameMaps.pattern_vars pat >>= \ var_children ->
                    lift (NameMaps.make_name_maps [] var_children []) >>= \ arm_nc ->
                    (pat',) <$> resolve_in_expr (NameMaps.NameMapStack arm_nc (Just nc_stack)) expr
                )
                arms

resolve_in_expr nc_stack (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> resolve_in_type_expr nc_stack ty) <*> resolve_in_expr nc_stack e

resolve_in_expr nc_stack (SIR.Expr'Forall id type_info sp vars e) =
    mapM
        (\ var ->
            NRReader.ask_quant_var_arena >>= \ quant_var_arena ->
            let (Type.QuantVar (Located name_sp name)) = Arena.get quant_var_arena var
            in pure (name, DeclAt.DeclAt name_sp, SIR.Decl'Type $ TypeSolver.Type'QuantVar var))
        (toList vars) >>= \ vars' ->
    lift (NameMaps.make_name_maps vars' [] []) >>= \ new_nc ->
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr (NameMaps.NameMapStack new_nc (Just nc_stack)) e
resolve_in_expr nc_stack (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr nc_stack e <*> ((, arg_ty) <$> resolve_in_type_expr nc_stack arg)

resolve_in_expr _ (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr _ (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: (NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors resolved_iden) -> NameMaps.NameMapStack -> SIR.SplitIdentifier Unresolved UnresolvedIdenStart -> (NRReader.NRReader adt_arena var_arena QuantVarArena NameMaps.SIRChildMaps Error.WithErrors) (SIR.SplitIdentifier Resolved resolved_iden)
resolve_split_iden _ name_map_stack (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> resolve_in_type_expr name_map_stack texpr <*> pure next
resolve_split_iden resolve_start name_map_stack (SIR.SplitIdentifier'Single i) = SIR.SplitIdentifier'Single <$> lift (resolve_start name_map_stack i)

resolve_iden_start :: (NameMaps.NameMaps -> Map Text resolved) -> NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors (Maybe resolved)
resolve_iden_start which_map name_map_stack iden =
    case go name_map_stack of
        Right resolved -> pure $ Just resolved
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        go (NameMaps.NameMapStack name_maps parent) =
            case Map.lookup (unlocate iden) (which_map name_maps) of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> go parent
                        Nothing -> Left $ Error.Error'CouldNotFind iden

resolve_type_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedDIdenStart
resolve_type_iden = resolve_iden_start (\ (NameMaps.NameMaps d_children _ _) -> d_children)

resolve_expr_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedVIdenStart
resolve_expr_iden = resolve_iden_start (\ (NameMaps.NameMaps _ var_children _) -> var_children)

resolve_pat_iden :: NameMaps.NameMapStack -> UnresolvedIdenStart -> Error.WithErrors ResolvedPIdenStart
resolve_pat_iden = resolve_iden_start (\ (NameMaps.NameMaps _ _ adtv_children) -> adtv_children)
