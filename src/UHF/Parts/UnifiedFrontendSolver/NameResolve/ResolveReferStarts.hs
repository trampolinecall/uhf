module UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveReferStarts
    ( resolve
    , Unresolved
    , Resolved
    ) where

import UHF.Prelude

import UHF.Source.Located (Located (unlocate))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

type UnresolvedIdenStart = (NameMaps.NameMapStack, Located Text)

type ResolvedDIdenStart = Maybe (SIR.DeclRef TypeSolver.Type)
type ResolvedVIdenStart = Maybe SIR.ValueRef
type ResolvedPIdenStart = Maybe Type.ADT.VariantIndex

type Unresolved = (UnresolvedIdenStart, (), (), UnresolvedIdenStart, (), UnresolvedIdenStart, (), (), ())

type UnresolvedModuleArena = Arena.Arena (SIR.Module Unresolved) SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena (SIR.ADT Unresolved) Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Unresolved) Type.TypeSynonymKey

type Resolved = (ResolvedDIdenStart, (), (), ResolvedVIdenStart, (), ResolvedPIdenStart, (), (), ())

type ResolvedModuleArena = Arena.Arena (SIR.Module Resolved) SIR.ModuleKey
type ResolvedADTArena = Arena.Arena (SIR.ADT Resolved) Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena (SIR.TypeSynonym Resolved) Type.TypeSynonymKey

-- resolve entry point {{{1
resolve :: SIR.SIR Unresolved -> Error.WithErrors (SIR.SIR Resolved)
resolve (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    resolve_in_mods mods >>= \ mods ->
    resolve_in_adts adts >>= \ adts ->
    resolve_in_type_synonyms type_synonyms >>= \ synonyms ->
    pure (SIR.SIR mods adts synonyms type_vars (Arena.transform change_variable variables) (SIR.CU root_module main_function))
    where
        change_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

-- resolving through sir {{{1
resolve_in_mods :: UnresolvedModuleArena -> Error.WithErrors ResolvedModuleArena
resolve_in_mods = Arena.transformM resolve_in_module

resolve_in_adts :: UnresolvedADTArena -> Error.WithErrors ResolvedADTArena
resolve_in_adts = Arena.transformM resolve_in_adt

resolve_in_type_synonyms :: UnresolvedTypeSynonymArena -> Error.WithErrors ResolvedTypeSynonymArena
resolve_in_type_synonyms = Arena.transformM resolve_in_type_synonym

resolve_in_module :: SIR.Module Unresolved -> Error.WithErrors (SIR.Module Resolved)
resolve_in_module (SIR.Module id bindings adts type_synonyms) = SIR.Module id <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms

resolve_in_adt :: SIR.ADT Unresolved -> Error.WithErrors (SIR.ADT Resolved)
resolve_in_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM resolve_in_variant variants
    where
        resolve_in_variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id <$> mapM (\ (id, name, (ty, ())) -> resolve_in_type_expr ty >>= \ ty -> pure (id, name, (ty, ()))) fields
        resolve_in_variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id <$> mapM (\ (id, (ty, ())) -> resolve_in_type_expr ty >>= \ ty -> pure (id, (ty, ()))) fields

resolve_in_type_synonym :: SIR.TypeSynonym Unresolved -> Error.WithErrors (SIR.TypeSynonym Resolved)
resolve_in_type_synonym (Type.TypeSynonym id name (expansion, ())) =
    resolve_in_type_expr expansion >>= \ expansion ->
    pure (Type.TypeSynonym id name (expansion, ()))

resolve_in_binding :: SIR.Binding Unresolved -> Error.WithErrors (SIR.Binding Resolved)
resolve_in_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> resolve_in_pat target <*> pure eq_sp <*> resolve_in_expr expr

resolve_in_type_expr :: SIR.TypeExpr Unresolved -> Error.WithErrors (SIR.TypeExpr Resolved)
resolve_in_type_expr (SIR.TypeExpr'Refer resolved sp id) = SIR.TypeExpr'Refer resolved sp <$> resolve_type_iden id
resolve_in_type_expr (SIR.TypeExpr'Get resolved sp parent name) = SIR.TypeExpr'Get resolved sp <$> resolve_in_type_expr parent <*> pure name
resolve_in_type_expr (SIR.TypeExpr'Tuple resolved sp a b) = SIR.TypeExpr'Tuple resolved sp <$> resolve_in_type_expr a <*> resolve_in_type_expr b
resolve_in_type_expr (SIR.TypeExpr'Hole resolved type_info sp hid) = pure $ SIR.TypeExpr'Hole resolved type_info sp hid
resolve_in_type_expr (SIR.TypeExpr'Function resolved sp arg res) = SIR.TypeExpr'Function resolved sp <$> resolve_in_type_expr arg <*> resolve_in_type_expr res
resolve_in_type_expr (SIR.TypeExpr'Forall resolved sp vars ty) = SIR.TypeExpr'Forall resolved sp vars <$> resolve_in_type_expr ty
resolve_in_type_expr (SIR.TypeExpr'Apply resolved sp ty args) = SIR.TypeExpr'Apply resolved sp <$> resolve_in_type_expr ty <*> resolve_in_type_expr args
resolve_in_type_expr (SIR.TypeExpr'Wild resolved sp) = pure $ SIR.TypeExpr'Wild resolved sp
resolve_in_type_expr (SIR.TypeExpr'Poison resolved sp) = pure $ SIR.TypeExpr'Poison resolved sp

resolve_in_pat :: SIR.Pattern Unresolved -> Error.WithErrors (SIR.Pattern Resolved)
resolve_in_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
resolve_in_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
resolve_in_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> resolve_in_pat a <*> resolve_in_pat b
resolve_in_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM resolve_in_pat subpat
resolve_in_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden_split variant_resolved tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> resolve_split_iden resolve_pat_iden variant_iden_split <*> pure variant_resolved <*> pure tyargs <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> resolve_in_pat field_pat) subpat
resolve_in_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

resolve_in_expr :: SIR.Expr Unresolved -> Error.WithErrors (SIR.Expr Resolved)
resolve_in_expr (SIR.Expr'Refer id type_info sp iden_split ()) = SIR.Expr'Refer id type_info sp <$> resolve_split_iden resolve_expr_iden iden_split <*> pure ()
resolve_in_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
resolve_in_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
resolve_in_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
resolve_in_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
resolve_in_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

resolve_in_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> resolve_in_expr a <*> resolve_in_expr b

resolve_in_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> resolve_in_pat param <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'Let id type_info sp bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'LetRec id type_info sp bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp <$> mapM resolve_in_binding bindings <*> pure adts <*> pure type_synonyms <*> resolve_in_expr body

resolve_in_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> resolve_in_expr first
        <*> mapM
            (\ (sp, iden, (), rhs) ->
                (sp,,(),)
                    <$> resolve_split_iden resolve_expr_iden iden
                    <*> resolve_in_expr rhs)
            ops

resolve_in_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> resolve_in_expr callee <*> resolve_in_expr arg

resolve_in_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> resolve_in_expr cond <*> resolve_in_expr t <*> resolve_in_expr f
resolve_in_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> resolve_in_expr e
        <*> mapM (\ (pat, expr) -> (,) <$> resolve_in_pat pat <*> resolve_in_expr expr) arms

resolve_in_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, tye_ty) e) = SIR.Expr'TypeAnnotation id type_info sp <$> ((,tye_ty) <$> resolve_in_type_expr ty) <*> resolve_in_expr e

resolve_in_expr (SIR.Expr'Forall id type_info sp vars e) =
    SIR.Expr'Forall id type_info sp vars <$> resolve_in_expr e
resolve_in_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_ty)) = SIR.Expr'TypeApply id type_info sp <$> resolve_in_expr e <*> ((, arg_ty) <$> resolve_in_type_expr arg)

resolve_in_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

resolve_in_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

-- resolving identifiers {{{1
resolve_split_iden :: (UnresolvedIdenStart -> Error.WithErrors resolved_iden) -> SIR.SplitIdentifier UnresolvedIdenStart Unresolved -> Error.WithErrors (SIR.SplitIdentifier resolved_iden Resolved)
resolve_split_iden _ (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get <$> resolve_in_type_expr texpr <*> pure next
resolve_split_iden resolve_single (SIR.SplitIdentifier'Single s) = SIR.SplitIdentifier'Single <$> resolve_single s
resolve_iden_start :: (NameMaps.NameMaps -> Map Text resolved) -> UnresolvedIdenStart -> Error.WithErrors (Maybe resolved)
resolve_iden_start which_map (name_map_stack, iden) =
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
resolve_type_iden :: UnresolvedIdenStart -> Error.WithErrors ResolvedDIdenStart
resolve_type_iden = resolve_iden_start (\ (NameMaps.NameMaps d_children _ _) -> d_children)
resolve_expr_iden :: UnresolvedIdenStart -> Error.WithErrors ResolvedVIdenStart
resolve_expr_iden = resolve_iden_start (\ (NameMaps.NameMaps _ var_children _) -> var_children)
resolve_pat_iden :: UnresolvedIdenStart -> Error.WithErrors ResolvedPIdenStart
resolve_pat_iden = resolve_iden_start (\ (NameMaps.NameMaps _ _ adtv_children) -> adtv_children)
