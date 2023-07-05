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

    , UnresolvedDeclArena
    , ResolvedDeclArena

    , resolve
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located (Located))
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity, runIdentity))

type UnresolvedDIden = (SIR.NameContext, [Located Text])
type UnresolvedVIden = (SIR.NameContext, [Located Text])
type UnresolvedPIden = (SIR.NameContext, [Located Text])

type UnresolvedSIR = SIR.SIR UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedDecl = SIR.Decl
type UnresolvedModule = SIR.Module UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedADT = Type.ADT UnresolvedTypeExpr
type UnresolvedTypeSynonym = Type.TypeSynonym UnresolvedTypeExpr
type UnresolvedTypeExpr = SIR.TypeExpr UnresolvedDIden ()
type UnresolvedBinding = SIR.Binding UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedExpr = SIR.Expr UnresolvedDIden UnresolvedVIden UnresolvedPIden () ()
type UnresolvedPattern = SIR.Pattern UnresolvedPIden ()

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl SIR.DeclKey
type UnresolvedModuleArena = Arena.Arena UnresolvedModule SIR.ModuleKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT Type.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey

type ResolvedDIden = Maybe SIR.DeclKey
type ResolvedVIden = Located (Maybe SIR.BoundValueKey)
type ResolvedPIden = Maybe Type.ADTVariantIndex

type ResolvedSIR = SIR.SIR ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedDecl = SIR.Decl
type ResolvedModule = SIR.Module ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedADT = Type.ADT ResolvedTypeExpr
type ResolvedTypeSynonym = Type.TypeSynonym ResolvedTypeExpr
type ResolvedTypeExpr = SIR.TypeExpr ResolvedDIden ()
type ResolvedBinding = SIR.Binding ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedExpr = SIR.Expr ResolvedDIden ResolvedVIden ResolvedPIden () ()
type ResolvedPattern = SIR.Pattern ResolvedPIden ()

type ResolvedDeclArena = Arena.Arena ResolvedDecl SIR.DeclKey
type ResolvedModuleArena = Arena.Arena ResolvedModule SIR.ModuleKey
type ResolvedADTArena = Arena.Arena ResolvedADT Type.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym Type.TypeSynonymKey

data Error
    = CouldNotFind (Maybe (Located Text)) (Located Text)

instance Diagnostic.ToError Error where
    to_error (CouldNotFind prev (Located sp name)) =
        let message =
                "could not find name '" <> name <> "'"
                    <> case prev of
                        Just (Located _ prev_name) -> "in '" <> prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name (Just sp) message [] []

transform_identifiers ::
    Monad m =>
    (d_iden -> m d_iden') ->
    (v_iden -> m v_iden') ->
    (p_iden -> m p_iden') ->
    Arena.Arena (Type.ADT (SIR.TypeExpr d_iden type_info)) Type.ADTKey ->
    Arena.Arena (Type.TypeSynonym (SIR.TypeExpr d_iden type_info)) Type.TypeSynonymKey ->
    Arena.Arena (SIR.Module d_iden v_iden p_iden type_info binary_ops_allowed) SIR.ModuleKey ->
    m
        ( Arena.Arena (Type.ADT (SIR.TypeExpr d_iden' type_info)) Type.ADTKey
        , Arena.Arena (Type.TypeSynonym (SIR.TypeExpr d_iden' type_info)) Type.TypeSynonymKey
        , Arena.Arena (SIR.Module d_iden' v_iden' p_iden' type_info binary_ops_allowed) SIR.ModuleKey
        )
transform_identifiers transform_d_iden transform_v_iden transform_p_iden adts type_synonyms modules = (,,) <$> Arena.transformM transform_adt adts <*> Arena.transformM transform_type_synonym type_synonyms <*> Arena.transformM transform_module modules
    where
        transform_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM transform_variant variants
            where
                transform_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> transform_type_expr ty) fields
                transform_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM transform_type_expr fields

        transform_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> transform_type_expr expansion

        transform_type_expr (SIR.TypeExpr'Identifier type_info sp id) = SIR.TypeExpr'Identifier type_info sp <$> transform_d_iden id
        transform_type_expr (SIR.TypeExpr'Tuple type_info a b) = SIR.TypeExpr'Tuple type_info <$> transform_type_expr a <*> transform_type_expr b
        transform_type_expr (SIR.TypeExpr'Hole type_info sp hid) = pure $ SIR.TypeExpr'Hole type_info sp hid
        transform_type_expr (SIR.TypeExpr'Function type_info sp arg res) = SIR.TypeExpr'Function type_info sp <$> transform_type_expr arg <*> transform_type_expr res
        transform_type_expr (SIR.TypeExpr'Forall type_info names ty) = SIR.TypeExpr'Forall type_info names <$> transform_type_expr ty
        transform_type_expr (SIR.TypeExpr'Apply type_info sp ty args) = SIR.TypeExpr'Apply type_info sp <$> transform_type_expr ty <*> transform_type_expr args
        transform_type_expr (SIR.TypeExpr'Wild type_info sp) = pure $ SIR.TypeExpr'Wild type_info sp
        transform_type_expr (SIR.TypeExpr'Poison type_info sp) = pure $ SIR.TypeExpr'Poison type_info sp

        transform_module (SIR.Module id bindings adts syns) = SIR.Module id <$> mapM transform_binding bindings <*> pure adts <*> pure syns

        transform_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> transform_pat target <*> pure eq_sp <*> transform_expr expr
        transform_binding (SIR.Binding'ADTVariant bvk variant) = pure $ SIR.Binding'ADTVariant bvk variant

        transform_pat (SIR.Pattern'Identifier type_info sp bnk) = pure $ SIR.Pattern'Identifier type_info sp bnk
        transform_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
        transform_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> transform_pat a <*> transform_pat b
        transform_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> transform_pat subpat
        transform_pat (SIR.Pattern'AnonADTVariant type_info sp variant subpat) = SIR.Pattern'AnonADTVariant type_info sp <$> transform_p_iden variant <*> mapM transform_pat subpat
        transform_pat (SIR.Pattern'NamedADTVariant type_info sp variant subpat) = SIR.Pattern'NamedADTVariant type_info sp <$> transform_p_iden variant <*> mapM (\ (field_name, field_pat) -> (field_name,) <$> transform_pat field_pat) subpat
        transform_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

        transform_expr (SIR.Expr'Identifier id type_info sp i) = SIR.Expr'Identifier id type_info sp <$> transform_v_iden i
        transform_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
        transform_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
        transform_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
        transform_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
        transform_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b

        transform_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> transform_expr a <*> transform_expr b

        transform_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> transform_pat param <*> transform_expr body

        transform_expr (SIR.Expr'Let id type_info sp bindings body) = SIR.Expr'Let id type_info sp <$> mapM transform_binding bindings <*> transform_expr body
        transform_expr (SIR.Expr'LetRec id type_info sp bindings body) = SIR.Expr'LetRec id type_info sp <$> mapM transform_binding bindings <*> transform_expr body

        transform_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) = SIR.Expr'BinaryOps id allowed type_info sp <$> transform_expr first <*> mapM (\ (iden, rhs) -> (,) <$> transform_v_iden iden <*> transform_expr rhs) ops

        transform_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> transform_expr callee <*> transform_expr arg

        transform_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> transform_expr cond <*> transform_expr t <*> transform_expr f
        transform_expr (SIR.Expr'Case id type_info sp case_sp e arms) = SIR.Expr'Case id type_info sp case_sp <$> transform_expr e <*> mapM (\ (pat, expr) -> (,) <$> transform_pat pat <*> transform_expr expr) arms

        transform_expr (SIR.Expr'TypeAnnotation id type_info sp ty e) = SIR.Expr'TypeAnnotation id type_info sp <$> transform_type_expr ty <*> transform_expr e

        transform_expr (SIR.Expr'Forall id type_info sp names e) = SIR.Expr'Forall id type_info sp names <$> transform_expr e
        transform_expr (SIR.Expr'TypeApply id type_info sp e args) = SIR.Expr'TypeApply id type_info sp <$> transform_expr e <*> transform_type_expr args

        transform_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid

        transform_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

resolve :: UnresolvedSIR -> Compiler.WithDiagnostics Error Void ResolvedSIR
resolve (SIR.SIR decls mods adts type_synonyms type_vars bound_values mod) =
    let (adts', type_synonyms', mods') = runIdentity (transform_identifiers Identity split_iden split_iden adts type_synonyms mods)
    in transform_identifiers (resolve_type_iden decls mods) (resolve_expr_iden decls mods) (resolve_pat_iden decls mods) adts' type_synonyms' mods' >>= \ (adts', type_synonyms', mods') ->
    pure (SIR.SIR decls mods' adts' type_synonyms' type_vars bound_values mod)

split_iden :: UnresolvedVIden -> Identity (SIR.NameContext, Maybe [Located Text], Located Text)
split_iden (_, []) = error "empty identifier"
split_iden (nc, [x]) = pure (nc, Nothing, x)
split_iden (nc, x) = pure (nc, Just $ init x, last x)

-- TODO: factor out repeated code?
resolve_expr_iden :: UnresolvedDeclArena -> UnresolvedModuleArena -> (SIR.NameContext, Maybe [Located Text], Located Text) -> Compiler.WithDiagnostics Error Void (Located (Maybe SIR.BoundValueKey))
resolve_expr_iden decls mods (nc, Just type_iden, last_segment) =
    let sp = Located.just_span (head type_iden) <> Located.just_span last_segment
    in resolve_type_iden decls mods (nc, type_iden) >>= \ resolved_type ->
    case get_value_child decls mods <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Located sp (Just v)
        Just (Left e) -> Compiler.tell_error e >> pure (Located sp Nothing)
        Nothing -> pure $ Located sp Nothing

resolve_expr_iden _ _ (nc, Nothing, last_segment@(Located last_segment_sp _)) =
    case resolve nc last_segment of
        Right v -> pure $ Located last_segment_sp (Just v)
        Left e -> Compiler.tell_error e >> pure (Located last_segment_sp Nothing)
    where
        resolve (SIR.NameContext _ bn_children _ parent) name =
            case Map.lookup (Located.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

resolve_type_iden :: UnresolvedDeclArena -> UnresolvedModuleArena -> UnresolvedDIden -> Compiler.WithDiagnostics Error Void (Maybe SIR.DeclKey)
resolve_type_iden _ mods (_, []) = error "empty identifier"
resolve_type_iden decls mods (nc, first:more) =
    case resolve_first nc first of
        Right first_resolved ->
            case foldlM (get_decl_child decls mods) first_resolved more of
                Right r -> pure $ Just r
                Left e -> Compiler.tell_error e >> pure Nothing
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve_first (SIR.NameContext d_children _ _ parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

resolve_pat_iden :: UnresolvedDeclArena -> UnresolvedModuleArena -> (SIR.NameContext, Maybe [Located Text], Located Text) -> Compiler.WithDiagnostics Error Void (Maybe Type.ADTVariantIndex)
resolve_pat_iden decls mods (nc, Just type_iden, last_segment) =
    resolve_type_iden decls mods (nc, type_iden) >>= \ resolved_type ->
    case get_variant_child decls mods <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Just v
        Just (Left e) -> Compiler.tell_error e >> pure Nothing
        Nothing -> pure Nothing

resolve_pat_iden _ mods (nc, Nothing, last_segment) =
    case resolve nc last_segment of
        Right v -> pure $ Just v
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve (SIR.NameContext _ _ adtv_children parent) name =
            case Map.lookup (Located.unlocate name) adtv_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

get_decl_child :: UnresolvedDeclArena -> UnresolvedModuleArena -> SIR.DeclKey -> Located Text -> Either Error SIR.DeclKey
get_decl_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let SIR.Module _ {- TODO: (SIR.NameContext d_children _ _ _) -} _ _ _ = Arena.get mods m
                in todo -- Map.lookup (Located.unlocate name) d_children
            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: UnresolvedDeclArena -> UnresolvedModuleArena -> SIR.DeclKey -> Located Text -> Either Error SIR.BoundValueKey
get_value_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let SIR.Module _ {- TODO: (SIR.NameContext _ v_children _ _) -} _ _ _ = Arena.get mods m
                in todo -- Map.lookup (Located.unlocate name) v_children
            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_variant_child :: UnresolvedDeclArena -> UnresolvedModuleArena -> SIR.DeclKey -> Located Text -> Either Error Type.ADTVariantIndex
get_variant_child decls mods thing name =
    let res = case Arena.get decls thing of
            SIR.Decl'Module m ->
                let SIR.Module _ {- TODO: (SIR.NameContext _ _ adtv_children _) -} _ _ _ = Arena.get mods m
                in todo -- Map.lookup (Located.unlocate name) adtv_children
            SIR.Decl'Type _ -> Nothing
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
