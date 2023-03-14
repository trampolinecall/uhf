module UHF.Phases.Middle.NameResolve
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

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity, runIdentity))

type UnresolvedHIR = HIR.HIR UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedDecl = HIR.Decl UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedTypeIdentifier = (HIR.NameContext, [Located Text])
type UnresolvedExprIdentifier = (HIR.NameContext, [Located Text])
type UnresolvedADT = Type.ADT UnresolvedType
type UnresolvedTypeSynonym = Type.TypeSynonym UnresolvedType
type UnresolvedType = HIR.TypeExpr UnresolvedTypeIdentifier
type UnresolvedBinding = HIR.Binding UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedExpr = HIR.Expr UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedPattern = HIR.Pattern UnresolvedExprIdentifier

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl DeclKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym Type.TypeSynonymKey

type ResolvedHIR = HIR.HIR (Located (Maybe BoundValueKey)) ResolvedType () ()
type ResolvedDecl = HIR.Decl (Located (Maybe BoundValueKey)) ResolvedType () ()
type ResolvedADT = Type.ADT ResolvedType
type ResolvedTypeSynonym = Type.TypeSynonym ResolvedType
type ResolvedType = HIR.TypeExpr (Maybe DeclKey)
type ResolvedBinding = HIR.Binding (Located (Maybe BoundValueKey)) ResolvedType () ()
type ResolvedExpr = HIR.Expr (Located (Maybe BoundValueKey)) ResolvedType () ()
type ResolvedPattern = HIR.Pattern (Located (Maybe BoundValueKey))

type ResolvedDeclArena = Arena.Arena ResolvedDecl DeclKey
type ResolvedADTArena = Arena.Arena ResolvedADT ADTKey
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

transform_identifiers :: Monad m => (t_iden -> m t_iden') -> (e_iden -> m e_iden') -> Arena.Arena (Type.ADT (HIR.TypeExpr t_iden)) ADTKey -> Arena.Arena (Type.TypeSynonym (HIR.TypeExpr t_iden)) Type.TypeSynonymKey -> Arena.Arena (HIR.Decl e_iden (HIR.TypeExpr t_iden) type_info binary_ops_allowed) DeclKey -> m (Arena.Arena (Type.ADT (HIR.TypeExpr t_iden')) ADTKey, Arena.Arena (Type.TypeSynonym (HIR.TypeExpr t_iden')) Type.TypeSynonymKey, Arena.Arena (HIR.Decl e_iden' (HIR.TypeExpr t_iden') type_info binary_ops_allowed) DeclKey)
transform_identifiers transform_t_iden transform_e_iden adts type_synonyms decls = (,,) <$> Arena.transformM transform_adt adts <*> Arena.transformM transform_type_synonym type_synonyms <*> Arena.transformM transform_decl decls
    where
        transform_adt (Type.ADT id name variants) = Type.ADT id name <$> mapM transform_variant variants
            where
                transform_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> transform_type_expr ty) fields
                transform_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> mapM transform_type_expr fields

        transform_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> transform_type_expr expansion

        transform_type_expr (HIR.TypeExpr'Identifier sp id) = HIR.TypeExpr'Identifier sp <$> transform_t_iden id
        transform_type_expr (HIR.TypeExpr'Tuple a b) = HIR.TypeExpr'Tuple <$> transform_type_expr a <*> transform_type_expr b
        transform_type_expr (HIR.TypeExpr'Poison sp) = pure $ HIR.TypeExpr'Poison sp

        transform_decl (HIR.Decl'Module id nc bindings adts syns) = HIR.Decl'Module id nc <$> mapM transform_binding bindings <*> pure adts <*> pure syns
        transform_decl (HIR.Decl'Type ty) = pure $ HIR.Decl'Type ty

        transform_binding (HIR.Binding target eq_sp expr) = HIR.Binding <$> transform_pat target <*> pure eq_sp <*> transform_expr expr

        transform_pat (HIR.Pattern'Identifier id type_info sp bnk) = pure $ HIR.Pattern'Identifier id type_info sp bnk
        transform_pat (HIR.Pattern'Wildcard id type_info sp) = pure $ HIR.Pattern'Wildcard id type_info sp
        transform_pat (HIR.Pattern'Tuple id type_info sp a b) = HIR.Pattern'Tuple id type_info sp <$> transform_pat a <*> transform_pat b
        transform_pat (HIR.Pattern'Named id type_info sp at_sp bnk subpat) = HIR.Pattern'Named id type_info sp at_sp bnk <$> transform_pat subpat
        transform_pat (HIR.Pattern'Poison id type_info sp) = pure $ HIR.Pattern'Poison id type_info sp

        transform_expr (HIR.Expr'Identifier id type_info sp i) = HIR.Expr'Identifier id type_info sp <$> transform_e_iden i
        transform_expr (HIR.Expr'Char id type_info sp c) = pure $ HIR.Expr'Char id type_info sp c
        transform_expr (HIR.Expr'String id type_info sp s) = pure $ HIR.Expr'String id type_info sp s
        transform_expr (HIR.Expr'Int id type_info sp i) = pure $ HIR.Expr'Int id type_info sp i
        transform_expr (HIR.Expr'Float id type_info sp f) = pure $ HIR.Expr'Float id type_info sp f
        transform_expr (HIR.Expr'Bool id type_info sp b) = pure $ HIR.Expr'Bool id type_info sp b

        transform_expr (HIR.Expr'Tuple id type_info sp a b) = HIR.Expr'Tuple id type_info sp <$> transform_expr a <*> transform_expr b

        transform_expr (HIR.Expr'Lambda id type_info sp param body) = HIR.Expr'Lambda id type_info sp <$> transform_pat param <*> transform_expr body

        transform_expr (HIR.Expr'Let id type_info sp bindings body) = HIR.Expr'Let id type_info sp <$> mapM transform_binding bindings <*> transform_expr body

        transform_expr (HIR.Expr'BinaryOps id allowed type_info sp first ops) = HIR.Expr'BinaryOps id allowed type_info sp <$> transform_expr first <*> mapM (\ (iden, rhs) -> (,) <$> transform_e_iden iden <*> transform_expr rhs) ops

        transform_expr (HIR.Expr'Call id type_info sp callee arg) = HIR.Expr'Call id type_info sp <$> transform_expr callee <*> transform_expr arg

        transform_expr (HIR.Expr'If id type_info sp if_sp cond t f) = HIR.Expr'If id type_info sp if_sp <$> transform_expr cond <*> transform_expr t <*> transform_expr f
        transform_expr (HIR.Expr'Case id type_info sp case_sp e arms) = HIR.Expr'Case id type_info sp case_sp <$> transform_expr e <*> mapM (\ (pat, expr) -> (,) <$> transform_pat pat <*> transform_expr expr) arms

        transform_expr (HIR.Expr'TypeAnnotation id type_info sp ty e) = HIR.Expr'TypeAnnotation id type_info sp <$> transform_type_expr ty <*> transform_expr e

        transform_expr (HIR.Expr'Poison id type_info sp) = pure $ HIR.Expr'Poison id type_info sp

resolve :: UnresolvedHIR -> Compiler.WithDiagnostics Error Void ResolvedHIR
resolve (HIR.HIR decls adts type_synonyms bound_values mod) =
    let (adts', type_synonyms', decls') = runIdentity (transform_identifiers Identity split_expr_iden adts type_synonyms decls)
    in transform_identifiers (resolve_type_iden decls) (resolve_expr_iden decls) adts' type_synonyms' decls' >>= \ (adts', type_synonyms', decls') ->
    pure (HIR.HIR decls' adts' type_synonyms' bound_values mod)

split_expr_iden :: UnresolvedExprIdentifier -> Identity (HIR.NameContext, Maybe [Located Text], Located Text)
split_expr_iden (_, []) = error "empty identifier"
split_expr_iden (nc, [x]) = pure (nc, Nothing, x)
split_expr_iden (nc, x) = pure (nc, Just $ init x, last x)

resolve_expr_iden :: UnresolvedDeclArena -> (HIR.NameContext, Maybe [Located Text], Located Text) -> Compiler.WithDiagnostics Error Void (Located (Maybe BoundValueKey))
resolve_expr_iden decls (nc, Just type_iden, last_segment) =
    let sp = Located.just_span (head type_iden) <> Located.just_span last_segment
    in resolve_type_iden decls (nc, type_iden) >>= \ resolved_type ->
    case get_value_child decls <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Located sp (Just v)
        Just (Left e) -> Compiler.tell_error e >> pure (Located sp Nothing)
        Nothing -> pure $ Located sp Nothing

resolve_expr_iden _ (nc, Nothing, last_segment@(Located last_segment_sp _)) =
    case resolve nc last_segment of
        Right v -> pure $ Located last_segment_sp (Just v)
        Left e -> Compiler.tell_error e >> pure (Located last_segment_sp Nothing)
    where
        resolve (HIR.NameContext _ bn_children parent) name =
            case Map.lookup (Located.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

resolve_type_iden :: UnresolvedDeclArena -> UnresolvedTypeIdentifier -> Compiler.WithDiagnostics Error Void (Maybe DeclKey)
resolve_type_iden _ (_, []) = error "empty identifier"
resolve_type_iden decls (nc, first:more) =
    case resolve_first nc first of
        Right first_resolved ->
            case foldlM (get_decl_child decls) first_resolved more of
                Right r -> pure $ Just r
                Left e -> Compiler.tell_error e >> pure Nothing
        Left e -> Compiler.tell_error e >> pure Nothing
    where
        resolve_first (HIR.NameContext d_children _ parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

get_decl_child :: UnresolvedDeclArena -> DeclKey -> Located Text -> Either Error DeclKey
get_decl_child decls thing name =
    let res = case Arena.get decls thing of
            HIR.Decl'Module _ (HIR.NameContext d_children _ _) _ _ _ -> Map.lookup (Located.unlocate name) d_children
            HIR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks, this will also need infinite recursion checking
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: UnresolvedDeclArena -> DeclKey -> Located Text -> Either Error BoundValueKey
get_value_child decls thing name =
    let res = case Arena.get decls thing of
            HIR.Decl'Module _ (HIR.NameContext _ v_children _) _ _ _ -> Map.lookup (Located.unlocate name) v_children
            HIR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
