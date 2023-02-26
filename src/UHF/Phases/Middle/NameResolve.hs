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

import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity, runIdentity))

type UnresolvedDecl = HIR.Decl UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedTypeIdentifier = (HIR.NameContext, [Located Text])
type UnresolvedExprIdentifier = (HIR.NameContext, [Located Text])
type UnresolvedADT = HIR.ADT UnresolvedType
type UnresolvedTypeSynonym = HIR.TypeSynonym UnresolvedType
type UnresolvedType = HIR.TypeExpr UnresolvedTypeIdentifier
type UnresolvedBinding = HIR.Binding UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedExpr = HIR.Expr UnresolvedExprIdentifier UnresolvedType () ()
type UnresolvedPattern = HIR.Pattern UnresolvedExprIdentifier

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl HIR.DeclKey
type UnresolvedADTArena = Arena.Arena UnresolvedADT HIR.ADTKey
type UnresolvedTypeSynonymArena = Arena.Arena UnresolvedTypeSynonym HIR.TypeSynonymKey

type ResolvedDecl = HIR.Decl (Located (Maybe HIR.BoundValueKey)) ResolvedType () ()
type ResolvedADT = HIR.ADT ResolvedType
type ResolvedTypeSynonym = HIR.TypeSynonym ResolvedType
type ResolvedType = HIR.TypeExpr (Maybe HIR.DeclKey)
type ResolvedBinding = HIR.Binding (Located (Maybe HIR.BoundValueKey)) ResolvedType () ()
type ResolvedExpr = HIR.Expr (Located (Maybe HIR.BoundValueKey)) ResolvedType () ()
type ResolvedPattern = HIR.Pattern (Located (Maybe HIR.BoundValueKey))

type ResolvedDeclArena = Arena.Arena ResolvedDecl HIR.DeclKey
type ResolvedADTArena = Arena.Arena ResolvedADT HIR.ADTKey
type ResolvedTypeSynonymArena = Arena.Arena ResolvedTypeSynonym HIR.TypeSynonymKey

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

transform_identifiers :: Monad m => (t_iden -> m t_iden') -> (e_iden -> m e_iden') -> Arena.Arena (HIR.ADT (HIR.TypeExpr t_iden)) HIR.ADTKey -> Arena.Arena (HIR.TypeSynonym (HIR.TypeExpr t_iden)) HIR.TypeSynonymKey -> Arena.Arena (HIR.Decl e_iden (HIR.TypeExpr t_iden) typeinfo binaryopsallowed) HIR.DeclKey -> m (Arena.Arena (HIR.ADT (HIR.TypeExpr t_iden')) HIR.ADTKey, Arena.Arena (HIR.TypeSynonym (HIR.TypeExpr t_iden')) HIR.TypeSynonymKey, Arena.Arena (HIR.Decl e_iden' (HIR.TypeExpr t_iden') typeinfo binaryopsallowed) HIR.DeclKey)
transform_identifiers transform_t_iden transform_e_iden adts type_synonyms decls = (,,) <$> Arena.transformM transform_adt adts <*> Arena.transformM transform_type_synonym type_synonyms <*> Arena.transformM transform_decl decls
    where
        transform_adt (HIR.ADT name variants) = HIR.ADT name <$> mapM transform_variant variants
            where
                transform_variant (HIR.ADTVariant'Named name fields) = HIR.ADTVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> transform_type_expr ty) fields
                transform_variant (HIR.ADTVariant'Anon name fields) = HIR.ADTVariant'Anon name <$> mapM transform_type_expr fields

        transform_type_synonym (HIR.TypeSynonym name expansion) = HIR.TypeSynonym name <$> transform_type_expr expansion

        transform_type_expr (HIR.TypeExpr'Identifier sp id) = HIR.TypeExpr'Identifier sp <$> transform_t_iden id
        transform_type_expr (HIR.TypeExpr'Tuple a b) = HIR.TypeExpr'Tuple <$> transform_type_expr a <*> transform_type_expr b
        transform_type_expr (HIR.TypeExpr'Poison sp) = pure $ HIR.TypeExpr'Poison sp

        transform_decl (HIR.Decl'Module nc bindings) = HIR.Decl'Module nc <$> mapM transform_binding bindings
        transform_decl (HIR.Decl'Type ty) = pure $ HIR.Decl'Type ty

        transform_binding (HIR.Binding target eq_sp expr) = HIR.Binding <$> transform_pat target <*> pure eq_sp <*> transform_expr expr

        transform_pat (HIR.Pattern'Identifier typeinfo sp bnk) = pure $ HIR.Pattern'Identifier typeinfo sp bnk
        transform_pat (HIR.Pattern'Wildcard typeinfo sp) = pure $ HIR.Pattern'Wildcard typeinfo sp
        transform_pat (HIR.Pattern'Tuple typeinfo sp a b) = HIR.Pattern'Tuple typeinfo sp <$> transform_pat a <*> transform_pat b
        transform_pat (HIR.Pattern'Named typeinfo sp at_sp bnk subpat) = HIR.Pattern'Named typeinfo sp at_sp bnk <$> transform_pat subpat
        transform_pat (HIR.Pattern'Poison typeinfo sp) = pure $ HIR.Pattern'Poison typeinfo sp

        transform_expr (HIR.Expr'Identifier typeinfo sp i) = HIR.Expr'Identifier typeinfo sp <$> transform_e_iden i
        transform_expr (HIR.Expr'Char typeinfo sp c) = pure $ HIR.Expr'Char typeinfo sp c
        transform_expr (HIR.Expr'String typeinfo sp s) = pure $ HIR.Expr'String typeinfo sp s
        transform_expr (HIR.Expr'Int typeinfo sp i) = pure $ HIR.Expr'Int typeinfo sp i
        transform_expr (HIR.Expr'Float typeinfo sp f) = pure $ HIR.Expr'Float typeinfo sp f
        transform_expr (HIR.Expr'Bool typeinfo sp b) = pure $ HIR.Expr'Bool typeinfo sp b

        transform_expr (HIR.Expr'Tuple typeinfo sp a b) = HIR.Expr'Tuple typeinfo sp <$> transform_expr a <*> transform_expr b

        transform_expr (HIR.Expr'Lambda typeinfo sp param body) = HIR.Expr'Lambda typeinfo sp <$> transform_pat param <*> transform_expr body

        transform_expr (HIR.Expr'Let typeinfo sp bindings body) = HIR.Expr'Let typeinfo sp <$> mapM transform_binding bindings <*> transform_expr body
        transform_expr (HIR.Expr'LetRec typeinfo sp bindings body) = HIR.Expr'LetRec typeinfo sp <$> mapM transform_binding bindings <*> transform_expr body

        transform_expr (HIR.Expr'BinaryOps allowed typeinfo sp first ops) = HIR.Expr'BinaryOps allowed typeinfo sp <$> transform_expr first <*> mapM (\ (iden, rhs) -> (,) <$> transform_e_iden iden <*> transform_expr rhs) ops

        transform_expr (HIR.Expr'Call typeinfo sp callee arg) = HIR.Expr'Call typeinfo sp <$> transform_expr callee <*> transform_expr arg

        transform_expr (HIR.Expr'If typeinfo sp if_sp cond t f) = HIR.Expr'If typeinfo sp if_sp <$> transform_expr cond <*> transform_expr t <*> transform_expr f
        transform_expr (HIR.Expr'Case typeinfo sp case_sp e arms) = HIR.Expr'Case typeinfo sp case_sp <$> transform_expr e <*> mapM (\ (pat, expr) -> (,) <$> transform_pat pat <*> transform_expr expr) arms

        transform_expr (HIR.Expr'TypeAnnotation typeinfo sp ty e) = HIR.Expr'TypeAnnotation typeinfo sp <$> transform_type_expr ty <*> transform_expr e

        transform_expr (HIR.Expr'Poison typeinfo sp) = pure $ HIR.Expr'Poison typeinfo sp

resolve :: (UnresolvedDeclArena, UnresolvedADTArena, UnresolvedTypeSynonymArena) -> Compiler.Compiler (ResolvedDeclArena, ResolvedADTArena, ResolvedTypeSynonymArena)
resolve (decls, adts, type_synonyms) =
    let (res, errs) =
            runWriter (
                let (adts', type_synonyms', decls') = runIdentity (transform_identifiers Identity split_expr_iden adts type_synonyms decls)
                in transform_identifiers (resolve_type_iden decls) (resolve_expr_iden decls) adts' type_synonyms' decls' >>= \ (adts', type_synonyms', decls') ->
                pure (decls', adts', type_synonyms')
            )
    in Compiler.errors errs >> pure res

split_expr_iden :: UnresolvedExprIdentifier -> Identity (HIR.NameContext, Maybe [Located Text], Located Text)
split_expr_iden (_, []) = error "empty identifier"
split_expr_iden (nc, [x]) = pure (nc, Nothing, x)
split_expr_iden (nc, x) = pure (nc, Just $ init x, last x)

resolve_expr_iden :: UnresolvedDeclArena -> (HIR.NameContext, Maybe [Located Text], Located Text) -> Writer [Error] (Located (Maybe HIR.BoundValueKey))
resolve_expr_iden decls (nc, Just type_iden, last_segment) =
    let sp = Located.just_span (head type_iden) <> Located.just_span last_segment
    in resolve_type_iden decls (nc, type_iden) >>= \ resolved_type ->
    case get_value_child decls <$> resolved_type <*> pure last_segment of
        Just (Right v) -> pure $ Located sp (Just v)
        Just (Left e) -> tell [e] >> pure (Located sp Nothing)
        Nothing -> pure $ Located sp Nothing

resolve_expr_iden _ (nc, Nothing, last_segment@(Located last_segment_sp _)) =
    case resolve nc last_segment of
        Right v -> pure $ Located last_segment_sp (Just v)
        Left e -> tell [e] >> pure (Located last_segment_sp Nothing)
    where
        resolve (HIR.NameContext _ bn_children parent) name =
            case Map.lookup (Located.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name

resolve_type_iden :: UnresolvedDeclArena -> UnresolvedTypeIdentifier -> Writer [Error] (Maybe HIR.DeclKey)
resolve_type_iden _ (_, []) = error "empty identifier"
resolve_type_iden decls (nc, first:more) =
    case resolve_first nc first of
        Right first_resolved ->
            case foldlM (get_decl_child decls) first_resolved more of
                Right r -> pure $ Just r
                Left e -> tell [e] >> pure Nothing
        Left e -> tell [e] >> pure Nothing
    where
        resolve_first (HIR.NameContext d_children _ parent) first =
            case Map.lookup (Located.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first -- TODO: put previous in error

get_decl_child :: UnresolvedDeclArena -> HIR.DeclKey -> Located Text -> Either Error HIR.DeclKey
get_decl_child decls thing name =
    let res = case Arena.get decls thing of
            HIR.Decl'Module (HIR.NameContext d_children _ _) _ -> Map.lookup (Located.unlocate name) d_children
            HIR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks, this will also need infinite recursion checking
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: UnresolvedDeclArena -> HIR.DeclKey -> Located Text -> Either Error HIR.BoundValueKey
get_value_child decls thing name =
    let res = case Arena.get decls thing of
            HIR.Decl'Module (HIR.NameContext _ v_children _) _ -> Map.lookup (Located.unlocate name) v_children
            HIR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
