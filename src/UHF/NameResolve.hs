module UHF.NameResolve
    ( UnresolvedBinding
    , ResolvedBinding
    , UnresolvedExpr
    , ResolvedExpr
    , UnresolvedPattern
    , ResolvedPattern

    , UnresolvedNominalTypeArena
    , ResolvedNominalTypeArena
    , UnresolvedBindingArena
    , ResolvedBindingArena

    , DeclArena

    , resolve
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified UHF.IR as IR

import qualified Data.Map as Map
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)

type UnresolvedTypeIdentifier = (IR.NameContext, [Location.Located Text])
type UnresolvedExprIdentifier = (IR.NameContext, [Location.Located Text])
type UnresolvedNominalType = IR.NominalType UnresolvedType
type UnresolvedType = IR.TypeExpr UnresolvedTypeIdentifier
type UnresolvedBinding = IR.Binding UnresolvedExprIdentifier
type UnresolvedExpr = IR.Expr UnresolvedExprIdentifier
type UnresolvedPattern = IR.Pattern UnresolvedExprIdentifier

type UnresolvedBindingArena = Arena.Arena UnresolvedBinding IR.BindingKey
type UnresolvedNominalTypeArena = Arena.Arena UnresolvedNominalType IR.NominalTypeKey

type ResolvedNominalType = IR.NominalType ResolvedType
type ResolvedType = IR.TypeExpr (Maybe IR.DeclKey)
type ResolvedBinding = IR.Binding (Maybe IR.BoundNameKey)
type ResolvedExpr = IR.Expr (Maybe IR.BoundNameKey)
type ResolvedPattern = IR.Pattern (Maybe IR.BoundNameKey)

type ResolvedBindingArena = Arena.Arena ResolvedBinding IR.BindingKey
type ResolvedNominalTypeArena = Arena.Arena ResolvedNominalType IR.NominalTypeKey

type Decl = IR.Decl

type DeclArena = Arena.Arena Decl IR.DeclKey

data Error
    = MultiIden (Location.Located [Location.Located Text])
    | CouldNotFind (Maybe (Location.Located Text)) (Location.Located Text)

instance Diagnostic.IsError Error where
    to_error (MultiIden (Location.Located sp _)) = Diagnostic.Error Diagnostic.Codes.multi_iden $
        Diagnostic.DiagnosticContents
            (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error "paths are not supported yet"]]] -- TODO

    to_error (CouldNotFind prev (Location.Located sp name)) =
        let message =
                ("could not find name '" <> convert_str name <> "'")
                    <> case prev of
                        Just (Location.Located _ prev_name) -> "in '" <> convert_str prev_name <> "'"
                        Nothing -> ""
        in Diagnostic.Error Diagnostic.Codes.undef_name $
            Diagnostic.DiagnosticContents
                (Just sp)
                [Underlines.underlines [sp `Underlines.primary` [Underlines.error message]]]

transform_identifiers :: Monad m => (t_iden -> m t_iden') -> (e_iden -> m e_iden') -> Arena.Arena (IR.NominalType (IR.TypeExpr t_iden)) IR.NominalTypeKey -> Arena.Arena (IR.Binding e_iden) IR.BindingKey -> m (Arena.Arena (IR.NominalType (IR.TypeExpr t_iden')) IR.NominalTypeKey, Arena.Arena (IR.Binding e_iden') IR.BindingKey)
transform_identifiers transform_t_iden transform_e_iden nominal_types bindings = (,) <$> Arena.transformM transform_nominal_type nominal_types <*> Arena.transformM transform_binding bindings
    where
        transform_nominal_type (IR.NominalType'Data variants) = IR.NominalType'Data <$> mapM transform_variant variants
            where
                transform_variant (IR.DataVariant'Named name fields) = IR.DataVariant'Named name <$> mapM (\ (name, ty) -> (,) name <$> transform_type_expr ty) fields
                transform_variant (IR.DataVariant'Anon name fields) = IR.DataVariant'Anon name <$> mapM transform_type_expr fields
        transform_nominal_type (IR.NominalType'Synonym expansion) = IR.NominalType'Synonym <$> transform_type_expr expansion

        transform_type_expr (IR.TypeExpr'Identifier id) = IR.TypeExpr'Identifier <$> transform_t_iden id
        transform_type_expr (IR.TypeExpr'Tuple items) = IR.TypeExpr'Tuple <$> mapM transform_type_expr items

        transform_binding (IR.Binding target expr) = IR.Binding <$> transform_pat target <*> transform_expr expr

        transform_pat (IR.Pattern'Identifier bnk) = pure $ IR.Pattern'Identifier bnk
        transform_pat (IR.Pattern'Tuple a b) = IR.Pattern'Tuple <$> transform_pat a <*> transform_pat b
        transform_pat (IR.Pattern'Named bnk subpat) = IR.Pattern'Named bnk <$> transform_pat subpat
        transform_pat (IR.Pattern'Poison) = pure IR.Pattern'Poison

        transform_expr (IR.Expr'Identifier i) = IR.Expr'Identifier <$> transform_e_iden i
        transform_expr (IR.Expr'Char c) = pure $ IR.Expr'Char c
        transform_expr (IR.Expr'String s) = pure $ IR.Expr'String s
        transform_expr (IR.Expr'Int i) = pure $ IR.Expr'Int i
        transform_expr (IR.Expr'Float f) = pure $ IR.Expr'Float f
        transform_expr (IR.Expr'Bool b) = pure $ IR.Expr'Bool b

        transform_expr (IR.Expr'Tuple a b) = IR.Expr'Tuple <$> transform_expr a <*> transform_expr b

        transform_expr (IR.Expr'Lambda bound_names param body) = IR.Expr'Lambda bound_names <$> transform_pat param <*> transform_expr body

        transform_expr (IR.Expr'Let name_context body) = IR.Expr'Let name_context <$> transform_expr body
        transform_expr (IR.Expr'LetRec name_context body) = IR.Expr'LetRec name_context <$> transform_expr body

        transform_expr (IR.Expr'BinaryOps first ops) = IR.Expr'BinaryOps <$> transform_expr first <*> mapM (\ (iden, rhs) -> (,) <$> transform_e_iden iden <*> transform_expr rhs) ops

        transform_expr (IR.Expr'Call callee args) = IR.Expr'Call <$> transform_expr callee <*> mapM transform_expr args

        transform_expr (IR.Expr'If cond t f) = IR.Expr'If <$> transform_expr cond <*> transform_expr t <*> transform_expr f
        transform_expr (IR.Expr'Case e arms) = IR.Expr'Case <$> transform_expr e <*> mapM (\ (bound_names, pat, expr) -> (,,) bound_names <$> transform_pat pat <*> transform_expr expr) arms

        transform_expr (IR.Expr'Poison) = pure IR.Expr'Poison

resolve :: (DeclArena, UnresolvedNominalTypeArena, UnresolvedBindingArena, IR.DeclKey) -> Writer [Error] (DeclArena, ResolvedNominalTypeArena, ResolvedBindingArena)
resolve (decls, nominals, bindings, mod) =
    let (nominals', bindings') = runIdentity (transform_identifiers (Identity) split_expr_iden nominals bindings)
    in transform_identifiers (resolve_type_iden decls mod) (resolve_expr_iden decls mod) nominals' bindings' >>= \ (nominals', bindings') ->
    pure (decls, nominals', bindings')
    -- Arena.transformM (resolve_for_decl decls mod) decls >>= \ decls' ->
    -- Arena.transformM (resolve_for_nominal_type decls mod) nominals >>= \ nominals' ->
    -- Arena.transformM (resolve_for_binding decls mod) bindings >>= \ bindings' ->
    -- pure (decls', nominals', bindings')

split_expr_iden :: UnresolvedExprIdentifier -> Identity (IR.NameContext, Maybe [Location.Located Text], Location.Located Text)
split_expr_iden (nc, []) = error "empty identifier"
split_expr_iden (nc, [x]) = pure (nc, Nothing, x)
split_expr_iden (nc, x) = pure (nc, Just $ init x, last x)

resolve_expr_iden :: DeclArena -> IR.DeclKey -> (IR.NameContext, Maybe [Location.Located Text], Location.Located Text) -> Writer [Error] (Maybe IR.BoundNameKey)
resolve_expr_iden decls mod (nc, Just type_iden, last_segment) =
    runMaybeT $
        MaybeT (resolve_type_iden decls mod (nc, type_iden)) >>= \ resolved_type ->
        case get_value_child decls resolved_type last_segment of
            Right v -> pure v
            Left e -> lift (tell [e]) >> (MaybeT $ pure Nothing)

resolve_expr_iden decls mod (nc, Nothing, last_segment) =
    case resolve nc last_segment of
        Right v -> pure $ Just v
        Left e -> tell [e] >> pure Nothing
    where
        resolve (IR.NameContext _ bn_children parent) name =
            case Map.lookup (Location.unlocate name) bn_children of
                Just res -> Right res
                Nothing ->
                    case parent of
                        Just parent -> resolve parent name
                        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous segment in error

resolve_type_iden :: DeclArena -> IR.DeclKey -> UnresolvedTypeIdentifier -> Writer [Error] (Maybe IR.DeclKey)
resolve_type_iden decls mod (nc, []) = error "empty identifier"
resolve_type_iden decls mod (nc, segments@(first:more)) =
    case resolve_first nc first of
        Right first_resolved ->
            case foldlM (get_decl_child decls) first_resolved more of
                Right r -> pure $ Just r
                Left e -> tell [e] >> pure Nothing
        Left e -> tell [e] >> pure Nothing
    where
        resolve_first nc@(IR.NameContext d_children _ parent) first =
            case Map.lookup (Location.unlocate first) d_children of
                Just decl -> Right decl
                Nothing ->
                    case parent of
                        Just parent -> resolve_first parent first
                        Nothing -> Left $ CouldNotFind Nothing first

get_name_context :: DeclArena -> IR.DeclKey -> Maybe IR.NameContext
get_name_context decls thing = case Arena.get decls thing of
    IR.Decl'Module (IR.Module nc) -> Just nc
    IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks

get_decl_child :: DeclArena -> IR.DeclKey -> Location.Located Text -> Either Error IR.DeclKey
get_decl_child decls thing name =
    let res = case Arena.get decls thing of
            IR.Decl'Module (IR.Module (IR.NameContext d_children _ _)) -> Map.lookup (Location.unlocate name) d_children
            IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous

get_value_child :: DeclArena -> IR.DeclKey -> Location.Located Text -> Either Error IR.BoundNameKey
get_value_child decls thing name =
    let res = case Arena.get decls thing of
            IR.Decl'Module (IR.Module (IR.NameContext _ v_children _)) -> Map.lookup (Location.unlocate name) v_children
            IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name -- TODO: put previous
