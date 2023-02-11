module UHF.NameResolve
    ( UnresolvedDecl
    , ResolvedDecl
    , UnresolvedModule
    , ResolvedModule
    , UnresolvedBinding
    , ResolvedBinding
    , UnresolvedExpr
    , ResolvedExpr

    , UnresolvedDeclArena
    , ResolvedDeclArena
    , UnresolvedNominalTypeArena
    , ResolvedNominalTypeArena
    , UnresolvedBindingArena
    , ResolvedBindingArena

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

type UnresolvedTypeIdentifier = (IR.NameContext, Location.Located [Location.Located Text])
type UnresolvedExprIdentifier = (IR.NameContext, Location.Located [Location.Located Text])
type UnresolvedDecl = IR.Decl
type UnresolvedModule = IR.Module
type UnresolvedNominalType = IR.NominalType UnresolvedType
type UnresolvedType = IR.TypeExpr UnresolvedTypeIdentifier
type UnresolvedBinding = IR.Binding UnresolvedExprIdentifier
type UnresolvedExpr = IR.Expr UnresolvedExprIdentifier
type UnresolvedPattern = IR.Pattern UnresolvedExprIdentifier

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl IR.DeclKey
type UnresolvedBindingArena = Arena.Arena UnresolvedBinding IR.BindingKey
type UnresolvedNominalTypeArena = Arena.Arena UnresolvedNominalType IR.NominalTypeKey

type ResolvedDecl = IR.Decl
type ResolvedModule = IR.Module
type ResolvedNominalType = IR.NominalType ResolvedType
type ResolvedType = IR.TypeExpr (Maybe IR.DeclKey)
type ResolvedBinding = IR.Binding (Maybe IR.BoundNameKey)
type ResolvedExpr = IR.Expr (Maybe IR.BoundNameKey)
type ResolvedPattern = IR.Pattern (Maybe IR.BoundNameKey)

type ResolvedDeclArena = Arena.Arena ResolvedDecl IR.DeclKey
type ResolvedBindingArena = Arena.Arena ResolvedBinding IR.BindingKey
type ResolvedNominalTypeArena = Arena.Arena ResolvedNominalType IR.NominalTypeKey

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

-- TODO: resolve nominal types
resolve :: (UnresolvedDeclArena, UnresolvedNominalTypeArena, UnresolvedBindingArena, IR.DeclKey) -> Writer [Error] (ResolvedDeclArena, ResolvedNominalTypeArena, ResolvedBindingArena)
resolve (decls, nominals, values, mod) =
    Arena.transformM (resolve_for_decl decls mod) decls >>= \ decls' ->
    Arena.transformM (resolve_for_nominal_type decls mod) nominals >>= \ nominals' ->
    Arena.transformM (resolve_for_value decls mod) values >>= \ values' ->
    pure (decls', nominals', values')

resolve_for_decl :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedDecl -> Writer [Error] ResolvedDecl
resolve_for_decl _ _ (IR.Decl'Module m) = IR.Decl'Module <$> resolve_for_module m
    where
        resolve_for_module = pure
resolve_for_decl _ _ (IR.Decl'Type t) = pure $ IR.Decl'Type t

resolve_for_nominal_type :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedNominalType -> Writer [Error] ResolvedNominalType
resolve_for_nominal_type decls mod (IR.NominalType'Data variants) = todo
resolve_for_nominal_type decls mod (IR.NominalType'Synonym expansion) = todo

resolve_for_value :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedBinding -> Writer [Error] ResolvedBinding
resolve_for_value decls mod (IR.Binding target expr) = IR.Binding <$> resolve_for_pat decls mod target <*> resolve_for_expr decls mod expr

resolve_for_pat :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedPattern -> Writer [Error] ResolvedPattern
-- TOOD: this will change when destructuring is implemented beccause that needs to resolve constructor names (including constructor names without fields that will look exactly the same as identifier patterns)
resolve_for_pat _ _ (IR.Pattern'Identifier bnk) = pure $ IR.Pattern'Identifier bnk
resolve_for_pat decls mod (IR.Pattern'Tuple a b) = IR.Pattern'Tuple <$> resolve_for_pat decls mod a <*> resolve_for_pat decls mod b
resolve_for_pat decls mod (IR.Pattern'Named bnk subpat) = IR.Pattern'Named bnk <$> resolve_for_pat decls mod subpat
resolve_for_pat _ _ (IR.Pattern'Poison) = pure IR.Pattern'Poison

resolve_for_expr :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedExpr -> Writer [Error] ResolvedExpr
resolve_for_expr decls mod (IR.Expr'Identifier i) = IR.Expr'Identifier <$> resolve_expr_iden decls mod i
resolve_for_expr _ _ (IR.Expr'Char c) = pure $ IR.Expr'Char c
resolve_for_expr _ _ (IR.Expr'String s) = pure $ IR.Expr'String s
resolve_for_expr _ _ (IR.Expr'Int i) = pure $ IR.Expr'Int i
resolve_for_expr _ _ (IR.Expr'Float f) = pure $ IR.Expr'Float f
resolve_for_expr _ _ (IR.Expr'Bool b) = pure $ IR.Expr'Bool b

resolve_for_expr decls mod (IR.Expr'Tuple a b) = IR.Expr'Tuple <$> resolve_for_expr decls mod a <*> resolve_for_expr decls mod b

resolve_for_expr decls mod (IR.Expr'Lambda bound_names param body) = IR.Expr'Lambda bound_names <$> resolve_for_pat decls mod param <*> resolve_for_expr decls mod body

resolve_for_expr decls mod (IR.Expr'Let name_context body) = IR.Expr'Let name_context <$> resolve_for_expr decls mod body
resolve_for_expr decls mod (IR.Expr'LetRec name_context body) = IR.Expr'LetRec name_context <$> resolve_for_expr decls mod body

resolve_for_expr decls mod (IR.Expr'BinaryOps first ops) = IR.Expr'BinaryOps <$> resolve_for_expr decls mod first <*> mapM (\ (iden, rhs) -> (,) <$> resolve_expr_iden decls mod iden <*> resolve_for_expr decls mod rhs) ops

resolve_for_expr decls mod (IR.Expr'Call callee args) = IR.Expr'Call <$> resolve_for_expr decls mod callee <*> mapM (resolve_for_expr decls mod) args

resolve_for_expr decls mod (IR.Expr'If cond t f) = IR.Expr'If <$> resolve_for_expr decls mod cond <*> resolve_for_expr decls mod t <*> resolve_for_expr decls mod f
resolve_for_expr decls mod (IR.Expr'Case e arms) = IR.Expr'Case <$> resolve_for_expr decls mod e <*> mapM (\ (bound_names, pat, expr) -> (,,) bound_names <$> resolve_for_pat decls mod pat <*> resolve_for_expr decls mod expr) arms

resolve_for_expr _ _ (IR.Expr'Poison) = pure IR.Expr'Poison

resolve_expr_iden :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedExprIdentifier  -> Writer [Error] (Maybe IR.BoundNameKey)
resolve_expr_iden decls mod (nc, Location.Located _ [x]) =
    case get_value_child decls mod x of
        Right v -> pure (Just v)
        Left e -> tell [e] >> pure Nothing

resolve_expr_iden _ _ (_, i) = tell [MultiIden i] >> pure Nothing

get_value_child :: UnresolvedDeclArena -> IR.DeclKey -> Location.Located Text -> Either Error IR.BoundNameKey
get_value_child decls thing name =
-- TODO: look in parents too
    let res = case Arena.get decls thing of
            IR.Decl'Module (IR.Module (IR.NameContext _ children _)) -> Map.lookup (Location.unlocate name) children
            IR.Decl'Type _ -> Nothing -- TODO: implement children of types through impl blocks
    in case res of
        Just res -> Right res
        Nothing -> Left $ CouldNotFind Nothing name
