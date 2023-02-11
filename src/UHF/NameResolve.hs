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

type UnresolvedDecl = IR.Decl
type UnresolvedModule = IR.Module
type UnresolvedBinding = IR.Binding (Location.Located [Location.Located Text])
type UnresolvedExpr = IR.Expr (Location.Located [Location.Located Text])
type UnresolvedPattern = IR.Pattern (Location.Located [Location.Located Text])

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl IR.DeclKey
type UnresolvedBindingArena = Arena.Arena UnresolvedBinding IR.BindingKey

type ResolvedDecl = IR.Decl
type ResolvedModule = IR.Module
type ResolvedBinding = IR.Binding (Maybe IR.BoundNameKey)
type ResolvedExpr = IR.Expr (Maybe IR.BoundNameKey)
type ResolvedPattern = IR.Pattern (Maybe IR.BoundNameKey)

type ResolvedDeclArena = Arena.Arena ResolvedDecl IR.DeclKey
type ResolvedBindingArena = Arena.Arena ResolvedBinding IR.BindingKey

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

resolve :: (UnresolvedDeclArena, UnresolvedBindingArena, IR.DeclKey) -> Writer [Error] (ResolvedDeclArena, ResolvedBindingArena)
resolve (decls, values, mod) =
    Arena.transformM (resolve_for_decl decls mod) decls >>= \ decls' ->
    Arena.transformM (resolve_for_value decls mod) values >>= \ values' ->
    pure (decls', values')

resolve_for_decl :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedDecl -> Writer [Error] ResolvedDecl
resolve_for_decl _ _ (IR.Decl'Module m) = IR.Decl'Module <$> resolve_for_module m
    where
        resolve_for_module = pure
resolve_for_decl _ _ (IR.Decl'Type t) = pure $ IR.Decl'Type t

resolve_for_value :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedBinding -> Writer [Error] ResolvedBinding
resolve_for_value decls mod (IR.Binding target expr) = IR.Binding <$> resolve_for_pat decls mod target <*> resolve_for_expr decls mod expr

resolve_for_pat :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedPattern -> Writer [Error] ResolvedPattern
-- TOOD: this will change when destructuring is implemented beccause that needs to resolve constructor names
resolve_for_pat _ _ (IR.Pattern'Identifier bnk) = pure $ IR.Pattern'Identifier bnk
resolve_for_pat decls mod (IR.Pattern'Tuple items) = IR.Pattern'Tuple <$> mapM (resolve_for_pat decls mod) items
resolve_for_pat decls mod (IR.Pattern'Named bnk subpat) = IR.Pattern'Named bnk <$> resolve_for_pat decls mod subpat
resolve_for_pat _ _ (IR.Pattern'Poison) = pure IR.Pattern'Poison

resolve_for_expr :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedExpr -> Writer [Error] ResolvedExpr
-- TODO: do for rest of exprs
resolve_for_expr decls mod (IR.Expr'Identifier i) = IR.Expr'Identifier <$> resolve_iden decls mod i
resolve_for_expr _ _ (IR.Expr'Char c) = pure $ IR.Expr'Char c
resolve_for_expr _ _ (IR.Expr'String s) = pure $ IR.Expr'String s
resolve_for_expr _ _ (IR.Expr'Int i) = pure $ IR.Expr'Int i
resolve_for_expr _ _ (IR.Expr'Float f) = pure $ IR.Expr'Float f
resolve_for_expr _ _ (IR.Expr'Bool b) = pure $ IR.Expr'Bool b

resolve_iden :: UnresolvedDeclArena -> IR.DeclKey -> Location.Located [Location.Located Text] -> Writer [Error] (Maybe IR.BoundNameKey)
resolve_iden decls mod (Location.Located _ [x]) =
    case get_value_child decls mod x of
        Right v -> pure (Just v)
        Left e -> tell [e] >> pure Nothing

resolve_iden _ _ i = tell [MultiIden i] >> pure Nothing

get_value_child :: UnresolvedDeclArena -> IR.DeclKey -> Location.Located Text -> Either Error IR.BoundNameKey
get_value_child decls thing name =
    case Arena.get decls thing of
        IR.Decl'Module (IR.Module _ children) ->
            case Map.lookup (Location.unlocate name) children of
                Just res -> Right res
                Nothing -> Left $ CouldNotFind Nothing name
        IR.Decl'Type _ ->
            case Nothing of -- TODO: implement children of types through impl blocks
                Just res -> Right res
                Nothing -> Left $ CouldNotFind Nothing name

