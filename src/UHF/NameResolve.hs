module UHF.NameResolve
    ( UnresolvedDecl
    , ResolvedDecl
    , UnresolvedModule
    , ResolvedModule
    , UnresolvedValue
    , ResolvedValue
    , UnresolvedExpr
    , ResolvedExpr

    , UnresolvedDeclArena
    , ResolvedDeclArena
    , UnresolvedValueArena
    , ResolvedValueArena

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
type ResolvedDecl = IR.Decl
type UnresolvedModule = IR.Module
type ResolvedModule = IR.Module
type UnresolvedValue = IR.Value (Location.Located [Location.Located Text])
type ResolvedValue = IR.Value (Maybe IR.ValueKey)
type UnresolvedExpr = IR.Expr (Location.Located [Location.Located Text])
type ResolvedExpr = IR.Expr (Maybe IR.ValueKey)

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl IR.DeclKey
type ResolvedDeclArena = Arena.Arena ResolvedDecl IR.DeclKey
type UnresolvedValueArena = Arena.Arena UnresolvedValue IR.ValueKey
type ResolvedValueArena = Arena.Arena ResolvedValue IR.ValueKey

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

resolve :: (UnresolvedDeclArena, UnresolvedValueArena, IR.DeclKey) -> Writer [Error] (ResolvedDeclArena, ResolvedValueArena)
resolve (decls, values, mod) =
    Arena.transformM (resolve_for_decl decls mod) decls >>= \ decls' ->
    Arena.transformM (resolve_for_value decls mod) values >>= \ values' ->
    pure (decls', values')

resolve_for_decl :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedDecl -> Writer [Error] ResolvedDecl
resolve_for_decl _ _ (IR.Decl'Module m) = IR.Decl'Module <$> resolve_for_module m
    where
        resolve_for_module = pure

resolve_for_value :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedValue -> Writer [Error] ResolvedValue
resolve_for_value decls mod (IR.Value initializer) = IR.Value <$> resolve_for_expr decls mod initializer

resolve_for_expr :: UnresolvedDeclArena -> IR.DeclKey -> UnresolvedExpr -> Writer [Error] ResolvedExpr
resolve_for_expr decls mod (IR.Expr'Identifier i) = IR.Expr'Identifier <$> resolve_iden decls mod i
resolve_for_expr _ _ (IR.Expr'Char c) = pure $ IR.Expr'Char c
resolve_for_expr _ _ (IR.Expr'String s) = pure $ IR.Expr'String s
resolve_for_expr _ _ (IR.Expr'Int i) = pure $ IR.Expr'Int i
resolve_for_expr _ _ (IR.Expr'Float f) = pure $ IR.Expr'Float f
resolve_for_expr _ _ (IR.Expr'Bool b) = pure $ IR.Expr'Bool b

resolve_iden :: UnresolvedDeclArena -> IR.DeclKey -> Location.Located [Location.Located Text] -> Writer [Error] (Maybe IR.ValueKey)
resolve_iden decls mod (Location.Located _ [x]) =
    case get_value_child decls mod x of
        Right v -> pure (Just v)
        Left e -> tell [e] >> pure Nothing

resolve_iden _ _ i = tell [MultiIden i] >> pure Nothing

get_value_child :: UnresolvedDeclArena -> IR.DeclKey -> Location.Located Text -> Either Error IR.ValueKey
get_value_child decls thing name =
    case Arena.get decls thing of
        IR.Decl'Module (IR.Module _ children) ->
            case Map.lookup (Location.unlocate name) children of
                Just res -> Right res
                Nothing -> Left $ CouldNotFind Nothing name

