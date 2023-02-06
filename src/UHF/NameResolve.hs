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

import qualified UHF.IR.Decl as Decl
import qualified UHF.IR.Value as Value
import qualified UHF.IR.Expr as Expr

import qualified Data.Map as Map

type UnresolvedDecl = Decl.Decl
type ResolvedDecl = Decl.Decl
type UnresolvedModule = Decl.Module
type ResolvedModule = Decl.Module
type UnresolvedValue = Value.Value (Location.Located [Location.Located Text])
type ResolvedValue = Value.Value (Maybe Value.Key)
type UnresolvedExpr = Expr.Expr (Location.Located [Location.Located Text])
type ResolvedExpr = Expr.Expr (Maybe Value.Key)

type UnresolvedDeclArena = Arena.Arena UnresolvedDecl Decl.Key
type ResolvedDeclArena = Arena.Arena ResolvedDecl Decl.Key
type UnresolvedValueArena = Arena.Arena UnresolvedValue Value.Key
type ResolvedValueArena = Arena.Arena ResolvedValue Value.Key

data Error
    = MultiIden (Location.Located [Location.Located Text])
    | UndefName (Maybe (Location.Located Text)) (Location.Located Text)

instance Diagnostic.IsError Error where
    to_error (MultiIden (Location.Located sp _)) = Diagnostic.Error Diagnostic.Codes.multi_iden $
        Diagnostic.DiagnosticContents
            (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error "paths are not supported yet"]]] -- TODO

    to_error (UndefName _ (Location.Located sp name)) = Diagnostic.Error Diagnostic.Codes.undef_name $
        -- TODO: use prev
        Diagnostic.DiagnosticContents
            (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "could not find name '" <> convert_str name <> "'"]]]

resolve :: (UnresolvedDeclArena, UnresolvedValueArena, Decl.Key) -> Writer [Error] (ResolvedDeclArena, ResolvedValueArena)
resolve (decls, values, mod) =
    Arena.transformM (resolve_for_decl decls mod) decls >>= \ decls' ->
    Arena.transformM (resolve_for_value decls mod) values >>= \ values' ->
    pure (decls', values')

resolve_for_decl :: UnresolvedDeclArena -> Decl.Key -> UnresolvedDecl -> Writer [Error] ResolvedDecl
resolve_for_decl _ _ (Decl.Decl'Module m) = Decl.Decl'Module <$> resolve_for_module m
    where
        resolve_for_module = pure

resolve_for_value :: UnresolvedDeclArena -> Decl.Key -> UnresolvedValue -> Writer [Error] ResolvedValue
resolve_for_value decls mod (Value.Value initializer) = Value.Value <$> resolve_for_expr decls mod initializer

resolve_for_expr :: UnresolvedDeclArena -> Decl.Key -> UnresolvedExpr -> Writer [Error] ResolvedExpr
resolve_for_expr decls mod (Expr.Identifier i) = Expr.Identifier <$> resolve_iden decls mod i
resolve_for_expr _ _ (Expr.CharLit c) = pure $ Expr.CharLit c
resolve_for_expr _ _ (Expr.StringLit s) = pure $ Expr.StringLit s
resolve_for_expr _ _ (Expr.IntLit i) = pure $ Expr.IntLit i
resolve_for_expr _ _ (Expr.FloatLit f) = pure $ Expr.FloatLit f
resolve_for_expr _ _ (Expr.BoolLit b) = pure $ Expr.BoolLit b

resolve_iden :: UnresolvedDeclArena -> Decl.Key -> Location.Located [Location.Located Text] -> Writer [Error] (Maybe Value.Key)
resolve_iden decls mod (Location.Located _ [x]) =
    case get_value_child decls mod x of
        Right v -> pure (Just v)
        Left e -> tell [e] >> pure Nothing

resolve_iden _ _ i = tell [MultiIden i] >> pure Nothing

get_value_child :: UnresolvedDeclArena -> Decl.Key -> Location.Located Text -> Either Error Value.Key
get_value_child decls thing name =
    case Arena.get decls thing of
        Decl.Decl'Module (Decl.Module _ children) ->
            case Map.lookup (Location.unlocate name) children of
                Just res -> Right res
                Nothing -> Left $ UndefName Nothing name

