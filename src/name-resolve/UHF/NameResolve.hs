module UHF.NameResolve(resolve) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.IR.Decl as Decl
import qualified UHF.IR.Value as Value
import qualified UHF.IR.Expr as Expr

data Error

instance Diagnostic.IsDiagnostic Error where

resolve :: Decl.Module (Location.Located [(Location.Located Text)]) -> Writer [Error] (Decl.Module (Location.Located Value.ResolvedValue))
resolve = resolve_for_module

resolve_for_decl :: Decl.Decl (Location.Located [(Location.Located Text)]) -> Writer [Error] (Decl.Decl (Location.Located Value.ResolvedValue))
resolve_for_decl (Decl.Decl'Module m) = Decl.Decl'Module <$> resolve_for_module m

resolve_for_module :: Decl.Module (Location.Located [(Location.Located Text)]) -> Writer [Error] (Decl.Module (Location.Located Value.ResolvedValue))
resolve_for_module (Decl.Module decl_children value_children) =
    mapM resolve_for_decl decl_children >>= \ decl_children ->
    mapM resolve_for_value value_children >>= \ value_children ->
    pure (Decl.Module decl_children value_children)

resolve_for_value :: Value.Value (Location.Located [(Location.Located Text)]) -> Writer [Error] (Value.Value (Location.Located Value.ResolvedValue))
resolve_for_value (Value.Value initializer) = Value.Value <$> resolve_for_expr initializer

resolve_for_expr :: Expr.Expr (Location.Located [Location.Located Text]) -> Writer [Error] (Expr.Expr (Location.Located Value.ResolvedValue))
resolve_for_expr (Expr.Identifier i) = pure $ Expr.Identifier (todo i)
resolve_for_expr (Expr.CharLit c) = pure $ Expr.CharLit c
resolve_for_expr (Expr.StringLit s) = pure $ Expr.StringLit s
resolve_for_expr (Expr.IntLit i) = pure $ Expr.IntLit i
resolve_for_expr (Expr.FloatLit f) = pure $ Expr.FloatLit f
resolve_for_expr (Expr.BoolLit b) = pure $ Expr.BoolLit b
