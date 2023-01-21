module UHF.ASTToIR where

import UHF.Util.Prelude

import qualified UHF.AST as AST
import qualified UHF.IR.Decl as Decl
import qualified UHF.IR.Value as Value
import qualified UHF.IR.Expr as Expr

import qualified UHF.IO.Location as Location

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Map as Map

data Error = Redefinition (Location.Located Text)

instance Diagnostic.IsDiagnostic Error where
    to_diagnostic (Redefinition (Location.Located sp name)) =
        Diagnostic.Diagnostic Codes.symbol_redefinition (Just sp)
            [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "redefinition of '" <> convert_str name <> "'"]]]

convert :: [AST.Decl] -> Writer [Error] (Decl.Module (Location.Located [Location.Located Text]))
convert decls =
    convert_to_maps decls >>= \ (decl_map, value_map) ->
    pure (Decl.Module decl_map value_map)

convert_to_maps :: [AST.Decl] -> Writer [Error] (Map.Map Text (Decl.Decl (Location.Located [Location.Located Text])), Map.Map Text (Value.Value (Location.Located [Location.Located Text])))
convert_to_maps decls =
    foldl'
        (\ last_map cur_decl ->
            last_map >>= \ (last_decl_map, last_value_map) ->
            case cur_decl of
                AST.Decl'Binding l_name@(Location.Located _ name) expr ->
                    case Map.lookup name last_value_map of
                        Just _ -> tell [Redefinition l_name] >> pure (last_decl_map, last_value_map)
                        _ -> pure (last_decl_map, Map.insert name (convert_expr_to_value expr) last_value_map)
            )
        (pure (Map.empty, Map.empty))
        decls

convert_expr_to_value :: AST.Expr -> Value.Value (Location.Located [Location.Located Text])
convert_expr_to_value = Value.Value . convert_expr

convert_expr :: AST.Expr -> Expr.Expr (Location.Located [Location.Located Text])
convert_expr (AST.Expr'Identifier iden) = Expr.Identifier iden
convert_expr (AST.Expr'CharLit c) = Expr.CharLit c
convert_expr (AST.Expr'StringLit s) = Expr.StringLit s
convert_expr (AST.Expr'IntLit i) = Expr.IntLit i
convert_expr (AST.Expr'FloatLit f) = Expr.FloatLit f
convert_expr (AST.Expr'BoolLit b) = Expr.BoolLit b
