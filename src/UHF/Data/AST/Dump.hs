module UHF.Data.AST.Dump (dump) where

import UHF.Util.Prelude

import qualified UHF.PP as PP
import qualified UHF.Data.AST as AST

import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

-- TODO: figure out how to automate this?

dump :: [AST.Decl] -> Text
dump = PP.render . dump_decl_list

dump_decl_list :: [AST.Decl] -> PP.Token
dump_decl_list = PP.bracketed_comma_list PP.Consistent . map dump_decl

dump_decl :: AST.Decl -> PP.Token
dump_decl (AST.Decl'Value target _ init) = dump_struct "Decl'Value" [("target", dump_pattern target), ("init", dump_expr init)]
dump_decl (AST.Decl'Data name type_params variants) = dump_struct "Decl'Data" [("name", dump_identifier name), ("type_params", dump_list dump_identifier type_params), ("variants", dump_list dump_data_variant variants)]
dump_decl (AST.Decl'TypeSyn name ty) = dump_struct "Decl'TypeSyn" [("name", dump_identifier name), ("ty", dump_type ty)]

dump_data_variant :: AST.DataVariant -> PP.Token
dump_data_variant (AST.DataVariant'Anon name fields) = dump_struct "DataVariant'Anon" [("name", dump_identifier name), ("fields", dump_list dump_type fields)]
dump_data_variant (AST.DataVariant'Named name fields) = dump_struct "DataVariant'Named" [("name", dump_identifier name), ("fields", dump_list (\ (name, ty) -> PP.List [dump_identifier name, ": ", dump_type ty]) fields)]

dump_type :: AST.Type -> PP.Token
dump_type (AST.Type'Identifier iden) = dump_struct "Type'Identifier" [("iden", dump_identifier iden)]
dump_type (AST.Type'Tuple _ items) = dump_struct "Type'Tuple" [("items", dump_list dump_type items)]
dump_type (AST.Type'Hole _ name) = dump_struct "Type'Hole" [("name", dump_identifier name)]
dump_type (AST.Type'Forall _ tys ty) = dump_struct "Type'Forall" [("new", dump_list dump_identifier tys), ("ty", dump_type ty)]
dump_type (AST.Type'Apply _ ty tys) = dump_struct "Type'Apply" [("ty", dump_type ty), ("args", dump_list dump_type tys)]
dump_type (AST.Type'Wild _) = dump_struct "Type'Wild" []

dump_expr :: AST.Expr -> PP.Token
dump_expr (AST.Expr'Identifier iden) = dump_struct "Expr'Identifier" [("iden", dump_identifier iden)]
dump_expr (AST.Expr'Char _ c) = dump_struct "Expr'Char" [("c", PP.String $ show c)]
dump_expr (AST.Expr'String _ s) = dump_struct "Expr'String" [("s", PP.String $ show s)]
dump_expr (AST.Expr'Int _ i) = dump_struct "Expr'Int" [("i", PP.String $ show i)]
dump_expr (AST.Expr'Float _ f) = dump_struct "Expr'Float" [("f", PP.String $ show f)]
dump_expr (AST.Expr'Bool _ b) = dump_struct "Expr'Bool" [("b", PP.String $ if b then "true" else "false")]
dump_expr (AST.Expr'Tuple _ items) = dump_struct "Expr'Tuple" [("items", dump_list dump_expr items)]
dump_expr (AST.Expr'Lambda _ args body) = dump_struct "Expr'Lambda" [("args", dump_list dump_pattern args), ("body", dump_expr body)]
dump_expr (AST.Expr'Let _ decls res) = dump_struct "Expr'Let" [("decls", dump_decl_list decls), ("res", dump_expr res)]
dump_expr (AST.Expr'LetRec _ decls res) = dump_struct "Expr'LetRec" [("decls", dump_decl_list decls), ("res", dump_expr res)]
dump_expr (AST.Expr'BinaryOps _ first ops) = dump_struct "Expr'BinaryOps" [("first", dump_expr first), ("ops", dump_list (\ (op, rhs) -> PP.List [dump_identifier op, " ", dump_expr rhs]) ops)]
dump_expr (AST.Expr'Call _ callee args) = dump_struct "Expr'Call" [("callee", dump_expr callee), ("args", dump_list dump_expr args)]
dump_expr (AST.Expr'If _ _ cond true false) = dump_struct "Expr'If" [("cond", dump_expr cond), ("true", dump_expr true), ("false", dump_expr false)]
dump_expr (AST.Expr'Case _ _ e arms) = dump_struct "Expr'Case" [("e", dump_expr e), ("arms", dump_list (\ (pat, expr) -> PP.List [dump_pattern pat, " -> ", dump_expr expr]) arms)]
dump_expr (AST.Expr'TypeAnnotation _ ty e) = dump_struct "Expr'TypeAnnotation" [("ty", dump_type ty), ("e", dump_expr e)]
dump_expr (AST.Expr'Forall _ tys e) = dump_struct "Expr'Forall" [("new", dump_list dump_identifier tys), ("e", dump_expr e)]
dump_expr (AST.Expr'TypeApply _ e tys) = dump_struct "Expr'TypeApply" [("e", dump_expr e), ("args", dump_list dump_type tys)]
dump_expr (AST.Expr'Hole _ name) = dump_struct "Expr'Hole" [("name", dump_identifier name)]

dump_pattern :: AST.Pattern -> PP.Token
dump_pattern (AST.Pattern'Identifier i) = dump_struct "Pattern'Identifier" [("i", dump_identifier i)]
dump_pattern (AST.Pattern'Wildcard _) = dump_struct "Pattern'Wildcard" []
dump_pattern (AST.Pattern'Tuple _ items) = dump_struct "Pattern'Tuple" [("items", dump_list dump_pattern items)]
dump_pattern (AST.Pattern'Named _ name _ subpat) = dump_struct "Pattern'Named" [("name", dump_identifier name), ("subpat", dump_pattern subpat)]
dump_pattern (AST.Pattern'AnonADTVariant _ name fields) = dump_struct "Pattern'AnonADTVariant" [("variant", dump_identifier name), ("fields", dump_list dump_pattern fields)]
dump_pattern (AST.Pattern'NamedADTVariant _ name fields) = dump_struct "Pattern'NamedADTVariant" [("variant", dump_identifier name), ("fields", dump_list (\ (field_name, field_pat) -> PP.List [dump_identifier field_name, " = ", dump_pattern field_pat]) fields)]

dump_identifier :: Located [Located Text] -> PP.Token
dump_identifier (Located _ items) = PP.String $ Text.intercalate "::" (map unlocate items)

dump_struct :: Text -> [(Text, PP.Token)] -> PP.Token
dump_struct name fields = PP.List [PP.String name, " ", PP.braced_comma_list PP.Consistent (map dump_field fields)]
    where
        dump_field (name, value) = PP.List [PP.String name, " = ", value]

dump_list :: (d -> PP.Token) -> [d] -> PP.Token
dump_list dump = PP.bracketed_comma_list PP.Inconsistent . map dump
