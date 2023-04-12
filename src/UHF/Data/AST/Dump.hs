module UHF.Data.AST.Dump (dump) where

import UHF.Util.Prelude

import qualified UHF.PPUtils as PPUtils
import qualified UHF.Data.AST as AST

import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

-- TODO: figure out how to automate this?

dump :: [AST.Decl] -> Text
dump = PPUtils.exec_pp . dump_decl_list

dump_decl_list :: [AST.Decl] -> PPUtils.PP ()
dump_decl_list = mapM_ (\ decl -> dump_decl decl >> PPUtils.newline)

dump_decl :: AST.Decl -> PPUtils.PP ()
dump_decl (AST.Decl'Value target _ init) = dump_struct "Decl'Value" [("target", dump_pattern target), ("init", dump_expr init)]
dump_decl (AST.Decl'Data name type_params variants) = dump_struct "Decl'Data" [("name", dump_identifier name), ("type_params", dump_list dump_identifier type_params), ("variants", dump_list dump_data_variant variants)]
dump_decl (AST.Decl'TypeSyn name ty) = dump_struct "Decl'TypeSyn" [("name", dump_identifier name), ("ty", dump_type ty)]

dump_data_variant :: AST.DataVariant -> PPUtils.PP ()
dump_data_variant (AST.DataVariant'Anon name fields) = dump_struct "DataVariant'Anon" [("name", dump_identifier name), ("fields", dump_list dump_type fields)]
dump_data_variant (AST.DataVariant'Named name fields) = dump_struct "DataVariant'Named" [("name", dump_identifier name), ("fields", dump_list (\ (name, ty) -> dump_identifier name >> PPUtils.write ": " >> dump_type ty) fields)]

dump_type :: AST.Type -> PPUtils.PP ()
dump_type (AST.Type'Identifier iden) = dump_struct "Type'Identifier" [("iden", dump_identifier iden)]
dump_type (AST.Type'Tuple _ items) = dump_struct "Type'Tuple" [("items", dump_list dump_type items)]
dump_type (AST.Type'Hole _ name) = dump_struct "Type'Hole" [("name", dump_identifier name)]
dump_type (AST.Type'Forall _ tys ty) = dump_struct "Type'Forall" [("new", dump_list dump_identifier tys), ("ty", dump_type ty)]
dump_type (AST.Type'Apply _ ty tys) = dump_struct "Type'Apply" [("ty", dump_type ty), ("args", dump_list dump_type tys)]
dump_type (AST.Type'Wild _) = dump_struct "Type'Wild" []

dump_expr :: AST.Expr -> PPUtils.PP ()
dump_expr (AST.Expr'Identifier iden) = dump_struct "Expr'Identifier" [("iden", dump_identifier iden)]
dump_expr (AST.Expr'Char _ c) = dump_struct "Expr'Char" [("c", PPUtils.write $ show c)]
dump_expr (AST.Expr'String _ s) = dump_struct "Expr'String" [("s", PPUtils.write $ show s)]
dump_expr (AST.Expr'Int _ i) = dump_struct "Expr'Int" [("i", PPUtils.write $ show i)]
dump_expr (AST.Expr'Float _ f) = dump_struct "Expr'Float" [("f", PPUtils.write $ show f)]
dump_expr (AST.Expr'Bool _ b) = dump_struct "Expr'Bool" [("b", PPUtils.write $ if b then "true" else "false")]
dump_expr (AST.Expr'Tuple _ items) = dump_struct "Expr'Tuple" [("items", dump_list dump_expr items)]
dump_expr (AST.Expr'Lambda _ args body) = dump_struct "Expr'Lambda" [("args", dump_list dump_pattern args), ("body", dump_expr body)]
dump_expr (AST.Expr'Let _ decls res) = dump_struct "Expr'Let" [("decls", dump_decl_list decls), ("res", dump_expr res)]
dump_expr (AST.Expr'LetRec _ decls res) = dump_struct "Expr'LetRec" [("decls", dump_decl_list decls), ("res", dump_expr res)]
dump_expr (AST.Expr'BinaryOps _ first ops) = dump_struct "Expr'BinaryOps" [("first", dump_expr first), ("ops", dump_list (\ (op, rhs) -> dump_identifier op >> PPUtils.write " " >> dump_expr rhs) ops)]
dump_expr (AST.Expr'Call _ callee args) = dump_struct "Expr'Call" [("callee", dump_expr callee), ("args", dump_list dump_expr args)]
dump_expr (AST.Expr'If _ _ cond true false) = dump_struct "Expr'If" [("cond", dump_expr cond), ("true", dump_expr true), ("false", dump_expr false)]
dump_expr (AST.Expr'Case _ _ e arms) = dump_struct "Expr'Case" [("e", dump_expr e), ("arms", dump_list (\ (pat, expr) -> dump_pattern pat >> PPUtils.write " -> " >> dump_expr expr) arms)]
dump_expr (AST.Expr'TypeAnnotation _ ty e) = dump_struct "Expr'TypeAnnotation" [("ty", dump_type ty), ("e", dump_expr e)]
dump_expr (AST.Expr'Forall _ tys e) = dump_struct "Expr'Forall" [("new", dump_list dump_identifier tys), ("e", dump_expr e)]
dump_expr (AST.Expr'TypeApply _ e tys) = dump_struct "Expr'TypeApply" [("e", dump_expr e), ("args", dump_list dump_type tys)]
dump_expr (AST.Expr'Hole _ name) = dump_struct "Expr'Hole" [("name", dump_identifier name)]

dump_pattern :: AST.Pattern -> PPUtils.PP ()
dump_pattern (AST.Pattern'Identifier i) = dump_struct "Pattern'Identifier" [("i", dump_identifier i)]
dump_pattern (AST.Pattern'Wildcard _) = dump_struct "Pattern'Wildcard" []
dump_pattern (AST.Pattern'Tuple _ items) = dump_struct "Pattern'Tuple" [("items", dump_list dump_pattern items)]
dump_pattern (AST.Pattern'Named _ name _ subpat) = dump_struct "Pattern'Named" [("name", dump_identifier name), ("subpat", dump_pattern subpat)]

dump_identifier :: Located [Located Text] -> PPUtils.PP ()
dump_identifier (Located _ items) = PPUtils.write $ Text.intercalate "::" (map unlocate items)

dump_struct :: Text -> [(Text, PPUtils.PP ())] -> PPUtils.PP ()
dump_struct name fields =
    case map dump_field fields of
        [] -> PPUtils.write name >> PPUtils.write " {}"
        [field]
            | not $ PPUtils.is_multiline field -> PPUtils.write name >> PPUtils.write " { " >> field >> PPUtils.write " }"

        fields -> PPUtils.write name >> PPUtils.write " {\n" >> PPUtils.indent >> mapM (>> PPUtils.write ",\n") fields >> PPUtils.dedent >> PPUtils.write "}"
    where
        dump_field (name, value) = PPUtils.write name >> PPUtils.write " = " >> value

dump_list :: (d -> PPUtils.PP ()) -> [d] -> PPUtils.PP ()
dump_list dump items =
    let dumped = map dump items
        any_multiline = any PPUtils.is_multiline dumped
    in if null dumped
        then PPUtils.write "[]"
        else if any_multiline
            then PPUtils.write "[\n" >> PPUtils.indent >> mapM (>> PPUtils.write ",\n") dumped >> PPUtils.dedent >> PPUtils.write "]" -- true if first
            else PPUtils.write "[" >> intercalate_commas True dumped >> PPUtils.write "]" -- true if first
    where
        intercalate_commas _ [] = pure ()
        intercalate_commas True (x:more) = x >> intercalate_commas False more
        intercalate_commas False (x:more) = PPUtils.write ", " >> x >> intercalate_commas False more
