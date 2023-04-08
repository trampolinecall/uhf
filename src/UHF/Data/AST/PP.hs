module UHF.Data.AST.PP (pp_decls) where

import UHF.Util.Prelude

import qualified UHF.PPUtils as PPUtils
import qualified UHF.Data.AST as AST

import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

pp_decls :: [AST.Decl] -> Text
pp_decls = PPUtils.exec_pp . pp_decls'

pp_decls' :: [AST.Decl] -> PPUtils.PP ()
pp_decls' = mapM_ pp_decl

-- TODO: handle multiline things correctly everywhere

pp_decl :: AST.Decl -> PPUtils.PP ()
pp_decl (AST.Decl'Value target _ init) = pp_pattern target >> PPUtils.write " = " >> pp_expr init >> PPUtils.write ";\n" -- TODO: handle the initializer being multiline
pp_decl (AST.Decl'Data name []) = PPUtils.write "data " >> pp_iden name >> PPUtils.write " {};\n"
pp_decl (AST.Decl'Data name variants) = PPUtils.write "data " >> pp_iden name >> PPUtils.write " {\n" >> PPUtils.indent >> mapM_ pp_data_variant variants >> PPUtils.dedent >> PPUtils.write "};\n"
pp_decl (AST.Decl'TypeSyn name ty) = PPUtils.write "typesyn " >> pp_iden name >> PPUtils.write " = " >> pp_type ty >> PPUtils.write ";\n"

pp_data_variant :: AST.DataVariant -> PPUtils.PP ()
pp_data_variant (AST.DataVariant'Anon name fields) = pp_iden name >> PPUtils.write "(" >> pp_comma_separated pp_type fields >> PPUtils.write ")" >> PPUtils.write ";\n"
pp_data_variant (AST.DataVariant'Named name fields) = pp_iden name >> PPUtils.write " {\n" >> PPUtils.indent >> mapM_ (\ (name, ty) -> pp_iden name >> PPUtils.write ": " >> pp_type ty >> PPUtils.write ";\n") fields >> PPUtils.dedent >> PPUtils.write "};\n"

-- TODO: precedence
pp_type :: AST.Type -> PPUtils.PP ()
pp_type (AST.Type'Identifier iden) = pp_iden iden
pp_type (AST.Type'Tuple _ items) = PPUtils.write "(" >> pp_comma_separated pp_type items >> PPUtils.write ")"
pp_type (AST.Type'Hole _ name) = PPUtils.write "?" >> pp_iden name
pp_type (AST.Type'Forall _ names subty) = PPUtils.write "#(" >> pp_comma_separated pp_iden names >> PPUtils.write ") " >> pp_type subty
pp_type (AST.Type'Apply _ callee args) = pp_type callee >> PPUtils.write "#(" >> pp_comma_separated pp_type args >> PPUtils.write ")"
pp_type (AST.Type'Wild _) = PPUtils.write "_"

-- TODO: precedence
pp_expr :: AST.Expr -> PPUtils.PP ()
pp_expr (AST.Expr'Identifier iden) = pp_iden iden
pp_expr (AST.Expr'Char _ c) = PPUtils.write $ show c
pp_expr (AST.Expr'String _ s) = PPUtils.write $ show s -- TODO: deal with escapes properly / print token?
pp_expr (AST.Expr'Int _ i) = PPUtils.write $ show i
pp_expr (AST.Expr'Float _ (n :% d)) = PPUtils.write $ "(" <> show n <> " / " <> show d <> ")"
pp_expr (AST.Expr'Bool _ b) = PPUtils.write $ if b then "true" else "false"
pp_expr (AST.Expr'Tuple _ items) = PPUtils.write "(" >> pp_comma_separated pp_expr items >> PPUtils.write ")"
pp_expr (AST.Expr'Lambda _ args body) = PPUtils.write "\\ (" >> pp_comma_separated pp_pattern args >> PPUtils.write ") -> " >> pp_expr body
pp_expr (AST.Expr'Let _ decls res) = pp_let "let" decls res
pp_expr (AST.Expr'LetRec _ decls res) = pp_let "letrec" decls res
pp_expr (AST.Expr'BinaryOps _ first ops) = pp_expr first >> mapM_ (\ (op, rhs) -> pp_iden op >> PPUtils.write " " >> pp_expr rhs) ops
pp_expr (AST.Expr'Call _ callee args) = pp_expr callee >> PPUtils.write "(" >> pp_comma_separated pp_expr args >> PPUtils.write ")"
pp_expr (AST.Expr'If _ _ cond true false) = PPUtils.write "if " >> pp_expr cond >> PPUtils.write " then " >> pp_expr true >> PPUtils.write " else " >> pp_expr false
pp_expr (AST.Expr'Case _ _ e arms) = PPUtils.write "case " >> pp_expr e >> PPUtils.write " {\n" >> PPUtils.indent >> mapM_ (\ (pat, expr) -> pp_pattern pat >> PPUtils.write " -> " >> pp_expr expr >> PPUtils.write ";\n") arms >> PPUtils.dedent >> PPUtils.write "}\n"
pp_expr (AST.Expr'TypeAnnotation _ ty e) = PPUtils.write ":" >> pp_type ty >> PPUtils.write ": " >> pp_expr e -- TODO: add trailing : to parser
pp_expr (AST.Expr'Forall _ tys e) = PPUtils.write "#(" >> pp_comma_separated pp_iden tys >> PPUtils.write ") " >> pp_expr e
pp_expr (AST.Expr'TypeApply _ e tys) = pp_expr e >> PPUtils.write "#(" >> pp_comma_separated pp_type tys >> PPUtils.write ")"
pp_expr (AST.Expr'Hole _ name) = PPUtils.write "?" >> pp_iden name

pp_let :: Text -> [AST.Decl] -> AST.Expr -> PPUtils.PP ()
pp_let let_str [] res = PPUtils.write let_str >> PPUtils.write " {}\n" >> pp_expr res
pp_let let_str [decl] res = PPUtils.write let_str >> PPUtils.write " " >> pp_decl decl >> pp_expr res
pp_let let_str decls res = PPUtils.write let_str >> PPUtils.write " {\n" >> PPUtils.indent >> mapM_ pp_decl decls >> PPUtils.dedent >> PPUtils.write "}\n" >> pp_expr res

pp_pattern :: AST.Pattern -> PPUtils.PP ()
pp_pattern (AST.Pattern'Identifier i) = pp_iden i
pp_pattern (AST.Pattern'Wildcard _) = PPUtils.write "_"
pp_pattern (AST.Pattern'Tuple _ items) = PPUtils.write "(" >> pp_comma_separated pp_pattern items >> PPUtils.write ")"
pp_pattern (AST.Pattern'Named _ name _ subpat) = pp_iden name >> PPUtils.write "@" >> pp_pattern subpat

pp_iden :: Located [Located Text] -> PPUtils.PP ()
pp_iden (Located _ items) = PPUtils.write $ Text.intercalate "::" (map unlocate items)

pp_comma_separated :: (d -> PPUtils.PP ()) -> [d] -> PPUtils.PP ()
pp_comma_separated pp items =
    let pped = map pp items
        any_multiline = any PPUtils.is_multiline pped
    in if null pped
        then pure ()
        else if any_multiline
            then PPUtils.newline >> PPUtils.indent >> mapM (>> PPUtils.write ",\n") pped >> PPUtils.dedent
            else intercalate_commas True pped -- true if first
    where
        intercalate_commas _ [] = pure ()
        intercalate_commas True (x:more) = x >> intercalate_commas False more
        intercalate_commas False (x:more) = PPUtils.write ", " >> x >> intercalate_commas False more
