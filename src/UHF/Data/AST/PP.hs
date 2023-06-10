module UHF.Data.AST.PP (pp_decls) where

import UHF.Util.Prelude

import qualified UHF.PP as PP
import qualified UHF.Data.AST as AST

import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

pp_decls :: [AST.Decl] -> Text
pp_decls = PP.render . pp_decls'

pp_decls' :: [AST.Decl] -> PP.Token
pp_decls' = PP.flat_block . map pp_decl

pp_decl :: AST.Decl -> PP.Token
pp_decl (AST.Decl'Value target _ init) = PP.List [pp_pattern target, " = ", pp_expr init, ";"]
pp_decl (AST.Decl'Data name params variants) =
    let variants' = PP.braced_block $ map pp_data_variant variants
        params'
            | null params = PP.List [""]
            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map pp_iden params)]
    in PP.List ["data ", pp_iden name, params', " ", variants', ";"]
pp_decl (AST.Decl'TypeSyn name ty) = PP.List ["typesyn ", pp_iden name, " = ", pp_type ty, ";"]

pp_data_variant :: AST.DataVariant -> PP.Token
pp_data_variant (AST.DataVariant'Anon name fields) = PP.List [pp_iden name, PP.parenthesized_comma_list PP.Inconsistent $ map pp_type fields, ";"]
pp_data_variant (AST.DataVariant'Named name fields) = PP.List [pp_iden name, " ", PP.braced_block $ map (\ (name, ty) -> PP.List [pp_iden name, ": ", pp_type ty, ";"]) fields, ";"]

pp_type :: AST.Type -> PP.Token
pp_type = level1
    where
        -- TODO: fix infinite recursion when adding new thing and forgetting to add to here
        level1 (AST.Type'Forall _ names subty) = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_iden names, " ", level1 subty]
        level1 t = level2 t

        level2 (AST.Type'Function _ arg res) = PP.List [level3 arg, " -> ", level2 res]
        level2 t = level3 t

        level3 (AST.Type'Apply _ callee args) = PP.List [level3 callee, "#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_type args]
        level3 t = level4 t

        level4 (AST.Type'Identifier iden) = pp_iden iden
        level4 (AST.Type'Tuple _ items) = PP.parenthesized_comma_list PP.Inconsistent $ map pp_type items
        level4 (AST.Type'Hole _ name) = PP.List ["?", pp_iden name]
        level4 (AST.Type'Wild _) = PP.List ["_"]
        level4 t = PP.List ["(", pp_type t, ")"]

-- TODO: precedence
pp_expr :: AST.Expr -> PP.Token
pp_expr (AST.Expr'Identifier iden) = PP.FirstOnLineIfMultiline $ pp_iden iden
pp_expr (AST.Expr'Char _ c) = PP.FirstOnLineIfMultiline $ PP.String $ show c
pp_expr (AST.Expr'String _ s) = PP.FirstOnLineIfMultiline $ PP.String $ show s -- TODO: deal with escapes properly / print token?
pp_expr (AST.Expr'Int _ i) = PP.FirstOnLineIfMultiline $ PP.String $ show i
pp_expr (AST.Expr'Float _ (n :% d)) = PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> " / " <> show d <> ")"
pp_expr (AST.Expr'Bool _ b) = PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false"
pp_expr (AST.Expr'Tuple _ items) = PP.parenthesized_comma_list PP.Inconsistent $ map pp_expr items
pp_expr (AST.Expr'Lambda _ args body) = PP.FirstOnLineIfMultiline $ PP.List ["\\ ", PP.parenthesized_comma_list PP.Inconsistent $ map pp_pattern args, " -> ", pp_expr body]
pp_expr (AST.Expr'Let _ decls res) = pp_let "let" decls res
pp_expr (AST.Expr'LetRec _ decls res) = pp_let "letrec" decls res
pp_expr (AST.Expr'BinaryOps _ first ops) = PP.List [pp_expr first, PP.indented_block $ map (\ (op, rhs) -> PP.List [pp_iden op, " ", pp_expr rhs]) ops]
pp_expr (AST.Expr'Call _ callee args) = PP.List [pp_expr callee, PP.parenthesized_comma_list PP.Inconsistent $ map pp_expr args]
pp_expr (AST.Expr'If _ _ cond true false) = PP.FirstOnLineIfMultiline $ PP.List ["if ", pp_expr cond, " then ", pp_expr true, " else ", pp_expr false]
pp_expr (AST.Expr'Case _ _ e arms) = PP.List ["case ", pp_expr e, " ", PP.braced_block $ map (\ (pat, expr) -> PP.List [pp_pattern pat, " -> ", pp_expr expr, ";"]) arms]
pp_expr (AST.Expr'TypeAnnotation _ ty e) = PP.List [":", pp_type ty, ": ", pp_expr e] -- TODO: add trailing : to parser
pp_expr (AST.Expr'Forall _ tys e) = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_iden tys, " ", pp_expr e]
pp_expr (AST.Expr'TypeApply _ e tys) = PP.List [pp_expr e, "#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_type tys]
pp_expr (AST.Expr'Hole _ name) = PP.FirstOnLineIfMultiline $ PP.List ["?", pp_iden name]

pp_let :: Text -> [AST.Decl] -> AST.Expr -> PP.Token
pp_let let_str [decl] res = PP.FirstOnLineIfMultiline $ PP.List [PP.String let_str, " ", pp_decl decl, "\n", pp_expr res]
pp_let let_str decls res = PP.FirstOnLineIfMultiline $ PP.List [PP.String let_str, " ", PP.braced_block $ map pp_decl decls, "\n", pp_expr res]

pp_pattern :: AST.Pattern -> PP.Token
pp_pattern (AST.Pattern'Identifier i) = PP.List [pp_iden i]
pp_pattern (AST.Pattern'Wildcard _) = PP.List ["_"]
pp_pattern (AST.Pattern'Tuple _ items) = PP.parenthesized_comma_list PP.Inconsistent $ map pp_pattern items
pp_pattern (AST.Pattern'Named _ name _ subpat) = PP.List [pp_iden name, "@", pp_pattern subpat]
pp_pattern (AST.Pattern'AnonADTVariant _ variant fields) = PP.List [pp_iden variant, PP.parenthesized_comma_list PP.Inconsistent (map pp_pattern fields)]
pp_pattern (AST.Pattern'NamedADTVariant _ variant fields) = PP.List [pp_iden variant, PP.braced_block $ map (\ (field_name, field_pat) -> PP.List [pp_iden field_name, " = ", pp_pattern field_pat, ";"]) fields]

pp_iden :: Located [Located Text] -> PP.Token
pp_iden (Located _ items) = PP.List [PP.String $ Text.intercalate "::" (map unlocate items)]

