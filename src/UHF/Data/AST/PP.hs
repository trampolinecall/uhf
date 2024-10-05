module UHF.Data.AST.PP (pp_decls) where

import UHF.Prelude

import UHF.Source.Located (Located (..))
import qualified UHF.Data.AST as AST
import qualified UHF.Data.Token as Token
import qualified UHF.PP as PP
import qualified UHF.PP.Precedence as PP.Precedence

-- TODO: fix precedences after grammar is overhauled
-- TODO: also fix precedences in every other PP module too

pp_decls :: [AST.Decl] -> Text
pp_decls = PP.render . pp_decls'

pp_decls' :: [AST.Decl] -> PP.Token
pp_decls' = PP.flat_block . map pp_decl

pp_decl :: AST.Decl -> PP.Token
pp_decl (AST.Decl'Value _ target _ init) = PP.List [pp_pattern target, " = ", pp_expr init, ";"]
pp_decl (AST.Decl'Data _ name params variants) =
    let variants' = PP.braced_block $ map pp_data_variant variants
        params'
            | null params = PP.List [""]
            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map pp_aiden params)]
    in PP.List ["data ", pp_aiden name, params', " ", variants', ";"]
pp_decl (AST.Decl'TypeSyn _ name ty) = PP.List ["typesyn ", pp_aiden name, " = ", pp_type ty, ";"]

pp_data_variant :: AST.DataVariant -> PP.Token
pp_data_variant (AST.DataVariant'Anon name fields) = PP.List [pp_aiden name, PP.parenthesized_comma_list PP.Inconsistent $ map pp_type fields, ";"]
pp_data_variant (AST.DataVariant'Named name fields) = PP.List [pp_aiden name, " ", PP.braced_block $ map (\ (name, ty) -> PP.List [pp_aiden name, ": ", pp_type ty, ";"]) fields, ";"]

pp_type :: AST.Type -> PP.Token
pp_type = PP.Precedence.pp_precedence levels PP.Precedence.parenthesize
    where
        levels (AST.Type'Forall _ names subty) = (1, \ cur _ -> PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_aiden names, " ", cur subty])
        levels (AST.Type'Function _ arg res) = (2, \ cur next -> PP.List [next arg, " -> ", cur res])
        levels (AST.Type'Apply _ callee args) = (3, \ cur _ -> PP.List [cur callee, "#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_type args])
        levels (AST.Type'Get _ prev next) = (3, \ cur _ -> PP.List [cur prev, "::", pp_aiden next])
        levels (AST.Type'Refer iden) = (4, \ _ _ -> pp_aiden iden)
        levels (AST.Type'Tuple _ items) = (4, \ _ _ -> PP.parenthesized_comma_list PP.Inconsistent $ map pp_type items)
        levels (AST.Type'Hole _ name) = (4, \ _ _ -> PP.List ["?", pp_aiden name])
        levels (AST.Type'Wild _) = (4, \ _ _ -> PP.List ["_"])

pp_expr :: AST.Expr -> PP.Token
pp_expr = PP.Precedence.pp_precedence levels PP.Precedence.parenthesize
    where
        levels (AST.Expr'BinaryOps _ first ops) = (0, \ _ next -> PP.List [next first, PP.indented_block $ map (\ (op, rhs) -> PP.List [pp_operator op, " ", next rhs]) ops])

        levels (AST.Expr'Call _ callee args) = (1, \ cur _ -> PP.List [cur callee, PP.parenthesized_comma_list PP.Inconsistent $ map pp_expr args])
        levels (AST.Expr'TypeApply _ e tys) = (1, \ cur _ -> PP.List [cur e, "#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_type tys])

        levels (AST.Expr'ReferAlpha _ t iden) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ pp_path_or_single_iden pp_aiden t iden)
        levels (AST.Expr'Hole _ name) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.List ["?", pp_aiden name])
        levels (AST.Expr'Char _ c) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.String $ show c)
        levels (AST.Expr'String _ s) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.String $ show s) -- TODO: deal with escapes properly / print token?
        levels (AST.Expr'Int _ i) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.String $ show i)
        levels (AST.Expr'Float _ (n :% d)) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.String $ "(" <> show n <> " / " <> show d <> ")")
        levels (AST.Expr'Bool _ b) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.String $ if b then "true" else "false")
        levels (AST.Expr'Tuple _ items) = (2, \ _ _ -> PP.parenthesized_comma_list PP.Inconsistent $ map pp_expr items)

        levels (AST.Expr'Lambda _ args body) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.List ["\\ ", PP.parenthesized_comma_list PP.Inconsistent $ map pp_pattern args, " -> ", pp_expr body])

        levels (AST.Expr'Let _ decls res) = (2, \ _ _ -> pp_let "let" decls res)
        levels (AST.Expr'LetRec _ decls res) = (2, \ _ _ -> pp_let "letrec" decls res)

        levels (AST.Expr'If _ _ cond true false) = (2, \ _ _ -> PP.FirstOnLineIfMultiline $ PP.List ["if ", pp_expr cond, " then ", pp_expr true, " else ", pp_expr false])
        levels (AST.Expr'Match _ _ e arms) = (2, \ _ _ -> PP.List ["match ", pp_expr e, " ", PP.braced_block $ map (\ (pat, expr) -> PP.List [pp_pattern pat, " -> ", pp_expr expr, ";"]) arms])

        levels (AST.Expr'TypeAnnotation _ ty e) = (2, \ _ _ -> PP.List [":", pp_type ty, ": ", pp_expr e])

        levels (AST.Expr'Forall _ tys e) = (2, \ _ _ -> PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent $ map pp_aiden tys, " ", pp_expr e])

pp_let :: Text -> [AST.Decl] -> AST.Expr -> PP.Token
pp_let let_str [decl] res = PP.FirstOnLineIfMultiline $ PP.List [PP.String let_str, " ", pp_decl decl, "\n", pp_expr res]
pp_let let_str decls res = PP.FirstOnLineIfMultiline $ PP.List [PP.String let_str, " ", PP.braced_block $ map pp_decl decls, "\n", pp_expr res]

pp_pattern :: AST.Pattern -> PP.Token
pp_pattern (AST.Pattern'AlphaVar i) = PP.List [pp_aiden i]
pp_pattern (AST.Pattern'Wildcard _) = PP.List ["_"]
pp_pattern (AST.Pattern'Tuple _ items) = PP.parenthesized_comma_list PP.Inconsistent $ map pp_pattern items
pp_pattern (AST.Pattern'NamedAlpha _ name _ subpat) = PP.List [pp_aiden name, "@", pp_pattern subpat]
pp_pattern (AST.Pattern'AnonADTVariant _ variant_parent variant fields) = PP.List [pp_path_or_single_iden pp_aiden variant_parent variant, PP.parenthesized_comma_list PP.Inconsistent (map pp_pattern fields)]
pp_pattern (AST.Pattern'NamedADTVariant _ variant_parent variant fields) = PP.List [pp_path_or_single_iden pp_aiden variant_parent variant, PP.braced_block $ map (\ (field_name, field_pat) -> PP.List [pp_aiden field_name, " = ", pp_pattern field_pat, ";"]) fields]

pp_path_or_single_iden :: (iden -> PP.Token) -> Maybe AST.Type -> iden -> PP.Token
pp_path_or_single_iden pp_iden Nothing i = pp_iden i
pp_path_or_single_iden pp_iden (Just ty) i = PP.List [pp_type ty, "::", pp_iden i]

pp_operator :: AST.Operator -> PP.Token
pp_operator (AST.Operator'Path _ t i) = PP.List [pp_type t, "::", pp_siden i]
pp_operator (AST.Operator'Single i) = pp_siden i

pp_aiden :: Located Token.AlphaIdentifier -> PP.Token
pp_aiden (Located _ (Token.AlphaIdentifier i)) = PP.String i

pp_siden :: Located Token.SymbolIdentifier -> PP.Token
pp_siden (Located _ (Token.SymbolIdentifier i)) = PP.String i
