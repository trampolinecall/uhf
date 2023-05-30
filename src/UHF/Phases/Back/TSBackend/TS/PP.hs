module UHF.Phases.Back.TSBackend.TS.PP (stmts) where

import UHF.Util.Prelude

import qualified UHF.PP as PP

import qualified UHF.Phases.Back.TSBackend.TS as TS

-- TODO: format more nicely

stmts :: [TS.Stmt] -> Text
stmts = PP.render . stmts'

stmts' :: [TS.Stmt] -> PP.Token
stmts' s = PP.flat_block $ map stmt s

stmt :: TS.Stmt -> PP.Token
stmt (TS.Stmt'Function name p retty body) = PP.List ["function ", PP.String name, params p, type_annotation retty, body']
    where
        body' = case body of
            Nothing -> ";"
            Just stmts -> PP.braced_block $ map stmt stmts

stmt (TS.Stmt'Class name implements members) = PP.List ["class ", PP.String name, implements', PP.braced_block $ map class_member members]
    where
        implements' = case implements of
            [] -> PP.List []
            tys -> PP.List [" implements ", PP.comma_separated PP.Inconsistent (map ty_ref tys)]
stmt (TS.Stmt'Let name ty init) = PP.List ["let ", PP.String name, type_annotation ty, init', ";"]
    where
        init' = case init of
            Nothing -> ""
            Just e -> PP.List [" = ", expr e]

stmt (TS.Stmt'Return e) = PP.List ["return ", expr e, ";"]
stmt (TS.Stmt'Expr e) = PP.List [expr e, ";"]

expr :: TS.Expr -> PP.Token
expr e = PP.List ["(", expr' e, ")"] -- TODO: also do precedence correctly
    where
        expr' (TS.Expr'Identifier n) = PP.String n
        expr' (TS.Expr'Int i) = PP.String $ show i
        expr' (TS.Expr'Bool b) = if b then "true" else "false"
        expr' (TS.Expr'Char c) = PP.String $ show c
        expr' (TS.Expr'String s) = PP.String $ show s
        expr' (TS.Expr'Undefined) = "undefined"
        expr' (TS.Expr'StrLit s) = PP.List ["\"", PP.String s, "\""]
        expr' (TS.Expr'List es) = PP.bracketed_comma_list PP.Inconsistent (map expr es)
        expr' (TS.Expr'Object items) =
            PP.braced_comma_list
                PP.Inconsistent
                (map
                    (\ (n, init) ->
                        let init' = case init of
                                Nothing -> ""
                                Just e -> PP.List [": ", expr e]
                        in PP.List [PP.String n, init'])
                    items)
        expr' (TS.Expr'ArrowFunction p ret body) = PP.List [params p, type_annotation ret, " => ", body']
            where
                body' = case body of
                    Left e -> expr e
                    Right stmts -> PP.braced_block $ map stmt stmts
        expr' (TS.Expr'New constructor args) = PP.List ["new ", expr constructor, PP.parenthesized_comma_list PP.Inconsistent (map expr args)]
        expr' (TS.Expr'Call e args) = PP.List [expr e, PP.parenthesized_comma_list PP.Inconsistent (map expr args)]
        expr' (TS.Expr'Get e field) = PP.List [expr e, ".", PP.String field]
        expr' (TS.Expr'Div a b) = PP.List [expr a, " / ", expr b]
        expr' (TS.Expr'Assign lhs rhs) = PP.List [expr lhs, " = ", expr rhs]

type_ :: TS.Type -> PP.Token
type_ t = level1 t
    where
        -- precedence taken from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptParser.g4#L80
        -- "one simple trick" for printing precedence taken from https://www.haskellforall.com/2020/11/pretty-print-syntax-trees-with-this-one.html

        level1 (TS.Type'StrLit s) = PP.List ["\"", PP.String s, "\""]
        level1 t = level2 t

        level2 (TS.Type'Union a b) = PP.List [type_ a, " | ", type_ b]
        level2 t = level3 t

        level3 (TS.Type'Reference ref) = ty_ref ref
        level3 (TS.Type'Object fields) = PP.braced_comma_list PP.Inconsistent (map field fields) where field (name, ty) = PP.List [PP.String name, type_annotation ty]
        level3 (TS.Type'Never) = "never"
        level3 t = PP.List ["(", level1 t, ")"]

type_annotation :: Maybe TS.Type -> PP.Token
type_annotation (Just ty) = PP.List [": ", type_ ty]
type_annotation Nothing = PP.List []

ty_ref :: TS.TypeReference -> PP.Token
ty_ref (TS.TypeReference name []) = PP.String name
ty_ref (TS.TypeReference name args) = PP.List [PP.String name, "<", PP.comma_separated PP.Inconsistent (map type_ args), ">"]

params :: [TS.Parameter] -> PP.Token
params = PP.parenthesized_comma_list PP.Inconsistent . map param
    where
        param (TS.Parameter access name ty) = PP.List [access', PP.String name, type_annotation ty]
            where
                access' = case access of
                    Just TS.Public -> "public "
                    Nothing -> ""

class_member :: TS.ClassMember -> PP.Token
class_member (TS.ClassMember'Constructor p body) = PP.List ["constructor", params p, body']
    where
        body' = case body of
            Nothing -> ";"
            Just stmts -> PP.braced_block $ map stmt stmts
class_member (TS.ClassMember'PropDecl name ty init) = PP.List [PP.String name, type_annotation ty, init', ";"]
    where
        init' = case init of
            Nothing -> PP.List []
            Just e -> PP.List [" = ", expr e]
class_member (TS.ClassMember'MethodDecl name p retty body) = PP.List [PP.String name, params p, type_annotation retty, body']
    where
        body' = case body of
            Nothing -> ";"
            Just stmts -> PP.braced_block $ map stmt stmts
