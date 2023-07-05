module UHF.Phases.Back.TSBackend.TS.PP (stmts) where

import UHF.Util.Prelude

import qualified UHF.PP as PP

import qualified UHF.Phases.Back.TSBackend.TS as TS

stmts :: [TS.Stmt] -> Text
stmts = PP.render . stmts'

stmts' :: [TS.Stmt] -> PP.Token
stmts' s = PP.flat_block $ map stmt s

stmt :: TS.Stmt -> PP.Token
stmt (TS.Stmt'Function name p retty body) = PP.List ["function ", PP.String name, params p, type_annotation retty, body']
    where
        body' = case body of
            Nothing -> ";"
            Just stmts -> PP.List [" ", PP.braced_block $ map stmt stmts]

stmt (TS.Stmt'Class name implements members) = PP.List ["class ", PP.String name, implements', " ", PP.braced_block $ map class_member members]
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
stmt (TS.Stmt'Block stmts) = PP.braced_block $ map stmt stmts
stmt (TS.Stmt'If test true false) =
    case false of
        Just false -> PP.List [true', " else ", stmt false]
        Nothing -> true'
    where
        true' = PP.List ["if (", expr test, ") ", stmt true]

stmt TS.Stmt'Spacer = PP.List []

expr :: TS.Expr -> PP.Token
expr = level1
    where
        -- TODO: fix infinite recursion when adding new thing and forgetting to add to here
        -- "one simple trick" for printing precedence taken from https://www.haskellforall.com/2020/11/pretty-print-syntax-trees-with-this-one.html
        -- precedence info is taken from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table, also is not complete

        -- level 1: comma / sequence operator (not in this ast)
        level1 = level2

        -- level 2: '=>' rtl, '?:' rtl, assignment rtl
        level2 (TS.Expr'ArrowFunction p ret body) = PP.List [params p, type_annotation ret, " => ", body']
            where
                body' = case body of
                    Left e -> level2 e
                    Right stmts -> PP.braced_block $ map stmt stmts
        level2 (TS.Expr'Assign lhs rhs) = PP.List [level3 lhs, " = ", level2 rhs]
        level2 e = level3 e

        -- level 3: '||' ltr, '??' ltr
        level3 = level4

        -- level 4: '&&' ltr
        level4 = level5

        -- level 5: '|' ltr
        level5 = level6

        -- level 6: '^' ltr
        level6 = level7

        -- level 7: '&' ltr
        level7 = level8

        -- level 8: equality ltr
        level8 = level9

        -- level 9: comparison ltr, 'in' ltr, 'instanceof' ltr
        level9 = level10

        -- level 10: bitwise shift ltr
        level10 = level11

        -- level 11: addition and subtraction ltr
        level11 = level12

        -- level 12: multiplication, division, modulo ltr
        level12 (TS.Expr'Div a b) = PP.List [level12 a, " / ", level13 b]
        level12 e = level13 e

        -- level 13: exponentiation ('**') rtl
        level13 = level14

        -- level 14: '!x', '~x', '+x', '-x', '++x', '--x', 'typeof x', 'void x', 'delete x', 'await x'
        level14 = level15

        -- level 15: 'x++', 'y++'
        level15 = level16

        -- level 16: 'new x'
        level16 (TS.Expr'New constructor []) = PP.List ["new ", level17 constructor] -- not sure what this is supposed to be
        level16 e = level17 e

        -- level 17: 'x[y]', 'new x()', function call, also 'x.y' and 'x?.y' ltr
        level17 (TS.Expr'Get e field) = PP.List [level17 e, ".", PP.String field]
        level17 (TS.Expr'Call e args) = PP.List [level17 e, PP.parenthesized_comma_list PP.Inconsistent (map expr args)]
        level17 (TS.Expr'New constructor args) = PP.List ["new ", level18 constructor, PP.parenthesized_comma_list PP.Inconsistent (map expr args)]
        level17 e = level18 e

        -- level 18: parentheses
        level18 (TS.Expr'Identifier n) = PP.String n
        level18 (TS.Expr'Int i) = PP.String $ show i
        level18 (TS.Expr'Bool b) = if b then "true" else "false"
        level18 (TS.Expr'Char c) = PP.String $ show c
        level18 (TS.Expr'String s) = PP.String $ show s
        level18 TS.Expr'Undefined = "undefined"
        level18 (TS.Expr'StrLit s) = PP.List ["\"", PP.String s, "\""]
        level18 (TS.Expr'List es) = PP.bracketed_comma_list PP.Inconsistent (map expr es)
        level18 (TS.Expr'Object items) =
            PP.braced_comma_list
                PP.Inconsistent
                (map
                    (\ (n, init) ->
                        let init' = case init of
                                Nothing -> ""
                                Just e -> PP.List [": ", expr e]
                        in PP.List [PP.String n, init'])
                    items)
        level18 e = PP.List ["(", level1 e, ")"]

type_ :: TS.Type -> PP.Token
type_ = level1
    where
        -- precedence taken from https://github.com/antlr/grammars-v4/blob/master/javascript/typescript/TypeScriptParser.g4#L80
        -- same "one simple trick" used

        level1 (TS.Type'StrLit s) = PP.List ["\"", PP.String s, "\""]
        level1 t = level2 t

        level2 (TS.Type'Union a b) = PP.List [type_ a, " | ", type_ b]
        level2 t = level3 t

        level3 (TS.Type'Reference ref) = ty_ref ref
        level3 (TS.Type'Object fields) = PP.braced_comma_list PP.Inconsistent (map field fields)
            where field (name, ty) = PP.List [PP.String name, type_annotation ty]
        level3 TS.Type'Never = "never"
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
            Just stmts -> PP.List [" ", PP.braced_block $ map stmt stmts]
class_member (TS.ClassMember'PropDecl name ty init) = PP.List [PP.String name, type_annotation ty, init', ";"]
    where
        init' = case init of
            Nothing -> PP.List []
            Just e -> PP.List [" = ", expr e]
class_member (TS.ClassMember'MethodDecl name p retty body) = PP.List [PP.String name, params p, type_annotation retty, body']
    where
        body' = case body of
            Nothing -> ";"
            Just stmts -> PP.List [" ", PP.braced_block $ map stmt stmts]
