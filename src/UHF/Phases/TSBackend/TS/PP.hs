module UHF.Phases.TSBackend.TS.PP (stmts) where

import UHF.Prelude

import qualified UHF.PP as PP
import qualified UHF.PP.Precedence as PP.Precedence
import qualified UHF.Phases.TSBackend.TS as TS

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
stmt (TS.Stmt'Label name body) = PP.List [PP.String name, ": ", stmt body]
stmt (TS.Stmt'Break Nothing) = "break;"
stmt (TS.Stmt'Break (Just label)) = PP.List ["break ", PP.String label, ";"]

stmt TS.Stmt'Spacer = PP.List []

expr :: TS.Expr -> PP.Token
expr = PP.Precedence.pp_precedence levels PP.Precedence.parenthesize
    where
        -- precedence info is taken from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table
        -- also, this ast does not cover every possible type of expression

        -- level 1: comma / sequence operator (not in this ast)
        -- level 2: '=>' rtl, '?:' rtl, assignment rtl
        levels (TS.Expr'ArrowFunction p ret body) =
            ( 2
            , \ cur _ ->
                let body' = case body of
                        Left e -> cur e
                        Right stmts -> PP.braced_block $ map stmt stmts
                in PP.List [params p, type_annotation ret, " => ", body']
            )
        levels (TS.Expr'Assign lhs rhs) = (2, \ cur next -> PP.List [next lhs, " = ", cur rhs])
        -- level 3: '||' ltr, '??' ltr
        -- level 4: '&&' ltr
        -- level 5: '|' ltr
        -- level 6: '^' ltr
        -- level 7: '&' ltr
        -- level 8: equality ltr
        levels (TS.Expr'Eq a b) = (8, \ cur next -> PP.List [cur a, " == ", next b])
        -- level 9: comparison ltr, 'in' ltr, 'instanceof' ltr
        -- level 10: bitwise shift ltr
        -- level 11: addition and subtraction ltr
        -- level 12: multiplication, division, modulo ltr
        levels (TS.Expr'Div a b) = (12, \ cur next -> PP.List [cur a, " / ", next b])
        -- level 13: exponentiation ('**') rtl
        -- level 14: '!x', '~x', '+x', '-x', '++x', '--x', 'typeof x', 'void x', 'delete x', 'await x'
        -- level 15: 'x++', 'y++'
        -- level 16: 'new x'
        levels (TS.Expr'New constructor []) = (16, \ _ next -> PP.List ["new ", next constructor]) -- not entirely sure if this is correct
        -- level 17: 'x[y]', 'new x()', function call, also 'x.y' and 'x?.y' ltr
        levels (TS.Expr'Get e field) = (17, \ cur _ -> PP.List [cur e, ".", PP.String field])
        levels (TS.Expr'Call e args) = (17, \ cur _ -> PP.List [cur e, PP.parenthesized_comma_list PP.Inconsistent (map expr args)])
        levels (TS.Expr'New constructor args) = (17, \ _ next -> PP.List ["new ", next constructor, PP.parenthesized_comma_list PP.Inconsistent (map expr args)])
        -- level 18: parentheses
        levels (TS.Expr'Identifier n) = (18, \ _ _ -> PP.String n)
        levels (TS.Expr'Int i) = (18, \ _ _ -> PP.String $ show i)
        levels (TS.Expr'Bool b) = (18, \ _ _ -> if b then "true" else "false")
        levels (TS.Expr'Char c) = (18, \ _ _ -> PP.String $ show c)
        levels (TS.Expr'String s) = (18, \ _ _ -> PP.String $ show s)
        levels TS.Expr'Undefined = (18, \ _ _ -> "undefined")
        levels (TS.Expr'StrLit s) = (18, \ _ _ -> PP.List ["\"", PP.String s, "\""])
        levels (TS.Expr'List es) = (18, \ _ _ -> PP.bracketed_comma_list PP.Inconsistent (map expr es))
        levels (TS.Expr'Object items) =
            (18, \ _ _ -> PP.braced_comma_list
                PP.Inconsistent
                (map
                    (\ (n, init) ->
                        let init' = case init of
                                Nothing -> ""
                                Just e -> PP.List [": ", expr e]
                        in PP.List [PP.String n, init'])
                    items))

type_ :: TS.Type -> PP.Token
type_ = PP.Precedence.pp_precedence levels PP.Precedence.parenthesize
    where
        -- TODO: putting strlit on level 1 would result in ("a") | ("b"), right? that would be unnecessary; change strlit to level 3?
        levels (TS.Type'StrLit s) = (1, \ _ _ -> PP.List ["\"", PP.String s, "\""])
        levels (TS.Type'Union a b) = (2, \ cur next -> PP.List [cur a, " | ", next b])
        levels (TS.Type'Reference ref) = (3, \ _ _ -> ty_ref ref)
        levels (TS.Type'Object fields) = (3, \ _ _ -> PP.braced_comma_list PP.Inconsistent (map (\ (name, ty) -> PP.List [PP.String name, type_annotation ty]) fields))
        levels TS.Type'Never = (3, \ _ _ -> "never")

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
