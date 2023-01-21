module UHF.Parser.Expr(expr) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as Parser
import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

expr :: Parser.Parser AST.Expr
expr =
    Parser.choice
        [ identifier_expr
        , char_lit_expr
        , string_lit_expr
        , int_lit_expr
        , float_lit_expr
        , bool_lit_expr
        ]

identifier_expr :: Parser.Parser AST.Expr
identifier_expr =
    Parser.consume "identifier" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Expr'Identifier (Location.Located iden_sp iden))

char_lit_expr :: Parser.Parser AST.Expr
char_lit_expr =
    Parser.consume "character literal" (Token.CharLit ()) >>= \ (Location.Located _ (Token.CharLit ch)) ->
    pure (AST.Expr'CharLit ch)

string_lit_expr :: Parser.Parser AST.Expr
string_lit_expr =
    Parser.consume "string literal" (Token.StringLit ()) >>= \ (Location.Located _ (Token.StringLit s)) ->
    pure (AST.Expr'StringLit s)

int_lit_expr :: Parser.Parser AST.Expr
int_lit_expr =
    Parser.consume "integer literal" (Token.IntLit () ()) >>= \ (Location.Located _ (Token.IntLit _ i)) ->
    pure (AST.Expr'IntLit i)

float_lit_expr :: Parser.Parser AST.Expr
float_lit_expr =
    Parser.consume "float literal" (Token.FloatLit ()) >>= \ (Location.Located _ (Token.FloatLit f)) ->
    pure (AST.Expr'FloatLit f)

bool_lit_expr :: Parser.Parser AST.Expr
bool_lit_expr =
    Parser.consume "bool literal" (Token.BoolLit ()) >>= \ (Location.Located _ (Token.BoolLit b)) ->
    pure (AST.Expr'BoolLit b)
