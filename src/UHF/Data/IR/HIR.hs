module UHF.Data.IR.HIR
    ( HIR (..)

    , DeclKey
    , Decl(..)

    , BoundValueKey
    , BoundValue(..)

    , Binding (..)

    , NameContext (..)

    , TypeExpr(..)
    , Expr(..)
    , Pattern(..)
    , expr_type
    , pattern_type
    , expr_span
    , pattern_span
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type

import qualified Data.Map as Map

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

data HIR iden type_expr type_info binary_ops_allowed = HIR (Arena.Arena (Decl iden type_expr type_info binary_ops_allowed) DeclKey) (Arena.Arena (Type.ADT type_expr) ADTKey) (Arena.Arena (Type.TypeSynonym type_expr) TypeSynonymKey) (Arena.Arena (BoundValue type_info) BoundValueKey) DeclKey

data Decl identifier type_expr type_info binary_ops_allowed
    = Decl'Module NameContext [Binding identifier type_expr type_info binary_ops_allowed] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data BoundValue type_info = BoundValue type_info Span deriving Show

data Binding identifier type_expr type_info binary_ops_allowed = Binding (Pattern identifier type_info) Span (Expr identifier type_expr type_info binary_ops_allowed) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundValueKey) (Maybe NameContext) deriving Show

data TypeExpr identifier
    = TypeExpr'Identifier Span identifier
    | TypeExpr'Tuple (TypeExpr identifier) (TypeExpr identifier)
    | TypeExpr'Poison Span
    deriving Show

data Expr identifier type_expr type_info binary_ops_allowed
    = Expr'Identifier type_info Span identifier
    | Expr'Char type_info Span Char
    | Expr'String type_info Span Text
    | Expr'Int type_info Span Integer
    | Expr'Float type_info Span Rational
    | Expr'Bool type_info Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple type_info Span (Expr identifier type_expr type_info binary_ops_allowed) (Expr identifier type_expr type_info binary_ops_allowed)

    | Expr'Lambda type_info Span (Pattern identifier type_info) (Expr identifier type_expr type_info binary_ops_allowed)

    | Expr'Let type_info Span [Binding identifier type_expr type_info binary_ops_allowed] (Expr identifier type_expr type_info binary_ops_allowed)

    | Expr'BinaryOps binary_ops_allowed type_info Span (Expr identifier type_expr type_info binary_ops_allowed) [(identifier, Expr identifier type_expr type_info binary_ops_allowed)]

    | Expr'Call type_info Span (Expr identifier type_expr type_info binary_ops_allowed) (Expr identifier type_expr type_info binary_ops_allowed)

    | Expr'If type_info Span Span (Expr identifier type_expr type_info binary_ops_allowed) (Expr identifier type_expr type_info binary_ops_allowed) (Expr identifier type_expr type_info binary_ops_allowed)
    | Expr'Case type_info Span Span (Expr identifier type_expr type_info binary_ops_allowed) [(Pattern identifier type_info, Expr identifier type_expr type_info binary_ops_allowed)]

    | Expr'TypeAnnotation type_info Span type_expr (Expr identifier type_expr type_info binary_ops_allowed)

    | Expr'Poison type_info Span
    deriving Show

data Pattern identifier type_info
    = Pattern'Identifier type_info Span BoundValueKey
    | Pattern'Wildcard type_info Span
    | Pattern'Tuple type_info Span (Pattern identifier type_info) (Pattern identifier type_info)
    | Pattern'Named type_info Span Span (Located BoundValueKey) (Pattern identifier type_info)

    | Pattern'Poison type_info Span
    deriving Show

expr_type :: Expr identifier type_expr type_info binary_ops_allowed -> type_info
expr_type (Expr'Identifier type_info _ _) = type_info
expr_type (Expr'Char type_info _ _) = type_info
expr_type (Expr'String type_info _ _) = type_info
expr_type (Expr'Int type_info _ _) = type_info
expr_type (Expr'Float type_info _ _) = type_info
expr_type (Expr'Bool type_info _ _) = type_info
expr_type (Expr'Tuple type_info _ _ _) = type_info
expr_type (Expr'Lambda type_info _ _ _) = type_info
expr_type (Expr'Let type_info _ _ _) = type_info
expr_type (Expr'BinaryOps _ type_info _ _ _) = type_info
expr_type (Expr'Call type_info _ _ _) = type_info
expr_type (Expr'If type_info _ _ _ _ _) = type_info
expr_type (Expr'Case type_info _ _ _ _) = type_info
expr_type (Expr'Poison type_info _) = type_info
expr_type (Expr'TypeAnnotation type_info _ _ _) = type_info

expr_span :: Expr identifier type_expr type_info binary_ops_allowed -> Span
expr_span (Expr'Identifier _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _) = sp
expr_span (Expr'Let _ sp _ _) = sp
expr_span (Expr'BinaryOps _ _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'If _ sp _ _ _ _) = sp
expr_span (Expr'Case _ sp _ _ _) = sp
expr_span (Expr'Poison _ sp) = sp
expr_span (Expr'TypeAnnotation _ sp _ _) = sp

pattern_type :: Pattern type_expr type_info -> type_info
pattern_type (Pattern'Identifier type_info _ _) = type_info
pattern_type (Pattern'Wildcard type_info _) = type_info
pattern_type (Pattern'Tuple type_info _ _ _) = type_info
pattern_type (Pattern'Named type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison type_info _) = type_info

pattern_span :: Pattern type_expr type_info -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
