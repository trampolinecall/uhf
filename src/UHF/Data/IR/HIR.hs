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

data HIR iden type_expr type_info binary_ops_allowed = HIR (Arena.Arena (Decl iden type_expr type_info binary_ops_allowed) DeclKey) (Arena.Arena (Type.ADT type_expr) ADTKey) (Arena.Arena (Type.TypeSynonym type_expr) TypeSynonymKey) (Arena.Arena (BoundValue type_info) BoundValueKey)

data Decl identifier typeannotation typeinfo binaryopsallowed
    = Decl'Module NameContext [Binding identifier typeannotation typeinfo binaryopsallowed] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data BoundValue typeinfo = BoundValue typeinfo Span deriving Show

data Binding identifier typeannotation typeinfo binaryopsallowed = Binding (Pattern identifier typeinfo) Span (Expr identifier typeannotation typeinfo binaryopsallowed) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundValueKey) (Maybe NameContext) deriving Show

data TypeExpr identifier
    = TypeExpr'Identifier Span identifier
    | TypeExpr'Tuple (TypeExpr identifier) (TypeExpr identifier)
    | TypeExpr'Poison Span
    deriving Show

data Expr identifier typeannotation typeinfo binaryopsallowed
    = Expr'Identifier typeinfo Span identifier
    | Expr'Char typeinfo Span Char
    | Expr'String typeinfo Span Text
    | Expr'Int typeinfo Span Integer
    | Expr'Float typeinfo Span Rational
    | Expr'Bool typeinfo Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Lambda typeinfo Span (Pattern identifier typeinfo) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Let typeinfo Span [Binding identifier typeannotation typeinfo binaryopsallowed] (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'BinaryOps binaryopsallowed typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) [(identifier, Expr identifier typeannotation typeinfo binaryopsallowed)]

    | Expr'Call typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'If typeinfo Span Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)
    | Expr'Case typeinfo Span Span (Expr identifier typeannotation typeinfo binaryopsallowed) [(Pattern identifier typeinfo, Expr identifier typeannotation typeinfo binaryopsallowed)]

    | Expr'TypeAnnotation typeinfo Span typeannotation (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Poison typeinfo Span
    deriving Show

data Pattern identifier typeinfo
    = Pattern'Identifier typeinfo Span BoundValueKey
    | Pattern'Wildcard typeinfo Span
    | Pattern'Tuple typeinfo Span (Pattern identifier typeinfo) (Pattern identifier typeinfo)
    | Pattern'Named typeinfo Span Span (Located BoundValueKey) (Pattern identifier typeinfo)

    | Pattern'Poison typeinfo Span
    deriving Show

expr_type :: Expr identifier typeannotation typeinfo binaryopsallowed -> typeinfo
expr_type (Expr'Identifier typeinfo _ _) = typeinfo
expr_type (Expr'Char typeinfo _ _) = typeinfo
expr_type (Expr'String typeinfo _ _) = typeinfo
expr_type (Expr'Int typeinfo _ _) = typeinfo
expr_type (Expr'Float typeinfo _ _) = typeinfo
expr_type (Expr'Bool typeinfo _ _) = typeinfo
expr_type (Expr'Tuple typeinfo _ _ _) = typeinfo
expr_type (Expr'Lambda typeinfo _ _ _) = typeinfo
expr_type (Expr'Let typeinfo _ _ _) = typeinfo
expr_type (Expr'BinaryOps _ typeinfo _ _ _) = typeinfo
expr_type (Expr'Call typeinfo _ _ _) = typeinfo
expr_type (Expr'If typeinfo _ _ _ _ _) = typeinfo
expr_type (Expr'Case typeinfo _ _ _ _) = typeinfo
expr_type (Expr'Poison typeinfo _) = typeinfo
expr_type (Expr'TypeAnnotation typeinfo _ _ _) = typeinfo

expr_span :: Expr identifier typeannotation typeinfo binaryopsallowed -> Span
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

pattern_type :: Pattern typeannotation typeinfo -> typeinfo
pattern_type (Pattern'Identifier typeinfo _ _) = typeinfo
pattern_type (Pattern'Wildcard typeinfo _) = typeinfo
pattern_type (Pattern'Tuple typeinfo _ _ _) = typeinfo
pattern_type (Pattern'Named typeinfo _ _ _ _) = typeinfo
pattern_type (Pattern'Poison typeinfo _) = typeinfo

pattern_span :: Pattern typeannotation typeinfo -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
