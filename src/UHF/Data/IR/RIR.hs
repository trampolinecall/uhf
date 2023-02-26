module UHF.Data.IR.RIR
    ( Decl (..)
    , Binding (..)
    , Type
    , Expr (..)
    , Pattern (..)
    , expr_type
    , pattern_type
    , expr_span
    , pattern_span
    ) where

import UHF.Util.Prelude

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

data Decl
    = Decl'Module [Binding]
    | Decl'Type Type
    deriving Show

data Binding = Binding BoundValueKey Expr deriving Show

type Type = Type.Type Void

data Expr
    = Expr'Identifier (Maybe Type) Span (Maybe BoundValueKey)
    | Expr'Char (Maybe Type) Span Char
    | Expr'String (Maybe Type) Span Text
    | Expr'Int (Maybe Type) Span Integer
    | Expr'Float (Maybe Type) Span Rational
    | Expr'Bool (Maybe Type) Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple (Maybe Type) Span Expr Expr

    | Expr'Lambda (Maybe Type) Span BoundValueKey Expr

    | Expr'Let (Maybe Type) Span [Binding] Expr

    | Expr'Call (Maybe Type) Span Expr Expr

    | Expr'If (Maybe Type) Span Expr Expr Expr -- TODO: desugar to 'case x { true -> _; false -> _ }
    | Expr'Case (Maybe Type) Span Expr [(Pattern, Expr)] -- TODO: case should be more like switch at this level

    | Expr'Poison (Maybe Type) Span
    deriving Show

data Pattern
    = Pattern'Identifier (Maybe Type) Span BoundValueKey
    | Pattern'Wildcard (Maybe Type) Span
    | Pattern'Tuple (Maybe Type) Span Pattern Pattern
    | Pattern'Named (Maybe Type) Span (Located BoundValueKey) Pattern

    | Pattern'Poison (Maybe Type) Span
    deriving Show

expr_type :: Expr -> (Maybe Type)
expr_type (Expr'Identifier ty _ _) = ty
expr_type (Expr'Char ty _ _) = ty
expr_type (Expr'String ty _ _) = ty
expr_type (Expr'Int ty _ _) = ty
expr_type (Expr'Float ty _ _) = ty
expr_type (Expr'Bool ty _ _) = ty
expr_type (Expr'Tuple ty _ _ _) = ty
expr_type (Expr'Lambda ty _ _ _) = ty
expr_type (Expr'Let ty _ _ _) = ty
expr_type (Expr'Call ty _ _ _) = ty
expr_type (Expr'If ty _ _ _ _) = ty
expr_type (Expr'Case ty _ _ _) = ty
expr_type (Expr'Poison ty _) = ty

expr_span :: Expr -> Span
expr_span (Expr'Identifier _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _) = sp
expr_span (Expr'Let _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'If _ sp _ _ _) = sp
expr_span (Expr'Case _ sp _ _) = sp
expr_span (Expr'Poison _ sp) = sp

pattern_type :: Pattern -> (Maybe Type)
pattern_type (Pattern'Identifier ty _ _) = ty
pattern_type (Pattern'Wildcard ty _) = ty
pattern_type (Pattern'Tuple ty _ _ _) = ty
pattern_type (Pattern'Named ty _ _ _) = ty
pattern_type (Pattern'Poison ty _) = ty

pattern_span :: Pattern -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
