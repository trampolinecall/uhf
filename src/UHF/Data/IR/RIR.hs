module UHF.Data.IR.RIR
    ( RIR (..)

    , DeclKey
    , Decl (..)

    , Binding (..)

    , BoundValueKey
    , BoundValue (..)
    , BoundWhere (..)

    , Type
    , Expr (..)
    , SwitchMatcher (..)
    , expr_type
    , expr_span
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type

import UHF.IO.Span (Span)

data RIR captures = RIR (Arena.Arena (Decl captures) DeclKey) (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) ADTKey) (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) TypeSynonymKey) (Arena.Arena (BoundValue (Maybe (Type.Type Void))) BoundValueKey) DeclKey

data BoundWhere = InModule | InLambdaBody Unique.Unique deriving Show
data BoundValue typeinfo = BoundValue typeinfo BoundWhere Span deriving Show

data Decl captures
    = Decl'Module [Binding captures] [ADTKey] [TypeSynonymKey]
    | Decl'Type Type
    deriving Show

data Binding captures = Binding BoundValueKey (Expr captures) deriving Show

type Type = Type.Type Void

data Expr captures
    = Expr'Identifier (Maybe Type) Span (Maybe BoundValueKey)
    | Expr'Char (Maybe Type) Span Char
    | Expr'String (Maybe Type) Span Text
    | Expr'Int (Maybe Type) Span Integer
    | Expr'Float (Maybe Type) Span Rational
    | Expr'Bool (Maybe Type) Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple (Maybe Type) Span (Expr captures) (Expr captures)

    | Expr'Lambda (Maybe Type) Span Unique.Unique captures BoundValueKey (Expr captures)

    | Expr'Let (Maybe Type) Span [Binding captures] (Expr captures)

    | Expr'Call (Maybe Type) Span (Expr captures) (Expr captures)

    | Expr'Switch (Maybe Type) Span (Expr captures) [(SwitchMatcher, (Expr captures))]

    | Expr'Poison (Maybe Type) Span
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple (Maybe BoundValueKey) (Maybe BoundValueKey)
    | Switch'Default
    deriving Show

expr_type :: (Expr captures) -> Maybe Type
expr_type (Expr'Identifier ty _ _) = ty
expr_type (Expr'Char ty _ _) = ty
expr_type (Expr'String ty _ _) = ty
expr_type (Expr'Int ty _ _) = ty
expr_type (Expr'Float ty _ _) = ty
expr_type (Expr'Bool ty _ _) = ty
expr_type (Expr'Tuple ty _ _ _) = ty
expr_type (Expr'Lambda ty _ _ _ _ _) = ty
expr_type (Expr'Let ty _ _ _) = ty
expr_type (Expr'Call ty _ _ _) = ty
expr_type (Expr'Switch ty _ _ _) = ty
expr_type (Expr'Poison ty _) = ty

expr_span :: (Expr captures) -> Span
expr_span (Expr'Identifier _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _ _ _) = sp
expr_span (Expr'Let _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'Switch _ sp _ _) = sp
expr_span (Expr'Poison _ sp) = sp
