module UHF.Data.IR.RIR
    ( RIR (..)

    , DeclKey
    , Decl (..)

    , Binding (..)

    , BoundValueKey
    , BoundValue (..)

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
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Span (Span)

-- "reduced ir"
data RIR = RIR (Arena.Arena Decl DeclKey) (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) ADTKey) (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) TypeSynonymKey) (Arena.Arena Type.Var Type.TypeVarKey) (Arena.Arena (BoundValue (Maybe (Type.Type Void))) BoundValueKey) DeclKey

data BoundValue type_info = BoundValue ID.BoundValueID type_info Span deriving Show

data Decl
    = Decl'Module [Binding] [ADTKey] [TypeSynonymKey]
    | Decl'Type Type
    deriving Show

data Binding = Binding BoundValueKey Expr deriving Show

type Type = Type.Type Void

data Expr
    = Expr'Identifier ID.ExprID (Maybe Type) Span (Maybe BoundValueKey)
    | Expr'Char ID.ExprID (Maybe Type) Span Char
    | Expr'String ID.ExprID (Maybe Type) Span Text
    | Expr'Int ID.ExprID (Maybe Type) Span Integer
    | Expr'Float ID.ExprID (Maybe Type) Span Rational
    | Expr'Bool ID.ExprID (Maybe Type) Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID (Maybe Type) Span Expr Expr

    | Expr'Lambda ID.ExprID (Maybe Type) Span Unique.Unique BoundValueKey Expr

    | Expr'Let ID.ExprID (Maybe Type) Span [Binding] Expr

    | Expr'Call ID.ExprID (Maybe Type) Span Expr Expr

    | Expr'Switch ID.ExprID (Maybe Type) Span Expr [(SwitchMatcher, Expr)]

    | Expr'Forall ID.ExprID (Maybe Type) Span (NonEmpty TypeVarKey) Expr
    | Expr'TypeApply ID.ExprID (Maybe Type) Span Expr (Maybe Type)

    | Expr'MakeADT ID.ExprID Type Span Type.ADTVariantIndex [Expr]

    | Expr'Poison ID.ExprID (Maybe Type) Span
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple (Maybe BoundValueKey) (Maybe BoundValueKey)
    | Switch'Default
    deriving Show

expr_type :: Expr -> Maybe Type
expr_type (Expr'Identifier _ ty _ _) = ty
expr_type (Expr'Char _ ty _ _) = ty
expr_type (Expr'String _ ty _ _) = ty
expr_type (Expr'Int _ ty _ _) = ty
expr_type (Expr'Float _ ty _ _) = ty
expr_type (Expr'Bool _ ty _ _) = ty
expr_type (Expr'Tuple _ ty _ _ _) = ty
expr_type (Expr'Lambda _ ty _ _ _ _) = ty
expr_type (Expr'Let _ ty _ _ _) = ty
expr_type (Expr'Call _ ty _ _ _) = ty
expr_type (Expr'Switch _ ty _ _ _) = ty
expr_type (Expr'Forall _ ty _ _ _) = ty
expr_type (Expr'TypeApply _ ty _ _ _) = ty
expr_type (Expr'MakeADT _ ty _ _ _) = Just ty
expr_type (Expr'Poison _ ty _) = ty

expr_span :: Expr -> Span -- TODO: remove?
expr_span (Expr'Identifier _ _ sp _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _ _) = sp
expr_span (Expr'Let _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'Switch _ _ sp _ _) = sp
expr_span (Expr'Forall _ _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'MakeADT _ _ sp _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
