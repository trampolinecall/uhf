module UHF.Data.IR.SIR
    ( SIR (..)

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
    , type_expr_type_info
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

import qualified Data.Map as Map

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

-- "syntax based ir"
data SIR d_iden v_iden type_info binary_ops_allowed = SIR (Arena.Arena (Decl d_iden v_iden type_info binary_ops_allowed) DeclKey) (Arena.Arena (Type.ADT (TypeExpr d_iden type_info)) ADTKey) (Arena.Arena (Type.TypeSynonym (TypeExpr d_iden type_info)) TypeSynonymKey) (Arena.Arena (Type.Var) TypeVarKey) (Arena.Arena (BoundValue type_info) BoundValueKey) DeclKey

data Decl d_iden v_iden type_info binary_ops_allowed
    = Decl'Module ID.ModuleID NameContext [Binding d_iden v_iden type_info binary_ops_allowed] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data BoundValue type_info = BoundValue ID.BoundValueID type_info Span deriving Show

data Binding d_iden v_iden type_info binary_ops_allowed = Binding (Pattern type_info) Span (Expr d_iden v_iden type_info binary_ops_allowed) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundValueKey) (Maybe NameContext) deriving Show

type HoleIdentifier = Located [Located Text]

data TypeExpr d_iden type_info
    = TypeExpr'Identifier type_info Span d_iden
    | TypeExpr'Tuple type_info (TypeExpr d_iden type_info) (TypeExpr d_iden type_info)
    | TypeExpr'Hole type_info HoleIdentifier
    | TypeExpr'Forall type_info [TypeVarKey] (TypeExpr d_iden type_info)
    | TypeExpr'Apply type_info Span (TypeExpr d_iden type_info) (TypeExpr d_iden type_info)
    | TypeExpr'Wild type_info Span
    | TypeExpr'Poison type_info Span
    deriving Show

data Expr d_iden v_iden type_info binary_ops_allowed
    = Expr'Identifier ID.ExprID type_info Span v_iden
    | Expr'Char ID.ExprID type_info Span Char
    | Expr'String ID.ExprID type_info Span Text
    | Expr'Int ID.ExprID type_info Span Integer
    | Expr'Float ID.ExprID type_info Span Rational
    | Expr'Bool ID.ExprID type_info Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID type_info Span (Expr d_iden v_iden type_info binary_ops_allowed) (Expr d_iden v_iden type_info binary_ops_allowed)

    | Expr'Lambda ID.ExprID type_info Span (Pattern type_info) (Expr d_iden v_iden type_info binary_ops_allowed)

    | Expr'Let ID.ExprID type_info Span [Binding d_iden v_iden type_info binary_ops_allowed] (Expr d_iden v_iden type_info binary_ops_allowed)

    | Expr'BinaryOps ID.ExprID binary_ops_allowed type_info Span (Expr d_iden v_iden type_info binary_ops_allowed) [(v_iden, Expr d_iden v_iden type_info binary_ops_allowed)]

    | Expr'Call ID.ExprID type_info Span (Expr d_iden v_iden type_info binary_ops_allowed) (Expr d_iden v_iden type_info binary_ops_allowed)

    | Expr'If ID.ExprID type_info Span Span (Expr d_iden v_iden type_info binary_ops_allowed) (Expr d_iden v_iden type_info binary_ops_allowed) (Expr d_iden v_iden type_info binary_ops_allowed)
    | Expr'Case ID.ExprID type_info Span Span (Expr d_iden v_iden type_info binary_ops_allowed) [(Pattern type_info, Expr d_iden v_iden type_info binary_ops_allowed)]

    | Expr'Forall ID.ExprID type_info Span [TypeVarKey] (Expr d_iden v_iden type_info binary_ops_allowed) -- TODO: add variables
    | Expr'TypeApply ID.ExprID type_info Span (Expr d_iden v_iden type_info binary_ops_allowed) (TypeExpr d_iden type_info)

    | Expr'TypeAnnotation ID.ExprID type_info Span (TypeExpr d_iden type_info) (Expr d_iden v_iden type_info binary_ops_allowed)

    | Expr'Hole ID.ExprID type_info Span HoleIdentifier

    | Expr'Poison ID.ExprID type_info Span
    deriving Show

data Pattern type_info
    = Pattern'Identifier type_info Span BoundValueKey
    | Pattern'Wildcard type_info Span
    | Pattern'Tuple type_info Span (Pattern type_info) (Pattern type_info)
    | Pattern'Named type_info Span Span (Located BoundValueKey) (Pattern type_info)

    | Pattern'Poison type_info Span
    deriving Show

type_expr_type_info :: TypeExpr d_iden type_info -> type_info
type_expr_type_info (TypeExpr'Identifier type_info _ _) = type_info
type_expr_type_info (TypeExpr'Tuple type_info _ _) = type_info
type_expr_type_info (TypeExpr'Hole type_info _) = type_info
type_expr_type_info (TypeExpr'Forall type_info _ _) = type_info
type_expr_type_info (TypeExpr'Apply type_info _ _ _) = type_info
type_expr_type_info (TypeExpr'Wild type_info _) = type_info
type_expr_type_info (TypeExpr'Poison type_info _) = type_info

expr_type :: Expr d_iden v_iden type_info binary_ops_allowed -> type_info
expr_type (Expr'Identifier _ type_info _ _) = type_info
expr_type (Expr'Char _ type_info _ _) = type_info
expr_type (Expr'String _ type_info _ _) = type_info
expr_type (Expr'Int _ type_info _ _) = type_info
expr_type (Expr'Float _ type_info _ _) = type_info
expr_type (Expr'Bool _ type_info _ _) = type_info
expr_type (Expr'Tuple _ type_info _ _ _) = type_info
expr_type (Expr'Lambda _ type_info _ _ _) = type_info
expr_type (Expr'Let _ type_info _ _ _) = type_info
expr_type (Expr'BinaryOps _ _ type_info _ _ _) = type_info
expr_type (Expr'Call _ type_info _ _ _) = type_info
expr_type (Expr'If _ type_info _ _ _ _ _) = type_info
expr_type (Expr'Case _ type_info _ _ _ _) = type_info
expr_type (Expr'Poison _ type_info _) = type_info
expr_type (Expr'Hole _ type_info _ _) = type_info
expr_type (Expr'Forall _ type_info _ _ _) = type_info
expr_type (Expr'TypeApply _ type_info _ _ _) = type_info
expr_type (Expr'TypeAnnotation _ type_info _ _ _) = type_info

expr_span :: Expr d_iden v_iden type_info binary_ops_allowed -> Span
expr_span (Expr'Identifier _ _ sp _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _) = sp
expr_span (Expr'Let _ _ sp _ _) = sp
expr_span (Expr'BinaryOps _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'If _ _ sp _ _ _ _) = sp
expr_span (Expr'Case _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
expr_span (Expr'Hole _ _ sp _) = sp
expr_span (Expr'Forall _ _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ sp _ _) = sp

pattern_type :: Pattern type_info -> type_info
pattern_type (Pattern'Identifier type_info _ _) = type_info
pattern_type (Pattern'Wildcard type_info _) = type_info
pattern_type (Pattern'Tuple type_info _ _ _) = type_info
pattern_type (Pattern'Named type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison type_info _) = type_info

pattern_span :: Pattern type_info -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
