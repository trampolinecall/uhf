{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.IR.SIR
    ( SIR (..)
    , Stage.Stage(..)

    , DeclKey
    , Decl(..)

    , ModuleKey
    , Module(..)

    , BoundValueKey
    , BoundValue(..)

    , Binding (..)

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

import qualified UHF.Data.IR.SIR.Stage as Stage

import UHF.IO.Span (Span)
import UHF.IO.Located (Located)

-- "syntax based ir"
data SIR stage
    = SIR
        (Arena.Arena Decl DeclKey)
        (Arena.Arena (Module stage) ModuleKey)
        (Arena.Arena (Type.ADT (TypeExpr stage)) ADTKey)
        (Arena.Arena (Type.TypeSynonym (TypeExpr stage)) TypeSynonymKey)
        (Arena.Arena Type.Var TypeVarKey)
        (Arena.Arena (BoundValue stage) BoundValueKey)
        ModuleKey

data Decl
    = Decl'Module ModuleKey
    | Decl'Type (Type.Type Void)
    deriving Show

data Module stage
    = Module ID.ModuleID [Binding stage] [ADTKey] [TypeSynonymKey]
deriving instance Stage.AllShowable stage => Show (Module stage)

data BoundValue stage
    = BoundValue ID.BoundValueID (Stage.TypeInfo stage) (Located Text)
    | BoundValue'ADTVariant ID.BoundValueID Type.ADTVariantIndex [Type.TypeVarKey] (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (BoundValue stage)

data Binding stage
    = Binding (Pattern stage) Span (Expr stage)
    | Binding'ADTVariant Span BoundValueKey [Type.TypeVarKey] Type.ADTVariantIndex
deriving instance Stage.AllShowable stage => Show (Binding stage)

type HoleIdentifier = Located [Located Text] -- TODO: disallow paths in holes?

data TypeExpr stage
    = TypeExpr'Identifier (Stage.TypeInfo stage) Span (Stage.DIden stage)
    | TypeExpr'Tuple (Stage.TypeInfo stage) (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Hole (Stage.TypeInfo stage) Span HoleIdentifier
    | TypeExpr'Function (Stage.TypeInfo stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Forall (Stage.TypeInfo stage) (NonEmpty TypeVarKey) (TypeExpr stage)
    | TypeExpr'Apply (Stage.TypeInfo stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Wild (Stage.TypeInfo stage) Span
    | TypeExpr'Poison (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (TypeExpr stage)

data Expr stage
    = Expr'Identifier ID.ExprID (Stage.TypeInfo stage) Span (Stage.VIden stage)
    | Expr'Char ID.ExprID (Stage.TypeInfo stage) Span Char
    | Expr'String ID.ExprID (Stage.TypeInfo stage) Span Text
    | Expr'Int ID.ExprID (Stage.TypeInfo stage) Span Integer
    | Expr'Float ID.ExprID (Stage.TypeInfo stage) Span Rational
    | Expr'Bool ID.ExprID (Stage.TypeInfo stage) Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)

    | Expr'Lambda ID.ExprID (Stage.TypeInfo stage) Span (Pattern stage) (Expr stage)

    | Expr'Let ID.ExprID (Stage.TypeInfo stage) Span [Binding stage] (Expr stage)
    | Expr'LetRec ID.ExprID (Stage.TypeInfo stage) Span [Binding stage] (Expr stage)

    | Expr'BinaryOps ID.ExprID (Stage.BinaryOpsAllowed stage) (Stage.TypeInfo stage) Span (Expr stage) [((Stage.VIden stage), Expr stage)]

    | Expr'Call ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)

    | Expr'If ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) (Expr stage) (Expr stage)
    | Expr'Match ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) [(Pattern stage, Expr stage)]

    | Expr'Forall ID.ExprID (Stage.TypeInfo stage) Span (NonEmpty TypeVarKey) (Expr stage)
    | Expr'TypeApply ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (TypeExpr stage)

    | Expr'TypeAnnotation ID.ExprID (Stage.TypeInfo stage) Span (TypeExpr stage) (Expr stage)

    | Expr'Hole ID.ExprID (Stage.TypeInfo stage) Span HoleIdentifier

    | Expr'Poison ID.ExprID (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (Expr stage)

data Pattern stage
    = Pattern'Identifier (Stage.TypeInfo stage) Span BoundValueKey
    | Pattern'Wildcard (Stage.TypeInfo stage) Span
    | Pattern'Tuple (Stage.TypeInfo stage) Span (Pattern stage) (Pattern stage)
    | Pattern'Named (Stage.TypeInfo stage) Span Span (Located BoundValueKey) (Pattern stage)
    | Pattern'AnonADTVariant (Stage.TypeInfo stage) Span (Stage.PIden stage) [(Stage.TypeInfo stage)] [Pattern stage]
    | Pattern'NamedADTVariant (Stage.TypeInfo stage) Span (Stage.PIden stage) [(Stage.TypeInfo stage)] [(Located Text, Pattern stage)]

    | Pattern'Poison (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (Pattern stage)

type_expr_type_info :: TypeExpr stage -> (Stage.TypeInfo stage)
type_expr_type_info (TypeExpr'Identifier type_info _ _) = type_info
type_expr_type_info (TypeExpr'Tuple type_info _ _) = type_info
type_expr_type_info (TypeExpr'Hole type_info _ _) = type_info
type_expr_type_info (TypeExpr'Function type_info _ _ _) = type_info
type_expr_type_info (TypeExpr'Forall type_info _ _) = type_info
type_expr_type_info (TypeExpr'Apply type_info _ _ _) = type_info
type_expr_type_info (TypeExpr'Wild type_info _) = type_info
type_expr_type_info (TypeExpr'Poison type_info _) = type_info

expr_type :: Expr stage -> (Stage.TypeInfo stage)
expr_type (Expr'Identifier _ type_info _ _) = type_info
expr_type (Expr'Char _ type_info _ _) = type_info
expr_type (Expr'String _ type_info _ _) = type_info
expr_type (Expr'Int _ type_info _ _) = type_info
expr_type (Expr'Float _ type_info _ _) = type_info
expr_type (Expr'Bool _ type_info _ _) = type_info
expr_type (Expr'Tuple _ type_info _ _ _) = type_info
expr_type (Expr'Lambda _ type_info _ _ _) = type_info
expr_type (Expr'Let _ type_info _ _ _) = type_info
expr_type (Expr'LetRec _ type_info _ _ _) = type_info
expr_type (Expr'BinaryOps _ _ type_info _ _ _) = type_info
expr_type (Expr'Call _ type_info _ _ _) = type_info
expr_type (Expr'If _ type_info _ _ _ _ _) = type_info
expr_type (Expr'Match _ type_info _ _ _ _) = type_info
expr_type (Expr'Poison _ type_info _) = type_info
expr_type (Expr'Hole _ type_info _ _) = type_info
expr_type (Expr'Forall _ type_info _ _ _) = type_info
expr_type (Expr'TypeApply _ type_info _ _ _) = type_info
expr_type (Expr'TypeAnnotation _ type_info _ _ _) = type_info

expr_span :: Expr stage -> Span
expr_span (Expr'Identifier _ _ sp _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _) = sp
expr_span (Expr'Let _ _ sp _ _) = sp
expr_span (Expr'LetRec _ _ sp _ _) = sp
expr_span (Expr'BinaryOps _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'If _ _ sp _ _ _ _) = sp
expr_span (Expr'Match _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
expr_span (Expr'Hole _ _ sp _) = sp
expr_span (Expr'Forall _ _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ sp _ _) = sp

pattern_type :: Pattern stage -> (Stage.TypeInfo stage)
pattern_type (Pattern'Identifier type_info _ _) = type_info
pattern_type (Pattern'Wildcard type_info _) = type_info
pattern_type (Pattern'Tuple type_info _ _ _) = type_info
pattern_type (Pattern'Named type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison type_info _) = type_info
pattern_type (Pattern'AnonADTVariant type_info _ _ _ _) = type_info
pattern_type (Pattern'NamedADTVariant type_info _ _ _ _) = type_info

pattern_span :: Pattern stage -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
pattern_span (Pattern'AnonADTVariant _ sp _ _ _) = sp
pattern_span (Pattern'NamedADTVariant _ sp _ _ _) = sp
