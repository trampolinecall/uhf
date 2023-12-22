{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.SIR
    ( SIR (..)
    , Stage.Stage (..)

    , Decl (..)

    , ModuleKey
    , Module (..)

    , BoundValue (..)

    , VariableKey
    , Variable (..)

    , Binding (..)

    , HoleIdentifier

    , TypeExpr (..)
    , SplitIdentifier (..)
    , Expr (..)
    , Pattern (..)
    , expr_type
    , pattern_type
    , type_expr_evaled
    , expr_span
    , pattern_span
    , type_expr_span

    ) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.SIR.Stage as Stage
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT

-- "syntax based ir"
data SIR stage
    = SIR
        (Arena.Arena (Module stage) ModuleKey)
        (Arena.Arena (Type.ADT (TypeExpr stage, Stage.TypeExprEvaledAsType stage)) ADTKey)
        (Arena.Arena (Type.TypeSynonym (TypeExpr stage, Stage.TypeExprEvaledAsType stage)) TypeSynonymKey)
        (Arena.Arena Type.QuantVar QuantVarKey)
        (Arena.Arena (Variable stage) VariableKey)
        ModuleKey

data Decl ty
    = Decl'Module ModuleKey
    | Decl'Type ty
    deriving Show

data Module stage
    = Module ID.ModuleID [Binding stage] [ADTKey] [TypeSynonymKey]
deriving instance Stage.AllShowable stage => Show (Module stage)

data Variable stage
    = Variable ID.VariableID (Stage.TypeInfo stage) (Located Text)
deriving instance Stage.AllShowable stage => Show (Variable stage)

data BoundValue
    = BoundValue'Variable VariableKey
    | BoundValue'ADTVariant Type.ADT.VariantIndex
    deriving Show

data Binding stage
    = Binding (Pattern stage) Span (Expr stage)
deriving instance Stage.AllShowable stage => Show (Binding stage)

type HoleIdentifier = Located Text

data TypeExpr stage
    = TypeExpr'Refer (Stage.TypeExprEvaled stage) Span (Stage.DIdenStart stage)
    | TypeExpr'Get (Stage.TypeExprEvaled stage) Span (TypeExpr stage) (Located Text)
    | TypeExpr'Tuple (Stage.TypeExprEvaled stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Hole (Stage.TypeExprEvaled stage) (Stage.TypeExprEvaledAsType stage) Span HoleIdentifier
    | TypeExpr'Function (Stage.TypeExprEvaled stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Forall (Stage.TypeExprEvaled stage) Span (NonEmpty QuantVarKey) (TypeExpr stage)
    | TypeExpr'Apply (Stage.TypeExprEvaled stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Wild (Stage.TypeExprEvaled stage) Span
    | TypeExpr'Poison (Stage.TypeExprEvaled stage) Span
deriving instance Stage.AllShowable stage => Show (TypeExpr stage)

-- TODO: rename all Identifier to Refer
data SplitIdentifier stage start
    = SplitIdentifier'Get (TypeExpr stage) (Located Text)
    | SplitIdentifier'Single start
deriving instance (Stage.AllShowable stage, Show start) => Show (SplitIdentifier stage start)

data Expr stage
    = Expr'Identifier ID.ExprID (Stage.TypeInfo stage) Span (SplitIdentifier stage (Stage.VIdenStart stage)) (Stage.VIdenResolved stage)
    | Expr'Char ID.ExprID (Stage.TypeInfo stage) Span Char
    | Expr'String ID.ExprID (Stage.TypeInfo stage) Span Text
    | Expr'Int ID.ExprID (Stage.TypeInfo stage) Span Integer
    | Expr'Float ID.ExprID (Stage.TypeInfo stage) Span Rational
    | Expr'Bool ID.ExprID (Stage.TypeInfo stage) Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)

    | Expr'Lambda ID.ExprID (Stage.TypeInfo stage) Span (Pattern stage) (Expr stage)

    | Expr'Let ID.ExprID (Stage.TypeInfo stage) Span [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'LetRec ID.ExprID (Stage.TypeInfo stage) Span [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)

    | Expr'BinaryOps ID.ExprID (Stage.BinaryOpsAllowed stage) (Stage.TypeInfo stage) Span (Expr stage) [(Span, SplitIdentifier stage (Stage.VIdenStart stage), Stage.VIdenResolved stage, Expr stage)]

    | Expr'Call ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)

    | Expr'If ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) (Expr stage) (Expr stage)
    | Expr'Match ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) [(Pattern stage, Expr stage)]

    | Expr'Forall ID.ExprID (Stage.TypeInfo stage) Span (NonEmpty QuantVarKey) (Expr stage)
    | Expr'TypeApply ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (TypeExpr stage, Stage.TypeExprEvaledAsType stage)

    | Expr'TypeAnnotation ID.ExprID (Stage.TypeInfo stage) Span (TypeExpr stage, Stage.TypeExprEvaledAsType stage) (Expr stage)

    | Expr'Hole ID.ExprID (Stage.TypeInfo stage) Span HoleIdentifier

    | Expr'Poison ID.ExprID (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (Expr stage)

data Pattern stage
    = Pattern'Identifier (Stage.TypeInfo stage) Span VariableKey
    | Pattern'Wildcard (Stage.TypeInfo stage) Span
    | Pattern'Tuple (Stage.TypeInfo stage) Span (Pattern stage) (Pattern stage)
    | Pattern'Named (Stage.TypeInfo stage) Span Span (Located VariableKey) (Pattern stage)
    | Pattern'AnonADTVariant (Stage.TypeInfo stage) Span (SplitIdentifier stage (Stage.PIdenStart stage)) (Stage.PIdenResolved stage) [Stage.TypeInfo stage] [Pattern stage]
    | Pattern'NamedADTVariant (Stage.TypeInfo stage) Span (SplitIdentifier stage (Stage.PIdenStart stage)) (Stage.PIdenResolved stage) [Stage.TypeInfo stage] [(Located Text, Pattern stage)]

    | Pattern'Poison (Stage.TypeInfo stage) Span
deriving instance Stage.AllShowable stage => Show (Pattern stage)

type_expr_evaled :: TypeExpr stage -> Stage.TypeExprEvaled stage
type_expr_evaled (TypeExpr'Refer evaled _ _) = evaled
type_expr_evaled (TypeExpr'Get evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Tuple evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Hole evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Function evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Forall evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Apply evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Wild evaled _) = evaled
type_expr_evaled (TypeExpr'Poison evaled _) = evaled

type_expr_span :: TypeExpr stage -> Span
type_expr_span (TypeExpr'Refer _ span _) = span
type_expr_span (TypeExpr'Get _ span _ _) = span
type_expr_span (TypeExpr'Tuple _ span _ _) = span
type_expr_span (TypeExpr'Hole _ _ span _) = span
type_expr_span (TypeExpr'Function _ span _ _) = span
type_expr_span (TypeExpr'Forall _ span _ _) = span
type_expr_span (TypeExpr'Apply _ span _ _) = span
type_expr_span (TypeExpr'Wild _ span) = span
type_expr_span (TypeExpr'Poison _ span) = span

expr_type :: Expr stage -> Stage.TypeInfo stage
expr_type (Expr'Identifier _ type_info _ _ _) = type_info
expr_type (Expr'Char _ type_info _ _) = type_info
expr_type (Expr'String _ type_info _ _) = type_info
expr_type (Expr'Int _ type_info _ _) = type_info
expr_type (Expr'Float _ type_info _ _) = type_info
expr_type (Expr'Bool _ type_info _ _) = type_info
expr_type (Expr'Tuple _ type_info _ _ _) = type_info
expr_type (Expr'Lambda _ type_info _ _ _) = type_info
expr_type (Expr'Let _ type_info _ _ _ _ _) = type_info
expr_type (Expr'LetRec _ type_info _ _ _ _ _) = type_info
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
expr_span (Expr'Identifier _ _ sp _ _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _) = sp
expr_span (Expr'Let _ _ sp _ _ _ _) = sp
expr_span (Expr'LetRec _ _ sp _ _ _ _) = sp
expr_span (Expr'BinaryOps _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'If _ _ sp _ _ _ _) = sp
expr_span (Expr'Match _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
expr_span (Expr'Hole _ _ sp _) = sp
expr_span (Expr'Forall _ _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ sp _ _) = sp

pattern_type :: Pattern stage -> Stage.TypeInfo stage
pattern_type (Pattern'Identifier type_info _ _) = type_info
pattern_type (Pattern'Wildcard type_info _) = type_info
pattern_type (Pattern'Tuple type_info _ _ _) = type_info
pattern_type (Pattern'Named type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison type_info _) = type_info
pattern_type (Pattern'AnonADTVariant type_info _ _ _ _ _) = type_info
pattern_type (Pattern'NamedADTVariant type_info _ _ _ _ _) = type_info

pattern_span :: Pattern stage -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
pattern_span (Pattern'AnonADTVariant _ sp _ _ _ _) = sp
pattern_span (Pattern'NamedADTVariant _ sp _ _ _ _) = sp
