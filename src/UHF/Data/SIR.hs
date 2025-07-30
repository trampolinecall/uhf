{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.SIR
    ( SIR (..)
    , Stage.Stage (..)
    , CU (..)
    , ADT (..)
    , ADTVariant (..)
    , TypeSynonym (..)
    , DeclRef (..)
    , ExternPackage (..)
    , ModuleKey
    , Module (..)
    , ValueRef (..)
    , VariableKey
    , Variable (..)
    , Binding (..)
    , HoleIdentifier
    , TypeExpr (..)
    , SplitIdentifier (..)
    , ExprIdentifierRef
    , OperatorRef
    , Expr (..)
    , PatternADTVariantRef
    , Pattern (..)
    , expr_type
    , pattern_type
    , type_expr_evaled
    , split_identifier_resolved
    , expr_span
    , pattern_span
    , type_expr_span
    ) where

import UHF.Prelude

import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR.Stage as Stage
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

-- TODO: transpose grouping of these? ie restructure as files for ast, type synonyms, intrinsics, ..., and all related operations like child maps, getting children, ... together in thesdecls with all related thing in the same file

type AllShowable stage =
    ( Stage.AllShowable stage
    , Stage.IdenResolvedKeyHasInstance (Stage.TypeExprEvaledKey stage) Show stage
    , Stage.IdenResolvedKeyHasInstance (Stage.TypeExprEvaledAsTypeKey stage) Show stage
    , Stage.IdenResolvedKeyHasInstance (DeclRef (Stage.TypeInRefer stage)) Show stage
    , Stage.IdenResolvedKeyHasInstance ValueRef Show stage
    , Stage.IdenResolvedKeyHasInstance Type.ADT.VariantIndex Show stage
    )

-- "syntax based ir"
data SIR stage
    = SIR
    { sir_modules :: Arena.Arena (Module stage) ModuleKey
    , sir_adts :: Arena.Arena (ADT stage) ADTKey
    , sir_type_synonyms :: Arena.Arena (TypeSynonym stage) TypeSynonymKey
    , sir_quant_vars :: Arena.Arena Type.QuantVar QuantVarKey
    , sir_variables :: Arena.Arena (Variable stage) VariableKey
    , sir_cu :: CU stage
    }

-- TODO: when support for compiling libraries that should not need a main function, a field should be added that identifies whether or not the compilation unit is a library or an executable or this should be split into 2 constructors for libraries or executables
data CU stage = CU {cu_root_module :: ModuleKey, cu_main_function :: Maybe VariableKey}

data ExternPackage
    = ExternPackage'IntrinsicsPackage
    deriving Show

data Module stage
    = Module ID.ModuleID (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey]
deriving instance AllShowable stage => Show (Module stage)

data Variable stage
    = Variable ID.VariableID (Stage.TypeInfo stage) (Located Text)
deriving instance AllShowable stage => Show (Variable stage)

data Binding stage
    = Binding (Pattern stage) Span (Expr stage)
deriving instance AllShowable stage => Show (Binding stage)

type HoleIdentifier = Located Text

data DeclRef ty
    = DeclRef'Module ModuleKey
    | DeclRef'Type ty
    | DeclRef'ExternPackage ExternPackage -- TODO: change this to ExternModule? because referring to an external package would just refer to its root module
    deriving Show

data ADT stage = ADT ID.DeclID (Located Text) [QuantVarKey] [ADTVariant stage]
data ADTVariant stage
    = ADTVariant'Named (Located Text) ID.ADTVariantID [(ID.ADTFieldID, Text, TypeExpr stage)]
    | ADTVariant'Anon (Located Text) ID.ADTVariantID [(ID.ADTFieldID, TypeExpr stage)]

data TypeSynonym stage = TypeSynonym ID.DeclID (Located Text) (TypeExpr stage)

data TypeExpr stage
    = TypeExpr'Refer
        (Stage.TypeExprEvaledKey stage)
        (Stage.IdenResolvedKey stage (DeclRef (Stage.TypeInRefer stage)))
        Span
        (Stage.NameMapIndex stage)
        (Located Text)
    | TypeExpr'Get (Stage.TypeExprEvaledKey stage) (Stage.IdenResolvedKey stage (DeclRef (Stage.TypeInRefer stage))) Span (TypeExpr stage) (Located Text)
    | TypeExpr'Tuple (Stage.TypeExprEvaledKey stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Hole
        (Stage.TypeExprEvaledKey stage)
        (Stage.TypeExprEvaledAsTypeKey stage)
        Span
        HoleIdentifier
    | TypeExpr'Function (Stage.TypeExprEvaledKey stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Forall (Stage.TypeExprEvaledKey stage) Span (Stage.NameMapIndex stage) (NonEmpty QuantVarKey) (TypeExpr stage)
    | TypeExpr'Apply (Stage.TypeExprEvaledKey stage) Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Wild (Stage.TypeExprEvaledKey stage) Span
    | TypeExpr'Poison (Stage.TypeExprEvaledKey stage) Span
deriving instance AllShowable stage => Show (TypeExpr stage)

data SplitIdentifier resolved stage
    = SplitIdentifier'Get (TypeExpr stage) (Located Text) (Stage.IdenResolvedKey stage resolved)
    | SplitIdentifier'Single (Stage.NameMapIndex stage) (Located Text) (Stage.IdenResolvedKey stage resolved)
deriving instance (AllShowable stage, Stage.IdenResolvedKeyHasInstance resolved Show stage) => Show (SplitIdentifier resolved stage)

type ExprIdentifierRef stage = SplitIdentifier ValueRef stage
type OperatorRef stage = SplitIdentifier ValueRef stage

data ValueRef
    = ValueRef'Variable VariableKey
    | ValueRef'ADTVariantConstructor Type.ADT.VariantIndex
    | ValueRef'Intrinsic Intrinsics.Intrinsic
    deriving Show

data Expr stage
    = Expr'Refer ID.ExprID (Stage.TypeInfo stage) Span (ExprIdentifierRef stage)
    | Expr'Char ID.ExprID (Stage.TypeInfo stage) Span Char
    | Expr'String ID.ExprID (Stage.TypeInfo stage) Span Text
    | Expr'Int ID.ExprID (Stage.TypeInfo stage) Span Integer
    | Expr'Float ID.ExprID (Stage.TypeInfo stage) Span Rational
    | Expr'Bool ID.ExprID (Stage.TypeInfo stage) Span Bool -- TODO: replace with identifier exprs
    | Expr'Tuple ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)
    | Expr'Lambda ID.ExprID (Stage.TypeInfo stage) Span (Pattern stage) (Expr stage)
    | Expr'Let ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'LetRec ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'BinaryOps
        ID.ExprID
        (Stage.InfixGroupedKey stage)
        (Stage.TypeInfo stage)
        Span
        (Expr stage)
        [(Span, OperatorRef stage, Expr stage)]
    | Expr'Call ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)
    | Expr'If ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) (Expr stage) (Expr stage)
    | Expr'Match ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) [(Stage.NameMapIndex stage, Pattern stage, Expr stage)]
    | Expr'Forall ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) (NonEmpty QuantVarKey) (Expr stage)
    | Expr'TypeApply ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (TypeExpr stage, Stage.TypeExprEvaledAsTypeKey stage)
    | Expr'TypeAnnotation
        ID.ExprID
        (Stage.TypeInfo stage)
        Span
        (TypeExpr stage, Stage.TypeExprEvaledAsTypeKey stage)
        (Expr stage)
    | Expr'Hole ID.ExprID (Stage.TypeInfo stage) Span HoleIdentifier
    | Expr'Poison ID.ExprID (Stage.TypeInfo stage) Span
deriving instance AllShowable stage => Show (Expr stage)

type PatternADTVariantRef stage = SplitIdentifier Type.ADT.VariantIndex stage

data Pattern stage
    = Pattern'Variable (Stage.TypeInfo stage) Span VariableKey
    | Pattern'Wildcard (Stage.TypeInfo stage) Span
    | Pattern'Tuple (Stage.TypeInfo stage) Span (Pattern stage) (Pattern stage)
    | Pattern'Named (Stage.TypeInfo stage) Span Span (Located VariableKey) (Pattern stage)
    | Pattern'AnonADTVariant
        (Stage.TypeInfo stage)
        Span
        (PatternADTVariantRef stage)
        [Stage.TypeInfo stage]
        [Pattern stage]
    | Pattern'NamedADTVariant
        (Stage.TypeInfo stage)
        Span
        (PatternADTVariantRef stage)
        [Stage.TypeInfo stage]
        [(Located Text, Pattern stage)]
    | Pattern'Poison (Stage.TypeInfo stage) Span
deriving instance AllShowable stage => Show (Pattern stage)

type_expr_evaled :: TypeExpr stage -> Stage.TypeExprEvaledKey stage
type_expr_evaled (TypeExpr'Refer evaled _ _ _ _) = evaled
type_expr_evaled (TypeExpr'Get evaled _ _ _ _) = evaled
type_expr_evaled (TypeExpr'Tuple evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Hole evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Function evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Forall evaled _ _ _ _) = evaled
type_expr_evaled (TypeExpr'Apply evaled _ _ _) = evaled
type_expr_evaled (TypeExpr'Wild evaled _) = evaled
type_expr_evaled (TypeExpr'Poison evaled _) = evaled

type_expr_span :: TypeExpr stage -> Span
type_expr_span (TypeExpr'Refer _ _ span _ _) = span
type_expr_span (TypeExpr'Get _ _ span _ _) = span
type_expr_span (TypeExpr'Tuple _ span _ _) = span
type_expr_span (TypeExpr'Hole _ _ span _) = span
type_expr_span (TypeExpr'Function _ span _ _) = span
type_expr_span (TypeExpr'Forall _ span _ _ _) = span
type_expr_span (TypeExpr'Apply _ span _ _) = span
type_expr_span (TypeExpr'Wild _ span) = span
type_expr_span (TypeExpr'Poison _ span) = span

split_identifier_resolved :: SplitIdentifier resolved stage -> Stage.IdenResolvedKey stage resolved
split_identifier_resolved (SplitIdentifier'Get _ _ resolved) = resolved
split_identifier_resolved (SplitIdentifier'Single _ _ resolved) = resolved

expr_type :: Expr stage -> Stage.TypeInfo stage
expr_type (Expr'Refer _ type_info _ _) = type_info
expr_type (Expr'Char _ type_info _ _) = type_info
expr_type (Expr'String _ type_info _ _) = type_info
expr_type (Expr'Int _ type_info _ _) = type_info
expr_type (Expr'Float _ type_info _ _) = type_info
expr_type (Expr'Bool _ type_info _ _) = type_info
expr_type (Expr'Tuple _ type_info _ _ _) = type_info
expr_type (Expr'Lambda _ type_info _ _ _) = type_info
expr_type (Expr'Let _ type_info _ _ _ _ _ _) = type_info
expr_type (Expr'LetRec _ type_info _ _ _ _ _ _) = type_info
expr_type (Expr'BinaryOps _ _ type_info _ _ _) = type_info
expr_type (Expr'Call _ type_info _ _ _) = type_info
expr_type (Expr'If _ type_info _ _ _ _ _) = type_info
expr_type (Expr'Match _ type_info _ _ _ _) = type_info
expr_type (Expr'Poison _ type_info _) = type_info
expr_type (Expr'Hole _ type_info _ _) = type_info
expr_type (Expr'Forall _ type_info _ _ _ _) = type_info
expr_type (Expr'TypeApply _ type_info _ _ _) = type_info
expr_type (Expr'TypeAnnotation _ type_info _ _ _) = type_info

expr_span :: Expr stage -> Span
expr_span (Expr'Refer _ _ sp _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _) = sp
expr_span (Expr'Let _ _ sp _ _ _ _ _) = sp
expr_span (Expr'LetRec _ _ sp _ _ _ _ _) = sp
expr_span (Expr'BinaryOps _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'If _ _ sp _ _ _ _) = sp
expr_span (Expr'Match _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
expr_span (Expr'Hole _ _ sp _) = sp
expr_span (Expr'Forall _ _ sp _ _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ sp _ _) = sp

pattern_type :: Pattern stage -> Stage.TypeInfo stage
pattern_type (Pattern'Variable type_info _ _) = type_info
pattern_type (Pattern'Wildcard type_info _) = type_info
pattern_type (Pattern'Tuple type_info _ _ _) = type_info
pattern_type (Pattern'Named type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison type_info _) = type_info
pattern_type (Pattern'AnonADTVariant type_info _ _ _ _) = type_info
pattern_type (Pattern'NamedADTVariant type_info _ _ _ _) = type_info

pattern_span :: Pattern stage -> Span
pattern_span (Pattern'Variable _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
pattern_span (Pattern'AnonADTVariant _ sp _ _ _) = sp
pattern_span (Pattern'NamedADTVariant _ sp _ _ _) = sp
