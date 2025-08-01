{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.SIR
    ( SIR (..)
    , Stage.Stage (..)
    , CU (..)
    , ADT (..)
    -- , ADTVariant (..) TODO: sir-type
    , TypeSynonym (..)
    , ExternPackage (..)
    , ModuleKey
    , Module (..)
    , VariableKey
    , Variable (..)
    , Binding (..)
    , HoleIdentifier
    , TypeExpr (..)
    , SplitIdentifier (..)
    , ValueIden
    , Expr (..)
    , VariantIden
    , Pattern (..)
    , expr_type
    , pattern_type
    , type_expr_evaled
    , expr_span
    , pattern_span
    , type_expr_span
    , split_identifier_id
    ) where

import UHF.Prelude

import qualified UHF.Data.IR.ID as ID
import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.SIR.ID as SIR.ID -- TODO: import this as ID instead of SIR.ID when ID.gets renamed to MangleID
import qualified UHF.Data.SIR.Stage as Stage
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

-- TODO: transpose grouping of these? ie restructure as files for ast, type synonyms, intrinsics, ..., and all related operations like child maps, getting children, ... together in thesdecls with all related thing in the same file

type AllShowable stage =
    ( Stage.AllShowable stage
    )

-- "syntax based ir"
-- TODO: no more arenas now that maps are used
data SIR stage
    = SIR
    { sir_modules :: Arena.Arena (Module stage) ModuleKey
    , sir_adts :: Arena.Arena (ADT stage) ADTKey
    , sir_type_synonyms :: Arena.Arena (TypeSynonym stage) TypeSynonymKey
    , sir_quant_vars :: Arena.Arena Type.QuantVar QuantVarKey
    , sir_variables :: Arena.Arena (Variable stage) VariableKey
    , sir_cu :: CU stage
    }
deriving instance AllShowable stage => Show (SIR stage)

-- TODO: when support for compiling libraries that should not need a main function, a field should be added that identifies whether or not the compilation unit is a library or an executable or this should be split into 2 constructors for libraries or executables
data CU stage = CU {cu_root_module :: ModuleKey, cu_main_function :: Maybe VariableKey} deriving Show

-- TODO: make these into their own datatypes and do not share representation with types
type ADT stage = Type.ADT (TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")
type TypeSynonym stage = Type.TypeSynonym (TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")

data ExternPackage
    = ExternPackage'IntrinsicsPackage
    deriving Show

data Module stage
    = Module (SIR.ID.ID "Module") ID.ModuleID (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey]
deriving instance AllShowable stage => Show (Module stage)

data Variable stage
    = Variable (SIR.ID.ID "Variable") ID.VariableID (Stage.TypeInfo stage) (Located Text)
deriving instance AllShowable stage => Show (Variable stage)

data Binding stage
    = Binding (SIR.ID.ID "Binding") (Pattern stage) Span (Expr stage)
deriving instance AllShowable stage => Show (Binding stage)

type HoleIdentifier = Located Text

-- TODO: data ADT stage = ADT ID.DeclID (Located Text) [QuantVarKey] [ADTVariant stage]
-- TODO: data ADTVariant stage
-- TODO:     = ADTVariant'Named (Located Text) ID.ADTVariantID [(ID.ADTFieldID, Text, TypeExpr stage)]
-- TODO:     | ADTVariant'Anon (Located Text) ID.ADTVariantID [(ID.ADTFieldID, TypeExpr stage)]
-- TODO:
-- TODO: data TypeSynonym stage = TypeSynonym ID.DeclID (Located Text) (TypeExpr stage)

data TypeExpr stage
    = TypeExpr'Refer (SIR.ID.ID "TypeExpr") (SIR.ID.ID "DeclIden") Span (Stage.NameMapIndex stage) (Located Text)
    | TypeExpr'Get (SIR.ID.ID "TypeExpr") (SIR.ID.ID "DeclIden") Span (TypeExpr stage) (Located Text) -- TODO: remove this unit field
    | TypeExpr'Tuple (SIR.ID.ID "TypeExpr") Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Hole (SIR.ID.ID "TypeExpr") (SIR.ID.ID "TypeExprEvaledAsType") Span HoleIdentifier
    | TypeExpr'Function (SIR.ID.ID "TypeExpr") Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Forall (SIR.ID.ID "TypeExpr") Span (Stage.NameMapIndex stage) (NonEmpty QuantVarKey) (TypeExpr stage)
    | TypeExpr'Apply (SIR.ID.ID "TypeExpr") Span (TypeExpr stage) (TypeExpr stage)
    | TypeExpr'Wild (SIR.ID.ID "TypeExpr") Span
    | TypeExpr'Poison (SIR.ID.ID "TypeExpr") Span
deriving instance AllShowable stage => Show (TypeExpr stage)

data SplitIdentifier id_name stage
    = SplitIdentifier'Get (SIR.ID.ID id_name) (TypeExpr stage) (Located Text)
    | SplitIdentifier'Single (SIR.ID.ID id_name) (Stage.NameMapIndex stage) (Located Text)
deriving instance AllShowable stage => Show (SplitIdentifier id_name stage)

type ValueIden stage = SplitIdentifier "ValueIden" stage -- TODO: ValueIden (and VariantIden) are probably not the most accurate names to describe these

data Expr stage
    = Expr'Refer (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (ValueIden stage)
    | Expr'Char (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Char
    | Expr'String (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Text
    | Expr'Int (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Integer
    | Expr'Float (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Rational
    | Expr'Bool (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Bool -- TODO: replace with identifier exprs
    | Expr'Tuple (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)
    | Expr'Lambda (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Pattern stage) (Expr stage)
    | Expr'Let (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'LetRec (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) [Binding stage] [ADTKey] [TypeSynonymKey] (Expr stage)
    | Expr'BinaryOps
        (SIR.ID.ID "Expr")
        (SIR.ID.ID "BinaryOpsExpr")
        ID.ExprID
        (Stage.TypeInfo stage)
        Span
        (Expr stage)
        [(Span, ValueIden stage, Expr stage)]
    | Expr'Call (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (Expr stage)
    | Expr'If (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) (Expr stage) (Expr stage)
    | Expr'Match (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span Span (Expr stage) [(Stage.NameMapIndex stage, Pattern stage, Expr stage)]
    | Expr'Forall (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Stage.NameMapIndex stage) (NonEmpty QuantVarKey) (Expr stage)
    | Expr'TypeApply (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span (Expr stage) (TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")
    | Expr'TypeAnnotation
        (SIR.ID.ID "Expr")
        ID.ExprID
        (Stage.TypeInfo stage)
        Span
        (TypeExpr stage, SIR.ID.ID "TypeExprEvaledAsType")
        (Expr stage)
    | Expr'Hole (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span HoleIdentifier
    | Expr'Poison (SIR.ID.ID "Expr") ID.ExprID (Stage.TypeInfo stage) Span
deriving instance AllShowable stage => Show (Expr stage)

type VariantIden stage = SplitIdentifier "VariantIden" stage

data Pattern stage
    = Pattern'Variable (SIR.ID.ID "Pattern") (Stage.TypeInfo stage) Span VariableKey
    | Pattern'Wildcard (SIR.ID.ID "Pattern") (Stage.TypeInfo stage) Span
    | Pattern'Tuple (SIR.ID.ID "Pattern") (Stage.TypeInfo stage) Span (Pattern stage) (Pattern stage)
    | Pattern'Named (SIR.ID.ID "Pattern") (Stage.TypeInfo stage) Span Span (Located VariableKey) (Pattern stage)
    | Pattern'AnonADTVariant
        (SIR.ID.ID "Pattern")
        (Stage.TypeInfo stage)
        Span
        (VariantIden stage)
        [Stage.TypeInfo stage]
        [Pattern stage]
    | Pattern'NamedADTVariant
        (SIR.ID.ID "Pattern")
        (Stage.TypeInfo stage)
        Span
        (VariantIden stage)
        [Stage.TypeInfo stage]
        [(Located Text, Pattern stage)]
    | Pattern'Poison (SIR.ID.ID "Pattern") (Stage.TypeInfo stage) Span
deriving instance AllShowable stage => Show (Pattern stage)

type_expr_evaled :: TypeExpr stage -> SIR.ID.ID "TypeExpr"
type_expr_evaled (TypeExpr'Refer id _ _ _ _) = id
type_expr_evaled (TypeExpr'Get id _ _ _ _) = id
type_expr_evaled (TypeExpr'Tuple id _ _ _) = id
type_expr_evaled (TypeExpr'Hole id _ _ _) = id
type_expr_evaled (TypeExpr'Function id _ _ _) = id
type_expr_evaled (TypeExpr'Forall id _ _ _ _) = id
type_expr_evaled (TypeExpr'Apply id _ _ _) = id
type_expr_evaled (TypeExpr'Wild id _) = id
type_expr_evaled (TypeExpr'Poison id _) = id

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

split_identifier_id :: SplitIdentifier id_name stage -> SIR.ID.ID id_name
split_identifier_id (SplitIdentifier'Get id _ _) = id
split_identifier_id (SplitIdentifier'Single id _ _) = id

expr_type :: Expr stage -> Stage.TypeInfo stage
expr_type (Expr'Refer _ _ type_info _ _) = type_info
expr_type (Expr'Char _ _ type_info _ _) = type_info
expr_type (Expr'String _ _ type_info _ _) = type_info
expr_type (Expr'Int _ _ type_info _ _) = type_info
expr_type (Expr'Float _ _ type_info _ _) = type_info
expr_type (Expr'Bool _ _ type_info _ _) = type_info
expr_type (Expr'Tuple _ _ type_info _ _ _) = type_info
expr_type (Expr'Lambda _ _ type_info _ _ _) = type_info
expr_type (Expr'Let _ _ type_info _ _ _ _ _ _) = type_info
expr_type (Expr'LetRec _ _ type_info _ _ _ _ _ _) = type_info
expr_type (Expr'BinaryOps _ _ _ type_info _ _ _) = type_info
expr_type (Expr'Call _ _ type_info _ _ _) = type_info
expr_type (Expr'If _ _ type_info _ _ _ _ _) = type_info
expr_type (Expr'Match _ _ type_info _ _ _ _) = type_info
expr_type (Expr'Poison _ _ type_info _) = type_info
expr_type (Expr'Hole _ _ type_info _ _) = type_info
expr_type (Expr'Forall _ _ type_info _ _ _ _) = type_info
expr_type (Expr'TypeApply _ _ type_info _ _ _) = type_info
expr_type (Expr'TypeAnnotation _ _ type_info _ _ _) = type_info

expr_span :: Expr stage -> Span
expr_span (Expr'Refer _ _ _ sp _) = sp
expr_span (Expr'Char _ _ _ sp _) = sp
expr_span (Expr'String _ _ _ sp _) = sp
expr_span (Expr'Int _ _ _ sp _) = sp
expr_span (Expr'Float _ _ _ sp _) = sp
expr_span (Expr'Bool _ _ _ sp _) = sp
expr_span (Expr'Tuple _ _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ _ sp _ _) = sp
expr_span (Expr'Let _ _ _ sp _ _ _ _ _) = sp
expr_span (Expr'LetRec _ _ _ sp _ _ _ _ _) = sp
expr_span (Expr'BinaryOps _ _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ _ sp _ _) = sp
expr_span (Expr'If _ _ _ sp _ _ _ _) = sp
expr_span (Expr'Match _ _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ _ sp) = sp
expr_span (Expr'Hole _ _ _ sp _) = sp
expr_span (Expr'Forall _ _ _ sp _ _ _) = sp
expr_span (Expr'TypeApply _ _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ _ sp _ _) = sp

pattern_type :: Pattern stage -> Stage.TypeInfo stage
pattern_type (Pattern'Variable _ type_info _ _) = type_info
pattern_type (Pattern'Wildcard _ type_info _) = type_info
pattern_type (Pattern'Tuple _ type_info _ _ _) = type_info
pattern_type (Pattern'Named _ type_info _ _ _ _) = type_info
pattern_type (Pattern'Poison _ type_info _) = type_info
pattern_type (Pattern'AnonADTVariant _ type_info _ _ _ _) = type_info
pattern_type (Pattern'NamedADTVariant _ type_info _ _ _ _) = type_info

pattern_span :: Pattern stage -> Span
pattern_span (Pattern'Variable _ _ sp _) = sp
pattern_span (Pattern'Wildcard _ _ sp) = sp
pattern_span (Pattern'Tuple _ _ sp _ _) = sp
pattern_span (Pattern'Named _ _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ _ sp) = sp
pattern_span (Pattern'AnonADTVariant _ _ sp _ _ _) = sp
pattern_span (Pattern'NamedADTVariant _ _ sp _ _ _) = sp
