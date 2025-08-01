{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module UHF.Data.SIR
    ( SIR (..)
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
    , type_expr_evaled
    , expr_span
    , pattern_span
    , type_expr_span
    , split_identifier_id
    , pattern_id
    , expr_id
    ) where

import UHF.Prelude

import qualified UHF.Data.IR.ID as ID
import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.SIR.ID as SIR.ID -- TODO: import this as ID instead of SIR.ID when ID.gets renamed to MangleID
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

-- TODO: transpose grouping of these? ie restructure as files for ast, type synonyms, intrinsics, ..., and all related operations like child maps, getting children, ... together in thesdecls with all related thing in the same file

-- "syntax based ir"
-- TODO: no more arenas now that maps are used
data SIR
    = SIR
    { sir_modules :: Arena.Arena Module ModuleKey
    , sir_adts :: Arena.Arena ADT ADTKey
    , sir_type_synonyms :: Arena.Arena TypeSynonym TypeSynonymKey
    , sir_quant_vars :: Arena.Arena Type.QuantVar QuantVarKey
    , sir_variables :: Arena.Arena Variable VariableKey
    , sir_cu :: CU
    }
    deriving Show

-- TODO: when support for compiling libraries that should not need a main function, a field should be added that identifies whether or not the compilation unit is a library or an executable or this should be split into 2 constructors for libraries or executables
data CU = CU {cu_root_module :: ModuleKey, cu_main_function :: Maybe VariableKey} deriving Show

-- TODO: make these into their own datatypes and do not share representation with types
type ADT = Type.ADT (TypeExpr, SIR.ID.ID "TypeExprEvaledAsType")
type TypeSynonym = Type.TypeSynonym (TypeExpr, SIR.ID.ID "TypeExprEvaledAsType")

data ExternPackage
    = ExternPackage'IntrinsicsPackage
    deriving Show

data Module
    = Module (SIR.ID.ID "Module") (SIR.ID.ID "HasChildNameContext") ID.ModuleID [Binding] [ADTKey] [TypeSynonymKey]
    deriving Show

data Variable
    = Variable (SIR.ID.ID "Variable") ID.VariableID (Located Text)
    deriving Show

data Binding
    = Binding (SIR.ID.ID "Binding") Pattern Span Expr
    deriving Show

type HoleIdentifier = Located Text

-- TODO: data ADT = ADT ID.DeclID (Located Text) [QuantVarKey] [ADTVariant]
-- TODO: data ADTVariant
-- TODO:     = ADTVariant'Named (Located Text) ID.ADTVariantID [(ID.ADTFieldID, Text, TypeExpr)]
-- TODO:     | ADTVariant'Anon (Located Text) ID.ADTVariantID [(ID.ADTFieldID, TypeExpr)]
-- TODO:
-- TODO: data TypeSynonym = TypeSynonym ID.DeclID (Located Text) TypeExpr

data TypeExpr
    = TypeExpr'Refer (SIR.ID.ID "TypeExpr") (SIR.ID.ID "DeclIden") (SIR.ID.ID "HasEnclosingNameContext") Span (Located Text)
    | TypeExpr'Get (SIR.ID.ID "TypeExpr") (SIR.ID.ID "DeclIden") Span TypeExpr (Located Text) -- TODO: remove this unit field
    | TypeExpr'Tuple (SIR.ID.ID "TypeExpr") Span TypeExpr TypeExpr
    | TypeExpr'Hole (SIR.ID.ID "TypeExpr") (SIR.ID.ID "TypeExprEvaledAsType") Span HoleIdentifier
    | TypeExpr'Function (SIR.ID.ID "TypeExpr") Span TypeExpr TypeExpr
    | TypeExpr'Forall (SIR.ID.ID "TypeExpr") (SIR.ID.ID "HasChildNameContext") Span (NonEmpty QuantVarKey) TypeExpr
    | TypeExpr'Apply (SIR.ID.ID "TypeExpr") Span TypeExpr TypeExpr
    | TypeExpr'Wild (SIR.ID.ID "TypeExpr") Span
    | TypeExpr'Poison (SIR.ID.ID "TypeExpr") Span
    deriving Show

data SplitIdentifier id_name
    = SplitIdentifier'Get (SIR.ID.ID id_name) TypeExpr (Located Text)
    | SplitIdentifier'Single (SIR.ID.ID id_name) (SIR.ID.ID "HasEnclosingNameContext") (Located Text)
    deriving Show

type ValueIden = SplitIdentifier "ValueIden" -- TODO: ValueIden (and VariantIden) are probably not the most accurate names to describe these

data Expr
    = Expr'Refer (SIR.ID.ID "Expr") ID.ExprID Span ValueIden
    | Expr'Char (SIR.ID.ID "Expr") ID.ExprID Span Char
    | Expr'String (SIR.ID.ID "Expr") ID.ExprID Span Text
    | Expr'Int (SIR.ID.ID "Expr") ID.ExprID Span Integer
    | Expr'Float (SIR.ID.ID "Expr") ID.ExprID Span Rational
    | Expr'Bool (SIR.ID.ID "Expr") ID.ExprID Span Bool -- TODO: replace with identifier exprs
    | Expr'Tuple (SIR.ID.ID "Expr") ID.ExprID Span Expr Expr
    | Expr'Lambda (SIR.ID.ID "Expr") ID.ExprID Span Pattern Expr
    | Expr'Let (SIR.ID.ID "Expr") (SIR.ID.ID "HasChildNameContext") ID.ExprID Span [Binding] [ADTKey] [TypeSynonymKey] Expr
    | Expr'LetRec (SIR.ID.ID "Expr") (SIR.ID.ID "HasChildNameContext") ID.ExprID Span [Binding] [ADTKey] [TypeSynonymKey] Expr
    | Expr'BinaryOps
        (SIR.ID.ID "Expr")
        (SIR.ID.ID "BinaryOpsExpr")
        ID.ExprID
        Span
        Expr
        [(Span, ValueIden, Expr)]
    | Expr'Call (SIR.ID.ID "Expr") ID.ExprID Span Expr Expr
    | Expr'If (SIR.ID.ID "Expr") ID.ExprID Span Span Expr Expr Expr
    | Expr'Match (SIR.ID.ID "Expr") ID.ExprID Span Span Expr [(SIR.ID.ID "HasChildNameContext", Pattern, Expr)]
    | Expr'Forall (SIR.ID.ID "Expr") (SIR.ID.ID "HasChildNameContext") ID.ExprID Span (NonEmpty QuantVarKey) Expr
    | Expr'TypeApply (SIR.ID.ID "Expr") ID.ExprID Span Expr (TypeExpr, SIR.ID.ID "TypeExprEvaledAsType")
    | Expr'TypeAnnotation
        (SIR.ID.ID "Expr")
        ID.ExprID
        Span
        (TypeExpr, SIR.ID.ID "TypeExprEvaledAsType")
        Expr
    | Expr'Hole (SIR.ID.ID "Expr") ID.ExprID Span HoleIdentifier
    | Expr'Poison (SIR.ID.ID "Expr") ID.ExprID Span
    deriving Show

type VariantIden = SplitIdentifier "VariantIden"

data Pattern
    = Pattern'Variable (SIR.ID.ID "Pattern") Span VariableKey
    | Pattern'Wildcard (SIR.ID.ID "Pattern") Span
    | Pattern'Tuple (SIR.ID.ID "Pattern") Span Pattern Pattern
    | Pattern'Named (SIR.ID.ID "Pattern") Span Span (Located VariableKey) Pattern
    | Pattern'AnonADTVariant (SIR.ID.ID "Pattern") (SIR.ID.ID "VariantPattern") Span VariantIden [Pattern]
    | Pattern'NamedADTVariant (SIR.ID.ID "Pattern") (SIR.ID.ID "VariantPattern") Span VariantIden [(Located Text, Pattern)]
    | Pattern'Poison (SIR.ID.ID "Pattern") Span
    deriving Show

type_expr_evaled :: TypeExpr -> SIR.ID.ID "TypeExpr"
type_expr_evaled (TypeExpr'Refer id _ _ _ _) = id
type_expr_evaled (TypeExpr'Get id _ _ _ _) = id
type_expr_evaled (TypeExpr'Tuple id _ _ _) = id
type_expr_evaled (TypeExpr'Hole id _ _ _) = id
type_expr_evaled (TypeExpr'Function id _ _ _) = id
type_expr_evaled (TypeExpr'Forall id _ _ _ _) = id
type_expr_evaled (TypeExpr'Apply id _ _ _) = id
type_expr_evaled (TypeExpr'Wild id _) = id
type_expr_evaled (TypeExpr'Poison id _) = id

type_expr_span :: TypeExpr -> Span
type_expr_span (TypeExpr'Refer _ _ _ span _) = span
type_expr_span (TypeExpr'Get _ _ span _ _) = span
type_expr_span (TypeExpr'Tuple _ span _ _) = span
type_expr_span (TypeExpr'Hole _ _ span _) = span
type_expr_span (TypeExpr'Function _ span _ _) = span
type_expr_span (TypeExpr'Forall _ _ span _ _) = span
type_expr_span (TypeExpr'Apply _ span _ _) = span
type_expr_span (TypeExpr'Wild _ span) = span
type_expr_span (TypeExpr'Poison _ span) = span

split_identifier_id :: SplitIdentifier id_name -> SIR.ID.ID id_name
split_identifier_id (SplitIdentifier'Get id _ _) = id
split_identifier_id (SplitIdentifier'Single id _ _) = id

expr_id :: Expr -> SIR.ID.ID "Expr"
expr_id (Expr'Refer id _ _ _) = id
expr_id (Expr'Char id _ _ _) = id
expr_id (Expr'String id _ _ _) = id
expr_id (Expr'Int id _ _ _) = id
expr_id (Expr'Float id _ _ _) = id
expr_id (Expr'Bool id _ _ _) = id
expr_id (Expr'Tuple id _ _ _ _) = id
expr_id (Expr'Lambda id _ _ _ _) = id
expr_id (Expr'Let id _ _ _ _ _ _ _) = id
expr_id (Expr'LetRec id _ _ _ _ _ _ _) = id
expr_id (Expr'BinaryOps id _ _ _ _ _) = id
expr_id (Expr'Call id _ _ _ _) = id
expr_id (Expr'If id _ _ _ _ _ _) = id
expr_id (Expr'Match id _ _ _ _ _) = id
expr_id (Expr'Poison id _ _) = id
expr_id (Expr'Hole id _ _ _) = id
expr_id (Expr'Forall id _ _ _ _ _) = id
expr_id (Expr'TypeApply id _ _ _ _) = id
expr_id (Expr'TypeAnnotation id _ _ _ _) = id

expr_span :: Expr -> Span
expr_span (Expr'Refer _ _ sp _) = sp
expr_span (Expr'Char _ _ sp _) = sp
expr_span (Expr'String _ _ sp _) = sp
expr_span (Expr'Int _ _ sp _) = sp
expr_span (Expr'Float _ _ sp _) = sp
expr_span (Expr'Bool _ _ sp _) = sp
expr_span (Expr'Tuple _ _ sp _ _) = sp
expr_span (Expr'Lambda _ _ sp _ _) = sp
expr_span (Expr'Let _ _ _ sp _ _ _ _) = sp
expr_span (Expr'LetRec _ _ _ sp _ _ _ _) = sp
expr_span (Expr'BinaryOps _ _ _ sp _ _) = sp
expr_span (Expr'Call _ _ sp _ _) = sp
expr_span (Expr'If _ _ sp _ _ _ _) = sp
expr_span (Expr'Match _ _ sp _ _ _) = sp
expr_span (Expr'Poison _ _ sp) = sp
expr_span (Expr'Hole _ _ sp _) = sp
expr_span (Expr'Forall _ _ _ sp _ _) = sp
expr_span (Expr'TypeApply _ _ sp _ _) = sp
expr_span (Expr'TypeAnnotation _ _ sp _ _) = sp

pattern_id :: Pattern -> SIR.ID.ID "Pattern"
pattern_id (Pattern'Variable id _ _) = id
pattern_id (Pattern'Wildcard id _) = id
pattern_id (Pattern'Tuple id _ _ _) = id
pattern_id (Pattern'Named id _ _ _ _) = id
pattern_id (Pattern'Poison id _) = id
pattern_id (Pattern'AnonADTVariant id _ _ _ _) = id
pattern_id (Pattern'NamedADTVariant id _ _ _ _) = id

pattern_span :: Pattern -> Span
pattern_span (Pattern'Variable _ sp _) = sp
pattern_span (Pattern'Wildcard _ sp) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
pattern_span (Pattern'AnonADTVariant _ _ sp _ _) = sp
pattern_span (Pattern'NamedADTVariant _ _ sp _ _) = sp
