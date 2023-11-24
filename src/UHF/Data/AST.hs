{-# LANGUAGE DeriveAnyClass #-}

module UHF.Data.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))
import UHF.IO.EqIgnoringSpans

type Identifier = Located Text

-- TODO: make all asts store spans (some do right now based on where they are needed in the later phases, but all of them should have one just for consistency)

data Decl
    = Decl'Value Pattern Span Expr
    | Decl'Data Identifier [Identifier] [DataVariant]
    | Decl'TypeSyn Identifier Type
    -- TODO: | Decl'Import Type
    deriving (Generic, EqIgnoringSpans, Show)

data DataVariant
    = DataVariant'Anon Identifier [Type]
    | DataVariant'Named Identifier [(Identifier, Type)]
    deriving (Generic, EqIgnoringSpans, Show)

data Type
    = Type'Refer Identifier
    | Type'Get Span Type Identifier
    | Type'Tuple Span [Type] -- TODO: anonymous named products? (ie field names, but no datatype name)
    | Type'Hole Span Identifier
    | Type'Function Span Type Type
    | Type'Forall Span [Identifier] Type
    | Type'Apply Span Type [Type]
    | Type'Wild Span -- TODO: come up with better name for this
    deriving (Generic, EqIgnoringSpans, Show)

data PathOrSingleIden
    = PathOrSingleIden'Path Type Identifier
    | PathOrSingleIden'Single Identifier
    deriving (Generic, EqIgnoringSpans, Show)

data Expr
    = Expr'Identifier Span PathOrSingleIden
    | Expr'Char Span Char
    | Expr'String Span Text
    | Expr'Int Span Integer
    | Expr'Float Span Rational
    | Expr'Bool Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple Span [Expr]

    | Expr'Lambda Span [Pattern] Expr

    | Expr'Let Span [Decl] Expr
    | Expr'LetRec Span [Decl] Expr

    | Expr'BinaryOps Span Expr [(Located PathOrSingleIden, Expr)]

    | Expr'Call Span Expr [Expr]

    | Expr'If Span Span Expr Expr Expr
    | Expr'Match Span Span Expr [(Pattern, Expr)]

    | Expr'Forall Span [Identifier] Expr -- TODO: add constraints like '#(T, U; Constraint#(T, U)) ...'
    | Expr'TypeApply Span Expr [Type]

    | Expr'TypeAnnotation Span Type Expr

    | Expr'Hole Span Identifier
    deriving (Generic, EqIgnoringSpans, Show)

data Pattern
    = Pattern'Identifier Identifier
    | Pattern'Wildcard Span
    | Pattern'Tuple Span [Pattern]
    | Pattern'Named Span Identifier Span Pattern
    | Pattern'AnonADTVariant Span PathOrSingleIden [Pattern]
    | Pattern'NamedADTVariant Span PathOrSingleIden [(Identifier, Pattern)]
    deriving (Generic, EqIgnoringSpans, Show)

type_span :: Type -> Span
type_span (Type'Refer iden) = just_span iden
type_span (Type'Get sp _ _) = sp
type_span (Type'Tuple sp _) = sp
type_span (Type'Hole sp _) = sp
type_span (Type'Function sp _ _) = sp
type_span (Type'Forall sp _ _) = sp
type_span (Type'Apply sp _ _) = sp
type_span (Type'Wild sp) = sp

expr_span :: Expr -> Span
expr_span (Expr'Identifier sp _) = sp
expr_span (Expr'Char sp _) = sp
expr_span (Expr'String sp _) = sp
expr_span (Expr'Int sp _) = sp
expr_span (Expr'Float sp _) = sp
expr_span (Expr'Bool sp _) = sp
expr_span (Expr'Tuple sp _) = sp
expr_span (Expr'Lambda sp _ _) = sp
expr_span (Expr'Let sp _ _) = sp
expr_span (Expr'LetRec sp _ _) = sp
expr_span (Expr'BinaryOps sp _ _) = sp
expr_span (Expr'Call sp _ _) = sp
expr_span (Expr'If sp _ _ _ _) = sp
expr_span (Expr'Match sp _ _ _) = sp
expr_span (Expr'Forall sp _ _) = sp
expr_span (Expr'TypeApply sp _ _) = sp
expr_span (Expr'TypeAnnotation sp _ _) = sp
expr_span (Expr'Hole sp _) = sp

pattern_span :: Pattern -> Span
pattern_span (Pattern'Identifier i) = just_span i
pattern_span (Pattern'Wildcard sp) = sp
pattern_span (Pattern'Tuple sp _) = sp
pattern_span (Pattern'Named sp _ _ _) = sp
pattern_span (Pattern'AnonADTVariant sp _ _) = sp
pattern_span (Pattern'NamedADTVariant sp _ _) = sp
