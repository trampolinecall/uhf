{-# LANGUAGE DeriveAnyClass #-}

module UHF.Data.AST where

-- TODO: rename this to literal ast and make it literally store tokens (is this actually necessary?)

import UHF.Prelude

import qualified UHF.Data.Token as Token
import UHF.Source.EqIgnoringSpans
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)

type WholeSpan = Span

data KeywordRef
    = KeywordRef'Path Type (Located Token.KeywordIdentifier)
    | KeywordRef'Single (Located Token.KeywordIdentifier)
    deriving (Generic, EqIgnoringSpans, Show)

data Operator
    = Operator'Path WholeSpan Type (Located Token.SymbolIdentifier)
    | Operator'Single (Located Token.SymbolIdentifier)
    deriving (Generic, EqIgnoringSpans, Show)

data Decl
    = Decl'Value WholeSpan Pattern (Located Token.Equal) Expr
    | Decl'Data WholeSpan (Located Token.AlphaIdentifier) [Located Token.AlphaIdentifier] [DataVariant]
    | Decl'TypeSyn WholeSpan (Located Token.AlphaIdentifier) Type
    -- TODO: | Decl'Import Type
    deriving (Generic, EqIgnoringSpans, Show)

data DataVariant
    = DataVariant'Anon (Located Token.AlphaIdentifier) [Type]
    | DataVariant'Named (Located Token.AlphaIdentifier) [(Located Token.AlphaIdentifier, Type)]
    -- TODO: keyword variants, which also replace named variants
    deriving (Generic, EqIgnoringSpans, Show)

data Type
    = Type'Refer (Located Token.AlphaIdentifier)
    | Type'Get WholeSpan Type (Located Token.AlphaIdentifier)
    | Type'Tuple WholeSpan [Type] -- TODO: anonymous named products? (ie field names, but no datatype name)
    | Type'Hole WholeSpan (Located Token.AlphaIdentifier)
    | Type'Function WholeSpan Type Type
    | Type'Forall WholeSpan [Located Token.AlphaIdentifier] Type
    | Type'Apply WholeSpan Type [Type]
    | Type'Wild WholeSpan -- TODO: come up with better name for this
    deriving (Generic, EqIgnoringSpans, Show)

data Expr
    = Expr'ReferAlpha WholeSpan (Maybe Type) (Located Token.AlphaIdentifier)
    | Expr'Char WholeSpan Char
    | Expr'String WholeSpan Text
    | Expr'Int WholeSpan Integer
    | Expr'Float WholeSpan Rational
    | Expr'Bool WholeSpan Bool -- TODO: replace with identifier exprs

    | Expr'Tuple WholeSpan [Expr]

    | Expr'Lambda WholeSpan [Pattern] Expr

    | Expr'Let WholeSpan [Decl] Expr
    | Expr'LetRec WholeSpan [Decl] Expr

    | Expr'BinaryOps WholeSpan Expr [(Operator, Expr)] -- TODO: fix this

    | Expr'Call WholeSpan Expr [Expr]

    | Expr'If WholeSpan (Located Token.If) Expr Expr Expr
    | Expr'Match WholeSpan (Located Token.Match) Expr [(Pattern, Expr)]

    | Expr'Forall WholeSpan [Located Token.AlphaIdentifier] Expr -- TODO: add constraints like '#(T, U; Constraint#(T, U)) ...' (actually this todo is outdated with the syntax redesign in the lr1 overhaul)
    | Expr'TypeApply WholeSpan Expr [Type]

    | Expr'TypeAnnotation WholeSpan Type Expr

    | Expr'Hole WholeSpan (Located Token.AlphaIdentifier)
    deriving (Generic, EqIgnoringSpans, Show)

data Pattern
    -- TODO: symbol and keyword patterns
    = Pattern'AlphaVar (Located Token.AlphaIdentifier)
    | Pattern'Wildcard (Located Token.Underscore)
    | Pattern'Tuple WholeSpan [Pattern]
    -- TODO: symbol and keyword named patterns
    | Pattern'NamedAlpha WholeSpan (Located Token.AlphaIdentifier) (Located Token.At) Pattern
    | Pattern'AnonADTVariant WholeSpan (Maybe Type) (Located Token.AlphaIdentifier) [Pattern]
    | Pattern'NamedADTVariant WholeSpan (Maybe Type) (Located Token.AlphaIdentifier) [(Located Token.AlphaIdentifier, Pattern)]
    deriving (Generic, EqIgnoringSpans, Show)

type_span :: Type -> WholeSpan
type_span (Type'Refer iden) = just_span iden
type_span (Type'Get sp _ _) = sp
type_span (Type'Tuple sp _) = sp
type_span (Type'Hole sp _) = sp
type_span (Type'Function sp _ _) = sp
type_span (Type'Forall sp _ _) = sp
type_span (Type'Apply sp _ _) = sp
type_span (Type'Wild sp) = sp

expr_span :: Expr -> WholeSpan
expr_span (Expr'ReferAlpha sp _ _) = sp
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

pattern_span :: Pattern -> WholeSpan
pattern_span (Pattern'AlphaVar i) = just_span i
pattern_span (Pattern'Wildcard underscore) = just_span underscore
pattern_span (Pattern'Tuple sp _) = sp
pattern_span (Pattern'NamedAlpha sp _ _ _) = sp
pattern_span (Pattern'AnonADTVariant sp _ _ _) = sp
pattern_span (Pattern'NamedADTVariant sp _ _ _) = sp
