module UHF.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

type Identifier = Located [Located Text]

-- TODO: make all asts store spans (some do right now based on where they are needed in the later phases, but all of them should have one just for consistency)

data Decl
    = Decl'Value Pattern Span Expr
    | Decl'Data Identifier [DataVariant]
    | Decl'TypeSyn Identifier Type
    -- TODO: | Decl'Import Type
    deriving (Eq, Show)

data DataVariant
    = DataVariant'Anon Identifier [Type]
    | DataVariant'Named Identifier [(Identifier, Type)]
    deriving (Eq, Show)

data Type
    = Type'Identifier Identifier
    | Type'Tuple Span [Type] -- TODO: anonymous named products? (ie field names, but no datatype name)
    deriving (Eq, Show)

data Expr
    = Expr'Identifier Identifier
    | Expr'Char Span Char
    | Expr'String Span Text
    | Expr'Int Span Integer
    | Expr'Float Span Rational
    | Expr'Bool Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple Span [Expr]

    | Expr'Lambda Span [Pattern] Expr

    | Expr'Let Span [Decl] Expr
    | Expr'LetRec Span [Decl] Expr

    | Expr'BinaryOps Span Expr [(Identifier, Expr)]

    | Expr'Call Span Expr [Expr]

    | Expr'If Span Span Expr Expr Expr
    | Expr'Case Span Span Expr [(Pattern, Expr)]

    | Expr'TypeAnnotation Span Type Expr
    deriving (Eq, Show)

data Pattern
    = Pattern'Identifier Identifier -- TODO: decide whether '_' should be this or a separate variant
    | Pattern'Tuple Span [Pattern]
    | Pattern'Named Span Identifier Span Pattern
    deriving (Eq, Show)

expr_span :: Expr -> Span
expr_span (Expr'Identifier iden) = just_span iden
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
expr_span (Expr'Case sp _ _ _) = sp
expr_span (Expr'TypeAnnotation sp _ _) = sp

pattern_span :: Pattern -> Span
pattern_span (Pattern'Identifier i) = just_span i
pattern_span (Pattern'Tuple sp _) = sp
pattern_span (Pattern'Named sp _ _ _) = sp
