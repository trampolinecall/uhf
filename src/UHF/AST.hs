module UHF.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

import UHF.IO.Location (Located, Span)

type Identifier = Located [Located Text]

-- TODO: make all asts store spans (some do right now based on where they are needed in the later phases, but all of them should have one just for consistency)

data Decl
    = Decl'Value Pattern Expr
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
    | Type'Tuple [Type] -- TODO: anonymous named products? (ie field names, but no datatype name)
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

    | Expr'If Span Expr Expr Expr
    | Expr'Case Span Expr [(Pattern, Expr)]

    | Expr'TypeAnnotation Span Type Expr
    deriving (Eq, Show)

data Pattern
    = Pattern'Identifier Identifier -- TODO: decide whether '_' should be this or a separate variant
    | Pattern'Tuple Span [Pattern]
    | Pattern'Named Span Identifier Pattern
    deriving (Eq, Show)
