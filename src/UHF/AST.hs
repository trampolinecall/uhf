module UHF.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

import UHF.IO.Location (Located, Span)

type Identifier = Located [Located Text]

-- TODO: asts store spans

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
    | Expr'Char Char
    | Expr'String Text
    | Expr'Int Integer
    | Expr'Float Rational
    | Expr'Bool Bool -- TODO: replace with identifier exprs

    | Expr'Tuple Span [Expr]

    | Expr'Lambda [Pattern] Expr

    | Expr'Let [Decl] Expr
    | Expr'LetRec [Decl] Expr

    | Expr'BinaryOps Expr [(Identifier, Expr)]

    | Expr'Call Expr [Expr]

    | Expr'If Expr Expr Expr
    | Expr'Case Expr [(Pattern, Expr)]

    | Expr'TypeAnnotation Type Expr
    deriving (Eq, Show)

data Pattern
    = Pattern'Identifier Identifier -- TODO: decide whether '_' should be this or a separate variant
    | Pattern'Tuple Span [Pattern]
    | Pattern'Named Identifier Pattern
    deriving (Eq, Show)
