module UHF.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

import UHF.IO.Location (Located)

data Decl
    = Decl'Binding (Located [Located Text]) Expr -- TODO: this should eventually be a pattern
    deriving (Eq, Show)

data Type
    = Type'Identifier (Located [Located Text])
    deriving (Eq, Show)

data Expr
    = Expr'Identifier (Located [Located Text])
    | Expr'CharLit Char
    | Expr'StringLit Text
    | Expr'IntLit Integer
    | Expr'FloatLit Rational
    | Expr'BoolLit Bool
    deriving (Eq, Show)
