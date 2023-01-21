module UHF.AST where

-- TODO: rename this to literal ast and make it literally store tokens

import UHF.Util.Prelude

data Decl
    = Decl'Binding [Text] Expr -- TODO: this should eventually be a pattern
    deriving (Eq, Show)

data Type
    = Type'Identifier [Text]
    deriving (Eq, Show)

data Expr
    = Expr'Identifier [Text]
    | Expr'CharLit Char
    | Expr'StringLit Text
    | Expr'IntLit Integer
    | Expr'FloatLit Rational
    | Expr'BoolLit Bool
    deriving (Eq, Show)
