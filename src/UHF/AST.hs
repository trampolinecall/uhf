module UHF.AST where

import UHF.Util.Prelude

data Decl
    = Decl'TypeSignature [Text] Type
    | Decl'Binding [Text] Expr -- TODO: this should eventually be a pattern
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
