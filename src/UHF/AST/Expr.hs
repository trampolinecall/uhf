module UHF.AST.Expr where

data Expr
    = Identifier [String]
    | CharLit Char
    | StringLit String
    | IntLit Integer
    | FloatLit Rational
    | BoolLit Bool
    deriving (Eq, Show)
