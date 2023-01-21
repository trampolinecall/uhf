module UHF.IR.Expr where

import UHF.Util.Prelude

data Expr identifier
    = Identifier identifier
    | CharLit Char
    | StringLit Text
    | IntLit Integer
    | FloatLit Rational
    | BoolLit Bool
    deriving (Eq, Show)
