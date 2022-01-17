module UHF.AST.Expr where

import qualified Data.Decimal as Decimal

data Expr
    = Identifier [String]
    | CharLit Char
    | StringLit String
    | IntLit Integer
    | FloatLit Decimal.Decimal
    | BoolLit Bool
