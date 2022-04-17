module UHF.AST.Expr where

import qualified UHF.Token as Token

data Expr
    = Identifier [String]
    | CharLit Char
    | StringLit String
    | IntLit Integer
    | FloatLit Token.Decimal
    | BoolLit Bool
