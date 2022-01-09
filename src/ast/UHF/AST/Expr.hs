module UHF.AST.Expr where

data Expr
    = Identifier [String]
    | Application Expr Expr
