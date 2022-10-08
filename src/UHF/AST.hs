module UHF.AST where

data Decl
    = Decl'TypeSignature [String] Type
    | Decl'Binding [String] Expr -- TODO: this should eventually be a pattern
    deriving (Eq, Show)

data Type
    = Type'Identifier [String]
    deriving (Eq, Show)

data Expr
    = Expr'Identifier [String]
    | Expr'CharLit Char
    | Expr'StringLit String
    | Expr'IntLit Integer
    | Expr'FloatLit Rational
    | Expr'BoolLit Bool
    deriving (Eq, Show)
