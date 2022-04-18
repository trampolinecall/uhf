module UHF.AST.Type where

data Type
    = Identifier [String]
    deriving (Eq, Show)
