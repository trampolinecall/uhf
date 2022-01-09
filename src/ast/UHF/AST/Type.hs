module UHF.AST.Type where

data Type
    = Identifier [String]
    | Application Type Type
