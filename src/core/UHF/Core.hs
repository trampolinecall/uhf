module UHF.Core where

data Core
    = Core [(String, Expr)]

data Expr
    = Var String
    | Abstract (String, Type) Expr
    | Apply Expr Expr
    | TypeAbstract String Expr
    | TypeApply Expr Type

data Type
    = BaseType
    | TypeVar String
    | Function Type Type
    | UniversalType String Type
