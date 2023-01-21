module UHF.Core where

import qualified UHF.Core.Name as Name

data Core
    = Core [(Name.Name, Expr)]

data Expr
    = Var Name.Name
    | Abstract (Name.Name, Type) Expr
    | Apply Expr Expr
    | TypeAbstract Name.Name Expr
    | TypeApply Expr Type

data Type
    = BaseType
    | TypeVar Name.Name
    | Function Type Type
    | UniversalType Name.Name Type
