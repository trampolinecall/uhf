module UHF.Phases.Back.TSBackend.TS (Stmt (..), Expr (..), Type (..), TypeReference (..), ClassMember (..), Parameter (..), Accessibility (..)) where

import UHF.Util.Prelude

-- the subset of typescript syntax that is actually used by the backend
-- TODO: reorganize and reorder module

data Stmt
    = Stmt'Function Text [Parameter] (Maybe Type) (Maybe [Stmt])
    | Stmt'Class Text [TypeReference] [ClassMember]
    | Stmt'Let Text (Maybe Type) (Maybe Expr)
    | Stmt'Return Expr
    | Stmt'Expr Expr
    | Stmt'Spacer

data Expr
    = Expr'Identifier Text
    | Expr'Int Integer
    | Expr'Bool Bool
    | Expr'Char Char
    | Expr'String Text
    | Expr'Undefined
    | Expr'StrLit Text
    | Expr'List [Expr]
    | Expr'Object [(Text, Maybe Expr)]
    | Expr'ArrowFunction [Parameter] (Maybe Type) (Either Expr [Stmt])
    | Expr'New Expr [Expr]
    | Expr'Call Expr [Expr]
    | Expr'Get Expr Text
    | Expr'Div Expr Expr
    | Expr'Assign Expr Expr

data Type
    = Type'Reference TypeReference
    | Type'Object [(Text, Maybe Type)]
    | Type'Never
    | Type'Union Type Type
    | Type'StrLit Text
data TypeReference = TypeReference Text [Type]

data ClassMember
    = ClassMember'Constructor [Parameter] (Maybe [Stmt])
    | ClassMember'PropDecl Text (Maybe Type) (Maybe Expr)
    | ClassMember'MethodDecl Text [Parameter] (Maybe Type) (Maybe [Stmt])

data Parameter = Parameter (Maybe Accessibility) Text (Maybe Type)
data Accessibility = Public
