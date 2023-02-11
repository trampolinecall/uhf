module UHF.IR
    ( DeclKey
    , Decl(..)
    , Module(..)

    , NominalTypeKey
    , NominalType(..)
    , DataVariant(..)

    , BoundNameKey
    , BoundName(..)

    , BindingKey
    , Binding (..)

    , NameContext (..)

    , TypeExpr(..)
    , Type(..)
    , Expr(..)
    , Pattern(..)
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map

newtype DeclKey = DeclKey Int deriving Show
instance Arena.Key DeclKey where
    make_key = DeclKey
    unmake_key (DeclKey i) = i
data Decl
    = Decl'Module Module
    | Decl'Type NominalTypeKey
    deriving Show
data Module = Module NameContext deriving Show

newtype NominalTypeKey = NominalTypeKey Int deriving Show
instance Arena.Key NominalTypeKey where
    make_key = NominalTypeKey
    unmake_key (NominalTypeKey i) = i
data NominalType ty
    = NominalType'Data [DataVariant ty]
    | NominalType'Synonym ty
    deriving Show
data DataVariant ty
    = DataVariant'Named Text [(Text, ty)]
    | DataVariant'Anon Text [ty]
    deriving Show

newtype BoundNameKey = BoundNameKey Int deriving Show
instance Arena.Key BoundNameKey where
    make_key = BoundNameKey
    unmake_key (BoundNameKey i) = i
newtype BoundName = BoundName () deriving Show -- TODO: replace with counter instead of arena of ()?, also rename to something better because decls are also "bound names", probably change to "bound value"?

newtype BindingKey = BindingKey Int deriving Show
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i
data Binding identifier = Binding (Pattern identifier) (Expr identifier) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundNameKey) (Maybe NameContext) deriving Show

data TypeExpr identifier
    = TypeExpr'Identifier identifier
    | TypeExpr'Tuple [TypeExpr identifier]
    deriving Show

data Type
    = Type'Nominal NominalTypeKey
    | Type'Tuple [Type]
    deriving Show

data Expr identifier
    = Expr'Identifier identifier
    | Expr'Char Char
    | Expr'String Text
    | Expr'Int Integer
    | Expr'Float Rational
    | Expr'Bool Bool -- TODO: replace with identifier exprs

    | Expr'Tuple (Expr identifier) (Expr identifier)

    | Expr'Lambda NameContext (Pattern identifier) (Expr identifier) -- TODO: remove name contexts from other places because it is only needed in identifier resolution

    | Expr'Let NameContext (Expr identifier)
    | Expr'LetRec NameContext (Expr identifier)

    | Expr'BinaryOps (Expr identifier) [(identifier, Expr identifier)]

    | Expr'Call (Expr identifier) [Expr identifier]

    | Expr'If (Expr identifier) (Expr identifier) (Expr identifier)
    | Expr'Case (Expr identifier) [(NameContext, Pattern identifier, Expr identifier)]

    | Expr'Poison

    -- TODO: | Expr'TypeAnnotation TypeExpr (Expr identifier)
    deriving Show

data Pattern identifier
    = Pattern'Identifier BoundNameKey
    | Pattern'Tuple (Pattern identifier) (Pattern identifier)
    | Pattern'Named BoundNameKey (Pattern identifier)

    | Pattern'Poison -- TODO: poisonallowed
    deriving Show
