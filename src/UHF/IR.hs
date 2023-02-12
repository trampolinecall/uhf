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

    , expr_type
    , pattern_type
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
data BoundName typeinfo = BoundName typeinfo deriving Show

newtype BindingKey = BindingKey Int deriving Show
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i
data Binding identifier typeannotation typeinfo = Binding (Pattern identifier typeinfo) (Expr identifier typeannotation typeinfo) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundNameKey) (Maybe NameContext) deriving Show

data TypeExpr identifier
    = TypeExpr'Identifier identifier
    | TypeExpr'Tuple [TypeExpr identifier]
    deriving Show

data Type var
    = Type'Nominal NominalTypeKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function (Type var) (Type var)
    | Type'Tuple (Type var) (Type var)
    | Type'Variable var
    deriving Show

data Expr identifier typeannotation typeinfo
    = Expr'Identifier typeinfo identifier
    | Expr'Char typeinfo Char
    | Expr'String typeinfo Text
    | Expr'Int typeinfo Integer
    | Expr'Float typeinfo Rational
    | Expr'Bool typeinfo Bool -- TODO: replace with identifier exprs

    | Expr'Tuple typeinfo (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'Lambda typeinfo (Pattern identifier typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'Let typeinfo (Expr identifier typeannotation typeinfo)
    | Expr'LetRec typeinfo (Expr identifier typeannotation typeinfo)

    | Expr'BinaryOps typeinfo (Expr identifier typeannotation typeinfo) [(identifier, Expr identifier typeannotation typeinfo)]

    | Expr'Call typeinfo (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'If typeinfo (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)
    | Expr'Case typeinfo (Expr identifier typeannotation typeinfo) [(Pattern identifier typeinfo, Expr identifier typeannotation typeinfo)]

    | Expr'Poison typeinfo

    | Expr'TypeAnnotation typeinfo typeannotation (Expr identifier typeannotation typeinfo)
    deriving Show

data Pattern identifier typeinfo
    = Pattern'Identifier typeinfo BoundNameKey
    | Pattern'Tuple typeinfo (Pattern identifier typeinfo) (Pattern identifier typeinfo)
    | Pattern'Named typeinfo BoundNameKey (Pattern identifier typeinfo)

    | Pattern'Poison typeinfo -- TODO: poisonallowed
    deriving Show

expr_type :: Expr identifier typeannotation typeinfo -> typeinfo
expr_type (Expr'Identifier typeinfo _) = typeinfo
expr_type (Expr'Char typeinfo _) = typeinfo
expr_type (Expr'String typeinfo _) = typeinfo
expr_type (Expr'Int typeinfo _) = typeinfo
expr_type (Expr'Float typeinfo _) = typeinfo
expr_type (Expr'Bool typeinfo _) = typeinfo
expr_type (Expr'Tuple typeinfo _ _) = typeinfo
expr_type (Expr'Lambda typeinfo _ _) = typeinfo
expr_type (Expr'Let typeinfo _) = typeinfo
expr_type (Expr'LetRec typeinfo _) = typeinfo
expr_type (Expr'BinaryOps typeinfo _ _) = typeinfo
expr_type (Expr'Call typeinfo _ _) = typeinfo
expr_type (Expr'If typeinfo _ _ _) = typeinfo
expr_type (Expr'Case typeinfo _ _) = typeinfo
expr_type (Expr'Poison typeinfo) = typeinfo
expr_type (Expr'TypeAnnotation typeinfo _ _) = typeinfo

pattern_type :: Pattern typeannotation typeinfo -> typeinfo
pattern_type (Pattern'Identifier typeinfo _) = typeinfo
pattern_type (Pattern'Tuple typeinfo _ _) = typeinfo
pattern_type (Pattern'Named typeinfo _ _) = typeinfo
pattern_type (Pattern'Poison typeinfo) = typeinfo
