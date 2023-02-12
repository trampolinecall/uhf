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
    , expr_span
    , pattern_span
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map

import UHF.IO.Location (Span, Located)

newtype DeclKey = DeclKey Int deriving Show
instance Arena.Key DeclKey where
    make_key = DeclKey
    unmake_key (DeclKey i) = i
data Decl
    = Decl'Module Module
    | Decl'Type NominalTypeKey
    deriving Show
data Module = Module NameContext deriving Show

newtype NominalTypeKey = NominalTypeKey Int deriving (Show, Eq)
instance Arena.Key NominalTypeKey where
    make_key = NominalTypeKey
    unmake_key (NominalTypeKey i) = i
data NominalType ty
    = NominalType'Data Text [DataVariant ty]
    | NominalType'Synonym Text ty
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
    = Expr'Identifier typeinfo Span identifier
    | Expr'Char typeinfo Span Char
    | Expr'String typeinfo Span Text
    | Expr'Int typeinfo Span Integer
    | Expr'Float typeinfo Span Rational
    | Expr'Bool typeinfo Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple typeinfo Span (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'Lambda typeinfo Span (Pattern identifier typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'Let typeinfo Span (Expr identifier typeannotation typeinfo)
    | Expr'LetRec typeinfo Span (Expr identifier typeannotation typeinfo)

    | Expr'BinaryOps typeinfo Span (Expr identifier typeannotation typeinfo) [(identifier, Expr identifier typeannotation typeinfo)]

    | Expr'Call typeinfo Span (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)

    | Expr'If typeinfo Span (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo) (Expr identifier typeannotation typeinfo)
    | Expr'Case typeinfo Span (Expr identifier typeannotation typeinfo) [(Pattern identifier typeinfo, Expr identifier typeannotation typeinfo)]

    | Expr'Poison typeinfo Span

    | Expr'TypeAnnotation typeinfo Span typeannotation (Expr identifier typeannotation typeinfo)
    deriving Show

data Pattern identifier typeinfo
    = Pattern'Identifier typeinfo Span BoundNameKey
    | Pattern'Tuple typeinfo Span (Pattern identifier typeinfo) (Pattern identifier typeinfo)
    | Pattern'Named typeinfo Span (Located BoundNameKey) (Pattern identifier typeinfo)

    | Pattern'Poison typeinfo Span -- TODO: poisonallowed
    deriving Show

expr_type :: Expr identifier typeannotation typeinfo -> typeinfo
expr_type (Expr'Identifier typeinfo _ _) = typeinfo
expr_type (Expr'Char typeinfo _ _) = typeinfo
expr_type (Expr'String typeinfo _ _) = typeinfo
expr_type (Expr'Int typeinfo _ _) = typeinfo
expr_type (Expr'Float typeinfo _ _) = typeinfo
expr_type (Expr'Bool typeinfo _ _) = typeinfo
expr_type (Expr'Tuple typeinfo _ _ _) = typeinfo
expr_type (Expr'Lambda typeinfo _ _ _) = typeinfo
expr_type (Expr'Let typeinfo _ _) = typeinfo
expr_type (Expr'LetRec typeinfo _ _) = typeinfo
expr_type (Expr'BinaryOps typeinfo _ _ _) = typeinfo
expr_type (Expr'Call typeinfo _ _ _) = typeinfo
expr_type (Expr'If typeinfo _ _ _ _) = typeinfo
expr_type (Expr'Case typeinfo _ _ _) = typeinfo
expr_type (Expr'Poison typeinfo _) = typeinfo
expr_type (Expr'TypeAnnotation typeinfo _ _ _) = typeinfo

expr_span :: Expr identifier typeannotation typeinfo -> Span
expr_span (Expr'Identifier _ sp _) = sp
expr_span (Expr'Char _ sp _) = sp
expr_span (Expr'String _ sp _) = sp
expr_span (Expr'Int _ sp _) = sp
expr_span (Expr'Float _ sp _) = sp
expr_span (Expr'Bool _ sp _) = sp
expr_span (Expr'Tuple _ sp _ _) = sp
expr_span (Expr'Lambda _ sp _ _) = sp
expr_span (Expr'Let _ sp _) = sp
expr_span (Expr'LetRec _ sp _) = sp
expr_span (Expr'BinaryOps _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'If _ sp _ _ _) = sp
expr_span (Expr'Case _ sp _ _) = sp
expr_span (Expr'Poison _ sp) = sp
expr_span (Expr'TypeAnnotation _ sp _ _) = sp

pattern_type :: Pattern typeannotation typeinfo -> typeinfo
pattern_type (Pattern'Identifier typeinfo _ _) = typeinfo
pattern_type (Pattern'Tuple typeinfo _ _ _) = typeinfo
pattern_type (Pattern'Named typeinfo _ _ _) = typeinfo
pattern_type (Pattern'Poison typeinfo _) = typeinfo

pattern_span :: Pattern spanannotation spaninfo -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp
