module UHF.IR
    ( DeclKey
    , Decl(..)
    , Module(..)

    , NominalTypeKey
    , NominalType(..)
    , DataVariant(..)

    , BoundValueKey
    , BoundValue(..)

    , BindingKey
    , Binding (..)

    , NameContext (..)

    , TypeExpr(..)
    , Type(..)
    , Expr(..)
    , Pattern(..)

    , GraphNodeKey
    , GraphNode(..)

    , expr_type
    , pattern_type
    , expr_span
    , pattern_span
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Map as Map

import UHF.IO.Location (Span, Located)

-- TODO: split this into separate modules

newtype DeclKey = DeclKey Int deriving Show
instance Arena.Key DeclKey where
    make_key = DeclKey
    unmake_key (DeclKey i) = i
data Decl
    = Decl'Module Module
    | Decl'Type (Type Void)
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

newtype BoundValueKey = BoundValueKey Int deriving (Show, Eq, Ord) -- TODO: remove Eq and Ord when BoundValues store their graph nodes
instance Arena.Key BoundValueKey where
    make_key = BoundValueKey
    unmake_key (BoundValueKey i) = i
data BoundValue typeinfo = BoundValue typeinfo Span deriving Show

newtype BindingKey = BindingKey Int deriving Show
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i
data Binding identifier typeannotation typeinfo binaryopsallowed = Binding (Pattern identifier typeinfo) Span (Expr identifier typeannotation typeinfo binaryopsallowed) deriving Show

data NameContext = NameContext (Map.Map Text DeclKey) (Map.Map Text BoundValueKey) (Maybe NameContext) deriving Show

data TypeExpr identifier
    = TypeExpr'Identifier Span identifier
    | TypeExpr'Tuple (TypeExpr identifier) (TypeExpr identifier)
    | TypeExpr'Poison Span
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

data Expr identifier typeannotation typeinfo binaryopsallowed
    = Expr'Identifier typeinfo Span identifier
    | Expr'Char typeinfo Span Char
    | Expr'String typeinfo Span Text
    | Expr'Int typeinfo Span Integer
    | Expr'Float typeinfo Span Rational
    | Expr'Bool typeinfo Span Bool -- TODO: replace with identifier exprs

    | Expr'Tuple typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Lambda typeinfo Span (Pattern identifier typeinfo) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Let typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed)
    | Expr'LetRec typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'BinaryOps binaryopsallowed typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) [(identifier, Expr identifier typeannotation typeinfo binaryopsallowed)]

    | Expr'Call typeinfo Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'If typeinfo Span Span (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed) (Expr identifier typeannotation typeinfo binaryopsallowed)
    | Expr'Case typeinfo Span Span (Expr identifier typeannotation typeinfo binaryopsallowed) [(Pattern identifier typeinfo, Expr identifier typeannotation typeinfo binaryopsallowed)]

    | Expr'TypeAnnotation typeinfo Span typeannotation (Expr identifier typeannotation typeinfo binaryopsallowed)

    | Expr'Poison typeinfo Span
    deriving Show

data Pattern identifier typeinfo
    = Pattern'Identifier typeinfo Span BoundValueKey
    | Pattern'Tuple typeinfo Span (Pattern identifier typeinfo) (Pattern identifier typeinfo)
    | Pattern'Named typeinfo Span Span (Located BoundValueKey) (Pattern identifier typeinfo)

    | Pattern'Poison typeinfo Span -- TODO: poisonallowed
    deriving Show

expr_type :: Expr identifier typeannotation typeinfo binaryopsallowed -> typeinfo
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
expr_type (Expr'BinaryOps _ typeinfo _ _ _) = typeinfo
expr_type (Expr'Call typeinfo _ _ _) = typeinfo
expr_type (Expr'If typeinfo _ _ _ _ _) = typeinfo
expr_type (Expr'Case typeinfo _ _ _ _) = typeinfo
expr_type (Expr'Poison typeinfo _) = typeinfo
expr_type (Expr'TypeAnnotation typeinfo _ _ _) = typeinfo

expr_span :: Expr identifier typeannotation typeinfo binaryopsallowed -> Span
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
expr_span (Expr'BinaryOps _ _ sp _ _) = sp
expr_span (Expr'Call _ sp _ _) = sp
expr_span (Expr'If _ sp _ _ _ _) = sp
expr_span (Expr'Case _ sp _ _ _) = sp
expr_span (Expr'Poison _ sp) = sp
expr_span (Expr'TypeAnnotation _ sp _ _) = sp

pattern_type :: Pattern typeannotation typeinfo -> typeinfo
pattern_type (Pattern'Identifier typeinfo _ _) = typeinfo
pattern_type (Pattern'Tuple typeinfo _ _ _) = typeinfo
pattern_type (Pattern'Named typeinfo _ _ _ _) = typeinfo
pattern_type (Pattern'Poison typeinfo _) = typeinfo

pattern_span :: Pattern spanannotation spaninfo -> Span
pattern_span (Pattern'Identifier _ sp _) = sp
pattern_span (Pattern'Tuple _ sp _ _) = sp
pattern_span (Pattern'Named _ sp _ _ _) = sp
pattern_span (Pattern'Poison _ sp) = sp

newtype GraphNodeKey = GraphNodeKey Int deriving Show
instance Arena.Key GraphNodeKey where
    make_key = GraphNodeKey
    unmake_key (GraphNodeKey i) = i
data GraphNode -- TODO: is type information necessary at this stage? (graph is created after typechecking)
    = GraphNode'Int (Maybe (Type Void)) Integer
    | GraphNode'Float (Maybe (Type Void)) Rational
    | GraphNode'Bool (Maybe (Type Void)) Bool
    | GraphNode'Char (Maybe (Type Void)) Char
    | GraphNode'String (Maybe (Type Void)) Text
    | GraphNode'Tuple (Maybe (Type Void)) GraphNodeKey GraphNodeKey -- TODO: replace with call constructor node

    | GraphNode'Lambda (Maybe (Type Void)) GraphNodeKey GraphNodeKey -- TODO: this probably doesnt work well
    | GraphNode'Param (Maybe (Type Void))

    | GraphNode'Call (Maybe (Type Void)) GraphNodeKey GraphNodeKey

    | GraphNode'TupleDestructure1 (Maybe (Type Void)) GraphNodeKey -- TODO: figure out better solution to this
    | GraphNode'TupleDestructure2 (Maybe (Type Void)) GraphNodeKey

    | GraphNode'Poison (Maybe (Type Void))
    deriving Show
