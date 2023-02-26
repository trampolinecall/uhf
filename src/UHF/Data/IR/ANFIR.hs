module UHF.Data.IR.ANFIR
    ( Decl(..)
    , NodeKey
    , ParamKey
    , Param(..)
    , Node(..)
    , node_type
    ) where

import UHF.Util.Prelude

import qualified UHF.Data.IR.HIR as HIR -- TODO: figure out where to put HIR.Type because this uses the same type system
import qualified Arena

-- TODO: figure out where to put HIR.DeclKey because this also uses that
data Decl
    = Decl'Module [NodeKey] -- TODO: should this include nominal types too?
    | Decl'Type (HIR.Type Void)
    deriving Show

newtype NodeKey = NodeKey Int deriving (Show, Eq, Ord) -- TODO: figure out better solution in ts backend than to use ord instance
instance Arena.Key NodeKey where
    make_key = NodeKey
    unmake_key (NodeKey i) = i
newtype ParamKey = ParamKey Int deriving (Show, Eq, Ord) -- TODO: figure out better solution in ts backend than to use ord instance for ordering parameters
instance Arena.Key ParamKey where
    make_key = ParamKey
    unmake_key (ParamKey i) = i

newtype Param ty = Param ty deriving Show

-- every node is implicitly its own variable declaration
-- because every nodes argument is another node, all arguments to expressions are variables
data Node ty poison_allowed
    = Node'Int ty Integer
    | Node'Float ty Rational
    | Node'Bool ty Bool
    | Node'Char ty Char
    | Node'String ty Text
    | Node'Tuple ty NodeKey NodeKey -- TODO: replace with call constructor node

    | Node'Lambda ty ParamKey [NodeKey] NodeKey
    | Node'Param ty ParamKey

    | Node'Call ty NodeKey NodeKey

    | Node'TupleDestructure1 ty NodeKey -- TODO: figure out better solution to this (probably general destructure node for any type, or actually probably use case expressions to match on things)
    | Node'TupleDestructure2 ty NodeKey

    | Node'Poison ty poison_allowed
    deriving Show

node_type :: Node ty poison_allowed -> ty
node_type (Node'Int ty _) = ty
node_type (Node'Float ty _) = ty
node_type (Node'Bool ty _) = ty
node_type (Node'Char ty _) = ty
node_type (Node'String ty _) = ty
node_type (Node'Tuple ty _ _) = ty

node_type (Node'Lambda ty _ _ _) = ty
node_type (Node'Param ty _) = ty

node_type (Node'Call ty _ _) = ty

node_type (Node'TupleDestructure1 ty _) = ty
node_type (Node'TupleDestructure2 ty _) = ty

node_type (Node'Poison ty _) = ty
