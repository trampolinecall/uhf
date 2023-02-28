module UHF.Data.IR.ANFIR
    ( Decl(..)
    , NodeKey
    , ParamKey
    , Param(..)
    , Node(..)
    , SwitchMatcher(..)
    , node_type
    ) where

import UHF.Util.Prelude

import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

data Decl
    = Decl'Module [NodeKey] -- TODO: should this include nominal types too?
    | Decl'Type (Type.Type Void)
    deriving Show

newtype Param ty = Param ty deriving Show

-- TODO: make every node more like a binding?

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

    | Node'Switch ty NodeKey [(SwitchMatcher, NodeKey)]

    | Node'TupleDestructure1 ty NodeKey -- TODO: figure out better solution to this (probably general destructure node for any type, or actually probably use case expressions to match on things)
    | Node'TupleDestructure2 ty NodeKey

    | Node'Poison ty poison_allowed
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple
    | Switch'Default
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

node_type (Node'Switch ty _ _) = ty

node_type (Node'TupleDestructure1 ty _) = ty
node_type (Node'TupleDestructure2 ty _) = ty

node_type (Node'Poison ty _) = ty
