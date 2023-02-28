module UHF.Data.IR.ANFIR
    ( Decl(..)
    , BindingKey
    , ParamKey
    , Binding(..)
    , Param(..)
    , Expr(..)
    , SwitchMatcher(..)
    , node_type
    , get_initializer
    ) where

import UHF.Util.Prelude

import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

data Decl
    = Decl'Module [BindingKey] -- TODO: should this include nominal types too?
    | Decl'Type (Type.Type Void)
    deriving Show

newtype Param ty = Param ty deriving Show

data Binding ty poison_allowed = Binding ty (Expr ty poison_allowed)

data Expr ty poison_allowed
    = Expr'Identifier ty BindingKey

    | Expr'Int ty Integer
    | Expr'Float ty Rational
    | Expr'Bool ty Bool
    | Expr'Char ty Char
    | Expr'String ty Text
    | Expr'Tuple ty BindingKey BindingKey -- TODO: replace with call constructor node

    | Expr'Lambda ty ParamKey [BindingKey] BindingKey
    | Expr'Param ty ParamKey

    | Expr'Call ty BindingKey BindingKey

    | Expr'Switch ty BindingKey [(SwitchMatcher, BindingKey)]

    | Expr'TupleDestructure1 ty BindingKey -- TODO: figure out better solution to this (probably general destructure node for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ty BindingKey

    | Expr'Poison ty poison_allowed
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple
    | Switch'Default
    deriving Show

node_type :: Expr ty poison_allowed -> ty
node_type (Expr'Identifier ty _) = ty
node_type (Expr'Int ty _) = ty
node_type (Expr'Float ty _) = ty
node_type (Expr'Bool ty _) = ty
node_type (Expr'Char ty _) = ty
node_type (Expr'String ty _) = ty
node_type (Expr'Tuple ty _ _) = ty

node_type (Expr'Lambda ty _ _ _) = ty
node_type (Expr'Param ty _) = ty

node_type (Expr'Call ty _ _) = ty

node_type (Expr'Switch ty _ _) = ty

node_type (Expr'TupleDestructure1 ty _) = ty
node_type (Expr'TupleDestructure2 ty _) = ty

node_type (Expr'Poison ty _) = ty

get_initializer :: Binding ty poison_allowed -> Expr ty poison_allowed
get_initializer (Binding _ e) = e
