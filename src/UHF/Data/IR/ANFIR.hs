module UHF.Data.IR.ANFIR
    ( ANFIR (..)
    , Decl (..)
    , BindingKey
    , ParamKey
    , Binding(..)
    , Param(..)
    , Expr(..)
    , SwitchMatcher(..)
    , get_initializer
    , expr_type
    , binding_type
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type

data ANFIR ty poison_allowed = ANFIR (Arena.Arena Decl DeclKey) (Arena.Arena (Type.ADT ty) ADTKey) (Arena.Arena (Type.TypeSynonym ty) TypeSynonymKey) (Arena.Arena (Binding ty poison_allowed) BindingKey) (Arena.Arena (Param ty) ParamKey)

data Decl
    = Decl'Module [BindingKey] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

newtype Param ty = Param ty deriving Show

data Binding ty poison_allowed = Binding (Expr ty poison_allowed)

data Expr ty poison_allowed
    = Expr'Identifier ty BindingKey

    | Expr'Int ty Integer
    | Expr'Float ty Rational
    | Expr'Bool ty Bool
    | Expr'Char ty Char
    | Expr'String ty Text
    | Expr'Tuple ty BindingKey BindingKey -- TODO: replace with call constructor expr

    | Expr'Lambda ty (Set BindingKey) ParamKey [BindingKey] BindingKey -- first collection of binding keys is captures, second is the body
    | Expr'Param ty ParamKey

    | Expr'Call ty BindingKey BindingKey

    | Expr'Switch ty BindingKey [(SwitchMatcher, BindingKey)]

    | Expr'TupleDestructure1 ty BindingKey -- TODO: figure out better solution to this (probably general destructure expr for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ty BindingKey

    | Expr'Poison ty poison_allowed
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple
    | Switch'Default
    deriving Show

get_initializer :: Binding ty poison_allowed -> Expr ty poison_allowed
get_initializer (Binding e) = e

expr_type :: Expr ty poison_allowed -> ty
expr_type (Expr'Identifier ty _) = ty
expr_type (Expr'Int ty _) = ty
expr_type (Expr'Float ty _) = ty
expr_type (Expr'Bool ty _) = ty
expr_type (Expr'Char ty _) = ty
expr_type (Expr'String ty _) = ty
expr_type (Expr'Tuple ty _ _) = ty

expr_type (Expr'Lambda ty _ _ _ _) = ty
expr_type (Expr'Param ty _) = ty

expr_type (Expr'Call ty _ _) = ty

expr_type (Expr'Switch ty _ _) = ty

expr_type (Expr'TupleDestructure1 ty _) = ty
expr_type (Expr'TupleDestructure2 ty _) = ty

expr_type (Expr'Poison ty _) = ty

binding_type :: Binding ty poison_allowed -> ty
binding_type = expr_type . get_initializer
