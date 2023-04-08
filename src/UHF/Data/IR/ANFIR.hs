module UHF.Data.IR.ANFIR
    ( ANFIR (..)
    , Decl (..)
    , DeclKey
    , BindingKey
    , ParamKey
    , Binding(..)
    , Param(..)
    , Expr(..)
    , SwitchMatcher(..)
    , get_initializer
    , expr_type
    , expr_id
    , binding_type
    , binding_id
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

-- "a-normal form ir"
data ANFIR ty poison_allowed = ANFIR (Arena.Arena Decl DeclKey) (Arena.Arena (Type.ADT ty) ADTKey) (Arena.Arena (Type.TypeSynonym ty) TypeSynonymKey) (Arena.Arena (Binding ty poison_allowed) BindingKey) (Arena.Arena (Param ty) ParamKey) DeclKey

data Decl
    = Decl'Module [BindingKey] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data Param ty = Param ID.BoundValueID ty deriving Show

newtype Binding ty poison_allowed = Binding (Expr ty poison_allowed)

data Expr ty poison_allowed
    = Expr'Identifier ID.ExprID ty BindingKey

    | Expr'Int ID.ExprID ty Integer
    | Expr'Float ID.ExprID ty Rational
    | Expr'Bool ID.ExprID ty Bool
    | Expr'Char ID.ExprID ty Char
    | Expr'String ID.ExprID ty Text
    | Expr'Tuple ID.ExprID ty BindingKey BindingKey -- TODO: replace with call constructor expr

    | Expr'Lambda ID.ExprID ty (Set BindingKey) ParamKey [BindingKey] BindingKey -- first collection of binding keys is captures, second is the body
    | Expr'Param ID.ExprID ty ParamKey

    | Expr'Call ID.ExprID ty BindingKey BindingKey

    | Expr'Switch ID.ExprID ty BindingKey [(SwitchMatcher, BindingKey)]

    | Expr'Seq ID.ExprID ty BindingKey BindingKey

    | Expr'TupleDestructure1 ID.ExprID ty BindingKey -- TODO: figure out better solution to this (probably general destructure expr for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ID.ExprID ty BindingKey

    | Expr'Forall ID.ExprID ty [TypeVarKey] BindingKey -- TODO: put child bindings
    | Expr'TypeApply ID.ExprID ty BindingKey ty

    | Expr'Poison ID.ExprID ty poison_allowed
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple
    | Switch'Default
    deriving Show

get_initializer :: Binding ty poison_allowed -> Expr ty poison_allowed
get_initializer (Binding e) = e

expr_type :: Expr ty poison_allowed -> ty
expr_type (Expr'Identifier _ ty _) = ty
expr_type (Expr'Int _ ty _) = ty
expr_type (Expr'Float _ ty _) = ty
expr_type (Expr'Bool _ ty _) = ty
expr_type (Expr'Char _ ty _) = ty
expr_type (Expr'String _ ty _) = ty
expr_type (Expr'Tuple _ ty _ _) = ty

expr_type (Expr'Lambda _ ty _ _ _ _) = ty
expr_type (Expr'Param _ ty _) = ty

expr_type (Expr'Call _ ty _ _) = ty

expr_type (Expr'Switch _ ty _ _) = ty
expr_type (Expr'Seq _ ty _ _) = ty

expr_type (Expr'TupleDestructure1 _ ty _) = ty
expr_type (Expr'TupleDestructure2 _ ty _) = ty

expr_type (Expr'Forall _ ty _ _) = ty
expr_type (Expr'TypeApply _ ty _ _) = ty

expr_type (Expr'Poison _ ty _) = ty

expr_id :: Expr ty poison_allowed -> ID.ExprID
expr_id (Expr'Identifier id _ _) = id
expr_id (Expr'Int id _ _) = id
expr_id (Expr'Float id _ _) = id
expr_id (Expr'Bool id _ _) = id
expr_id (Expr'Char id _ _) = id
expr_id (Expr'String id _ _) = id
expr_id (Expr'Tuple id _ _ _) = id

expr_id (Expr'Lambda id _ _ _ _ _) = id
expr_id (Expr'Param id _ _) = id

expr_id (Expr'Call id _ _ _) = id

expr_id (Expr'Switch id _ _ _) = id
expr_id (Expr'Seq id _ _ _) = id

expr_id (Expr'TupleDestructure1 id _ _) = id
expr_id (Expr'TupleDestructure2 id _ _) = id

expr_id (Expr'Forall id _ _ _) = id
expr_id (Expr'TypeApply id _ _ _) = id

expr_id (Expr'Poison id _ _) = id

binding_type :: Binding ty poison_allowed -> ty
binding_type = expr_type . get_initializer
binding_id :: Binding ty poison_allowed -> ID.ExprID
binding_id = expr_id . get_initializer
