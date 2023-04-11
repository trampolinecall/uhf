module UHF.Data.IR.ANFIR
    ( ANFIR (..)
    , Decl (..)
    , DeclKey
    , BindingKey
    , ParamKey
    , Binding (..)
    , Param (..)
    , ID (..)
    , mangle_id
    , stringify_id
    , Expr (..)
    , SwitchMatcher (..)
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
data ANFIR ty poison_allowed = ANFIR (Arena.Arena Decl DeclKey) (Arena.Arena (Type.ADT ty) ADTKey) (Arena.Arena (Type.TypeSynonym ty) TypeSynonymKey) (Arena.Arena Type.Var Type.TypeVarKey) (Arena.Arena (Binding ty poison_allowed) BindingKey) (Arena.Arena (Param ty) ParamKey) DeclKey

data Decl
    = Decl'Module [BindingKey] [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data Param ty = Param ID.BoundValueID ty deriving Show

newtype Binding ty poison_allowed = Binding (Expr ty poison_allowed)

data ID
    = ExprID ID.ExprID
    | BVID ID.BoundValueID
    deriving Show
mangle_id :: ID -> Text
mangle_id (ExprID id) = ID.mangle id
mangle_id (BVID id) = ID.mangle id
stringify_id :: ID -> Text
stringify_id (ExprID id) = ID.stringify id
stringify_id (BVID id) = ID.stringify id

data Expr ty poison_allowed
    = Expr'Identifier ID ty BindingKey

    | Expr'Int ID ty Integer
    | Expr'Float ID ty Rational
    | Expr'Bool ID ty Bool
    | Expr'Char ID ty Char
    | Expr'String ID ty Text
    | Expr'Tuple ID ty BindingKey BindingKey -- TODO: replace with call constructor expr

    | Expr'Lambda ID ty (Set BindingKey) ParamKey [BindingKey] BindingKey -- first collection of binding keys is captures, second is the body
    | Expr'Param ID ty ParamKey

    | Expr'Call ID ty BindingKey BindingKey

    | Expr'Switch ID ty BindingKey [(SwitchMatcher, BindingKey)]

    | Expr'Seq ID ty BindingKey BindingKey

    | Expr'TupleDestructure1 ID ty BindingKey -- TODO: figure out better solution to this (probably general destructure expr for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ID ty BindingKey

    | Expr'Forall ID ty (NonEmpty TypeVarKey) BindingKey -- TODO: put child bindings
    | Expr'TypeApply ID ty BindingKey ty

    | Expr'MakeADT ID ty Type.ADTVariantIndex [BindingKey]

    | Expr'Poison ID ty poison_allowed
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
expr_type (Expr'MakeADT _ ty _ _) = ty
expr_type (Expr'Poison _ ty _) = ty

expr_id :: Expr ty poison_allowed -> ID
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
expr_id (Expr'MakeADT id _ _ _) = id
expr_id (Expr'Poison id _ _) = id

binding_type :: Binding ty poison_allowed -> ty
binding_type = expr_type . get_initializer
binding_id :: Binding ty poison_allowed -> ID
binding_id = expr_id . get_initializer
