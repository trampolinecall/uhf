module UHF.Data.IR.ANFIR
    ( ANFIR (..)
    , Decl (..)

    , DeclKey
    , BindingKey
    , ParamKey

    , BoundWhere (..)
    , BindingGroup (..)
    , Binding (..)

    , Param (..)

    , ID (..)
    , mangle_id
    , stringify_id

    , Expr (..)
    , SwitchMatcher (..)
    , expr_type
    , expr_id
    , binding_type
    , binding_id
    ) where

import UHF.Util.Prelude

import qualified Arena
import qualified Unique

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

-- "a-normal form ir"
data ANFIR captures ty poison_allowed = ANFIR (Arena.Arena (Decl captures) DeclKey) (Arena.Arena (Type.ADT ty) ADTKey) (Arena.Arena (Type.TypeSynonym ty) TypeSynonymKey) (Arena.Arena Type.Var Type.TypeVarKey) (Arena.Arena (Binding captures ty poison_allowed) BindingKey) (Arena.Arena (Param ty) ParamKey) DeclKey

data Decl captures
    = Decl'Module (BindingGroup captures) [ADTKey] [TypeSynonymKey]
    | Decl'Type (Type.Type Void)
    deriving Show

data Param ty = Param ID.BoundValueID ty deriving Show

newtype BoundWhere = BoundWhere Unique.Unique
data Binding captures ty poison_allowed = Binding { binding_bound_where :: BoundWhere, binding_initializer :: Expr captures ty poison_allowed }

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

-- TODO: parameterize by type of captures
-- TODO: make BindingGroupNum = Globals | Local Unique.Unique
data BindingGroup captures = BindingGroup { binding_group_unique :: Unique.Unique, binding_group_captures :: captures, binding_group_bindings :: [BindingKey] } deriving Show

data Expr captures ty poison_allowed
    = Expr'Identifier ID ty BindingKey

    | Expr'Int ID ty Integer
    | Expr'Float ID ty Rational
    | Expr'Bool ID ty Bool
    | Expr'Char ID ty Char
    | Expr'String ID ty Text
    | Expr'Tuple ID ty BindingKey BindingKey -- TODO: replace with call constructor expr
    | Expr'MakeADT ID ty Type.ADTVariantIndex [BindingKey]

    | Expr'Lambda ID ty ParamKey (BindingGroup captures) BindingKey
    | Expr'Param ID ty ParamKey

    | Expr'Call ID ty BindingKey BindingKey

    | Expr'Switch ID ty BindingKey [(SwitchMatcher, BindingGroup captures, BindingKey)]

    | Expr'Seq ID ty BindingKey BindingKey

    | Expr'TupleDestructure1 ID ty BindingKey -- TODO: figure out better solution to this (probably general destructure expr for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ID ty BindingKey

    | Expr'Forall ID ty (NonEmpty TypeVarKey) (BindingGroup captures) BindingKey
    | Expr'TypeApply ID ty BindingKey ty

    | Expr'Poison ID ty poison_allowed
    deriving Show

data SwitchMatcher
    = Switch'BoolLiteral Bool
    | Switch'Tuple
    | Switch'Default
    deriving Show

expr_type :: Expr captures ty poison_allowed -> ty
expr_type (Expr'Identifier _ ty _) = ty
expr_type (Expr'Int _ ty _) = ty
expr_type (Expr'Float _ ty _) = ty
expr_type (Expr'Bool _ ty _) = ty
expr_type (Expr'Char _ ty _) = ty
expr_type (Expr'String _ ty _) = ty
expr_type (Expr'Tuple _ ty _ _) = ty
expr_type (Expr'Lambda _ ty _ _ _) = ty
expr_type (Expr'Param _ ty _) = ty
expr_type (Expr'Call _ ty _ _) = ty
expr_type (Expr'Switch _ ty _ _) = ty
expr_type (Expr'Seq _ ty _ _) = ty
expr_type (Expr'TupleDestructure1 _ ty _) = ty
expr_type (Expr'TupleDestructure2 _ ty _) = ty
expr_type (Expr'Forall _ ty _ _ _) = ty
expr_type (Expr'TypeApply _ ty _ _) = ty
expr_type (Expr'MakeADT _ ty _ _) = ty
expr_type (Expr'Poison _ ty _) = ty

expr_id :: Expr captures ty poison_allowed -> ID
expr_id (Expr'Identifier id _ _) = id
expr_id (Expr'Int id _ _) = id
expr_id (Expr'Float id _ _) = id
expr_id (Expr'Bool id _ _) = id
expr_id (Expr'Char id _ _) = id
expr_id (Expr'String id _ _) = id
expr_id (Expr'Tuple id _ _ _) = id
expr_id (Expr'Lambda id _ _ _ _) = id
expr_id (Expr'Param id _ _) = id
expr_id (Expr'Call id _ _ _) = id
expr_id (Expr'Switch id _ _ _) = id
expr_id (Expr'Seq id _ _ _) = id
expr_id (Expr'TupleDestructure1 id _ _) = id
expr_id (Expr'TupleDestructure2 id _ _) = id
expr_id (Expr'Forall id _ _ _ _) = id
expr_id (Expr'TypeApply id _ _ _) = id
expr_id (Expr'MakeADT id _ _ _) = id
expr_id (Expr'Poison id _ _) = id

binding_type :: Binding captures ty poison_allowed -> ty
binding_type = expr_type . binding_initializer
binding_id :: Binding captures ty poison_allowed -> ID
binding_id = expr_id . binding_initializer
