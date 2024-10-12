module UHF.Data.ANFIR
    ( ANFIR (..)
    , CU(..)

    , BindingKey
    , ParamKey

    , BindingChunk(..)
    , chunk_bindings
    , BindingGroup (..)
    , Binding (..)

    , Param (..)

    , ID (..)
    , mangle_id
    , stringify_id

    , Expr (..)
    , MatchTree (..)
    , MatchClause (..)
    , MatchMatcher (..)
    , expr_type
    , expr_id
    , binding_type
    , binding_id
    ) where

import UHF.Prelude

import qualified Data.Set as Set

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Intrinsics as Intrinsics
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Util.Arena as Arena

-- "a-normal form ir" even though this isnt actually a-normal form but it is the same idea
data ANFIR
    = ANFIR
        (Arena.Arena (Type.ADT (Maybe Type.Type)) ADTKey)
        (Arena.Arena (Type.TypeSynonym (Maybe Type.Type)) TypeSynonymKey)
        (Arena.Arena Type.QuantVar Type.QuantVarKey)
        (Arena.Arena Binding BindingKey)
        (Arena.Arena Param ParamKey)
        CU

-- "compilation unit"
data CU = CU { cu_main_function :: Maybe BindingKey, cu_bindings :: BindingGroup, cu_adts :: [ADTKey], cu_type_synonyms :: [TypeSynonymKey] }

data Param = Param ID.VariableID (Maybe Type.Type) deriving Show

data BindingChunk
    = SingleBinding BindingKey
    | MutuallyRecursiveBindings [BindingKey] deriving Show
data BindingGroup = BindingGroup { binding_group_chunks :: [BindingChunk] } deriving Show
data Binding = Binding { binding_initializer :: Expr } deriving Show

data ID
    = ExprID ID.ExprID
    | VarID ID.VariableID
    deriving Show
mangle_id :: ID -> Text
mangle_id (ExprID id) = ID.mangle id
mangle_id (VarID id) = ID.mangle id
stringify_id :: ID -> Text
stringify_id (ExprID id) = ID.stringify id
stringify_id (VarID id) = ID.stringify id

data Expr
    = Expr'Refer ID (Maybe Type.Type) BindingKey
    | Expr'Intrinsic ID (Maybe Type.Type) Intrinsics.IntrinsicBoundValue

    | Expr'Int ID (Maybe Type.Type) Integer
    | Expr'Float ID (Maybe Type.Type) Rational
    | Expr'Bool ID (Maybe Type.Type) Bool
    | Expr'Char ID (Maybe Type.Type) Char
    | Expr'String ID (Maybe Type.Type) Text
    | Expr'Tuple ID (Maybe Type.Type) BindingKey BindingKey
    | Expr'MakeADT ID (Maybe Type.Type) Type.ADT.VariantIndex [Maybe Type.Type] [BindingKey]

    | Expr'Lambda ID (Maybe Type.Type) ParamKey (Set.Set BindingKey) BindingGroup BindingKey -- TODO: dont use BindingKey Ord for order of captures
    | Expr'Param ID (Maybe Type.Type) ParamKey

    | Expr'Call ID (Maybe Type.Type) BindingKey BindingKey

    | Expr'Match ID (Maybe Type.Type) MatchTree

    | Expr'TupleDestructure1 ID (Maybe Type.Type) BindingKey
    | Expr'TupleDestructure2 ID (Maybe Type.Type) BindingKey
    | Expr'ADTDestructure ID (Maybe Type.Type) BindingKey (Maybe Type.ADT.FieldIndex)

    | Expr'Forall ID (Maybe Type.Type) QuantVarKey BindingGroup BindingKey
    | Expr'TypeApply ID (Maybe Type.Type) BindingKey (Maybe Type.Type)

    | Expr'Poison ID (Maybe Type.Type)
    deriving Show

data MatchTree
    = MatchTree [([MatchClause], Either MatchTree (BindingGroup, BindingKey))]
    deriving Show
data MatchClause
    = MatchClause'Match BindingKey MatchMatcher
    | MatchClause'Binding BindingKey
    deriving Show
data MatchMatcher
    = Match'BoolLiteral Bool
    | Match'Tuple
    | Match'AnonADTVariant (Maybe Type.ADT.VariantIndex)
    deriving Show

expr_type :: Expr -> Maybe Type.Type
expr_type (Expr'Refer _ ty _) = ty
expr_type (Expr'Intrinsic _ ty _) = ty
expr_type (Expr'Int _ ty _) = ty
expr_type (Expr'Float _ ty _) = ty
expr_type (Expr'Bool _ ty _) = ty
expr_type (Expr'Char _ ty _) = ty
expr_type (Expr'String _ ty _) = ty
expr_type (Expr'Tuple _ ty _ _) = ty
expr_type (Expr'Lambda _ ty _ _ _ _) = ty
expr_type (Expr'Param _ ty _) = ty
expr_type (Expr'Call _ ty _ _) = ty
expr_type (Expr'Match _ ty _) = ty
expr_type (Expr'TupleDestructure1 _ ty _) = ty
expr_type (Expr'TupleDestructure2 _ ty _) = ty
expr_type (Expr'ADTDestructure _ ty _ _) = ty
expr_type (Expr'Forall _ ty _ _ _) = ty
expr_type (Expr'TypeApply _ ty _ _) = ty
expr_type (Expr'MakeADT _ ty _ _ _) = ty
expr_type (Expr'Poison _ ty) = ty

expr_id :: Expr -> ID
expr_id (Expr'Refer id _ _) = id
expr_id (Expr'Intrinsic id _ _) = id
expr_id (Expr'Int id _ _) = id
expr_id (Expr'Float id _ _) = id
expr_id (Expr'Bool id _ _) = id
expr_id (Expr'Char id _ _) = id
expr_id (Expr'String id _ _) = id
expr_id (Expr'Tuple id _ _ _) = id
expr_id (Expr'Lambda id _ _ _ _ _) = id
expr_id (Expr'Param id _ _) = id
expr_id (Expr'Call id _ _ _) = id
expr_id (Expr'Match id _ _) = id
expr_id (Expr'TupleDestructure1 id _ _) = id
expr_id (Expr'TupleDestructure2 id _ _) = id
expr_id (Expr'ADTDestructure id _ _ _) = id
expr_id (Expr'Forall id _ _ _ _) = id
expr_id (Expr'TypeApply id _ _ _) = id
expr_id (Expr'MakeADT id _ _ _ _) = id
expr_id (Expr'Poison id _) = id

binding_type :: Binding -> Maybe Type.Type
binding_type = expr_type . binding_initializer
binding_id :: Binding -> ID
binding_id = expr_id . binding_initializer

chunk_bindings :: BindingChunk -> [BindingKey]
chunk_bindings (SingleBinding b) = [b]
chunk_bindings (MutuallyRecursiveBindings bs) = bs
