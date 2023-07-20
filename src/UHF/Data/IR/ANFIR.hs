module UHF.Data.IR.ANFIR
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
    , CaseTree (..)
    , CaseMatchingClause (..)
    , CaseMatcher (..)
    , expr_type
    , expr_id
    , binding_type
    , binding_id
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified Data.Set as Set

import UHF.Data.IR.Keys
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

-- "a-normal form ir" even though this isnt actually a-normal form but it is the same idea
data ANFIR
    = ANFIR
        (Arena.Arena (Type.ADT (Maybe (Type.Type Void))) ADTKey)
        (Arena.Arena (Type.TypeSynonym (Maybe (Type.Type Void))) TypeSynonymKey)
        (Arena.Arena Type.Var Type.TypeVarKey)
        (Arena.Arena Binding BindingKey)
        (Arena.Arena Param ParamKey)
        CU

-- "compilation unit"
data CU = CU (BindingGroup) [ADTKey] [TypeSynonymKey]

data Param = Param ID.BoundValueID (Maybe (Type.Type Void)) deriving Show

data BindingChunk
    = SingleBinding BindingKey
    | MutuallyRecursiveBindings [BindingKey] deriving Show
data BindingGroup = BindingGroup { binding_group_chunks :: [BindingChunk] } deriving Show
data Binding = Binding { binding_initializer :: Expr } deriving Show

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

data Expr
    = Expr'Refer ID (Maybe (Type.Type Void)) BindingKey

    | Expr'Int ID (Maybe (Type.Type Void)) Integer
    | Expr'Float ID (Maybe (Type.Type Void)) Rational
    | Expr'Bool ID (Maybe (Type.Type Void)) Bool
    | Expr'Char ID (Maybe (Type.Type Void)) Char
    | Expr'String ID (Maybe (Type.Type Void)) Text
    | Expr'Tuple ID (Maybe (Type.Type Void)) BindingKey BindingKey -- TODO: replace with call constructor expr
    | Expr'MakeADT ID (Maybe (Type.Type Void)) Type.ADTVariantIndex [Maybe (Type.Type Void)] [BindingKey]

    | Expr'Lambda ID (Maybe (Type.Type Void)) ParamKey (Set.Set BindingKey) BindingGroup BindingKey -- TODO: dont use BindingKey Ord for order of captures
    | Expr'Param ID (Maybe (Type.Type Void)) ParamKey

    | Expr'Call ID (Maybe (Type.Type Void)) BindingKey BindingKey

    | Expr'Case ID (Maybe (Type.Type Void)) CaseTree

    | Expr'TupleDestructure1 ID (Maybe (Type.Type Void)) BindingKey -- TODO: figure out better solution to this (probably general destructure expr for any type, or actually probably use case expressions to match on things)
    | Expr'TupleDestructure2 ID (Maybe (Type.Type Void)) BindingKey

    | Expr'Forall ID (Maybe (Type.Type Void)) (NonEmpty TypeVarKey) BindingGroup BindingKey
    | Expr'TypeApply ID (Maybe (Type.Type Void)) BindingKey (Maybe (Type.Type Void))

    | Expr'Poison ID (Maybe (Type.Type Void))
    deriving Show

-- TODO: split case things into separate module?
data CaseTree
    = CaseTree [([CaseMatchingClause], Either CaseTree (BindingGroup, BindingKey))]
    deriving Show
data CaseMatchingClause
    = CaseClause'Match BindingKey CaseMatcher
    | CaseClause'Binding BindingKey
    deriving Show
data CaseMatcher
    = Case'BoolLiteral Bool
    | Case'Tuple
    | Case'AnonADTVariant (Maybe Type.ADTVariantIndex) [Maybe (Type.Type Void)]
    deriving Show

expr_type :: Expr -> (Maybe (Type.Type Void))
expr_type (Expr'Refer _ ty _) = ty
expr_type (Expr'Int _ ty _) = ty
expr_type (Expr'Float _ ty _) = ty
expr_type (Expr'Bool _ ty _) = ty
expr_type (Expr'Char _ ty _) = ty
expr_type (Expr'String _ ty _) = ty
expr_type (Expr'Tuple _ ty _ _) = ty
expr_type (Expr'Lambda _ ty _ _ _ _) = ty
expr_type (Expr'Param _ ty _) = ty
expr_type (Expr'Call _ ty _ _) = ty
expr_type (Expr'Case _ ty _) = ty
expr_type (Expr'TupleDestructure1 _ ty _) = ty
expr_type (Expr'TupleDestructure2 _ ty _) = ty
expr_type (Expr'Forall _ ty _ _ _) = ty
expr_type (Expr'TypeApply _ ty _ _) = ty
expr_type (Expr'MakeADT _ ty _ _ _) = ty
expr_type (Expr'Poison _ ty) = ty

expr_id :: Expr -> ID
expr_id (Expr'Refer id _ _) = id
expr_id (Expr'Int id _ _) = id
expr_id (Expr'Float id _ _) = id
expr_id (Expr'Bool id _ _) = id
expr_id (Expr'Char id _ _) = id
expr_id (Expr'String id _ _) = id
expr_id (Expr'Tuple id _ _ _) = id
expr_id (Expr'Lambda id _ _ _ _ _) = id
expr_id (Expr'Param id _ _) = id
expr_id (Expr'Call id _ _ _) = id
expr_id (Expr'Case id _ _) = id
expr_id (Expr'TupleDestructure1 id _ _) = id
expr_id (Expr'TupleDestructure2 id _ _) = id
expr_id (Expr'Forall id _ _ _ _) = id
expr_id (Expr'TypeApply id _ _ _) = id
expr_id (Expr'MakeADT id _ _ _ _) = id
expr_id (Expr'Poison id _) = id

binding_type :: Binding -> Maybe (Type.Type Void)
binding_type = expr_type . binding_initializer
binding_id :: Binding -> ID
binding_id = expr_id . binding_initializer

chunk_bindings :: BindingChunk -> [BindingKey]
chunk_bindings (SingleBinding b) = [b]
chunk_bindings (MutuallyRecursiveBindings bs) = bs
