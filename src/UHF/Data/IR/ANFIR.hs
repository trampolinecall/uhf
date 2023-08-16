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
    , MatchTree (..)
    , MatchClause (..)
    , MatchMatcher (..)
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

data Param = Param { param_bvid :: ID.BoundValueID, param_ty :: Maybe (Type.Type Void) } deriving Show

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
    = Expr'Refer ID BindingKey

    | Expr'Int ID Integer
    | Expr'Float ID Rational
    | Expr'Bool ID Bool
    | Expr'Char ID Char
    | Expr'String ID Text
    | Expr'Tuple ID BindingKey BindingKey
    | Expr'MakeADT ID Type.ADTVariantIndex [Maybe (Type.Type Void)] [BindingKey]

    | Expr'Lambda ID ParamKey (Set.Set BindingKey) BindingGroup BindingKey -- TODO: dont use BindingKey Ord for order of captures
    | Expr'Param ID ParamKey

    | Expr'Call ID BindingKey BindingKey

    -- same comment as in rir: match needs to know its result type in case its match tree has no arms
    | Expr'Match ID (Maybe (Type.Type Void)) MatchTree

    | Expr'TupleDestructure1 ID BindingKey
    | Expr'TupleDestructure2 ID BindingKey
    | Expr'ADTDestructure ID (Maybe (Type.Type Void)) BindingKey (Maybe Type.ADTFieldIndex) -- TODO: remove type from this; also substitute function?; also function in Type to get field type of adt but already substituted?

    | Expr'Forall ID (NonEmpty TypeVarKey) BindingGroup BindingKey
    | Expr'TypeApply ID (Maybe (Type.Type Void)) BindingKey (Maybe (Type.Type Void)) -- TODO: also remove type from this

    | Expr'Poison ID (Maybe (Type.Type Void))
    deriving Show

-- TODO: split match things into separate module?
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
    | Match'AnonADTVariant (Maybe Type.ADTVariantIndex)
    deriving Show

get_bk_ty :: Arena.Arena Param ParamKey -> Arena.Arena Binding BindingKey -> BindingKey -> Maybe (Type.Type Void)
get_bk_ty param_arena binding_arena bk = Arena.get binding_arena bk & binding_initializer & expr_type param_arena binding_arena
expr_type :: Arena.Arena Param ParamKey -> Arena.Arena Binding BindingKey -> Expr -> (Maybe (Type.Type Void))
expr_type param_arena binding_arena (Expr'Refer _ other) = get_bk_ty param_arena binding_arena other
expr_type _ _ (Expr'Int _ _) = Just Type.Type'Int
expr_type _ _ (Expr'Float _ _) = Just Type.Type'Float
expr_type _ _ (Expr'Bool _ _) = Just Type.Type'Bool
expr_type _ _ (Expr'Char _ _) = Just Type.Type'Char
expr_type _ _ (Expr'String _ _) = Just Type.Type'String
expr_type param_arena binding_arena (Expr'Tuple _ a b) = Type.Type'Tuple <$> (get_bk_ty param_arena binding_arena a) <*> (get_bk_ty param_arena binding_arena b)
expr_type _ _ (Expr'MakeADT _ (Type.ADTVariantIndex adt_key _) vars _) = Type.Type'ADT adt_key <$> sequence vars
expr_type param_arena binding_arena (Expr'Lambda _ param _ _ result) = Type.Type'Function <$> (Arena.get param_arena param & param_ty) <*> get_bk_ty param_arena binding_arena result
expr_type param_arena _ (Expr'Param _ param) = Arena.get param_arena param & param_ty
expr_type param_arena binding_arena (Expr'Call _ callee _) =
    let callee_ty = get_bk_ty param_arena binding_arena callee
    in callee_ty <&> \case
        Type.Type'Function _ res -> res
        _ -> error $ "anfir call expression created with callee of type " <> show callee_ty
expr_type _ _ (Expr'Match _ ty _) = ty
expr_type param_arena binding_arena (Expr'TupleDestructure1 _ tup) =
    let tup_ty = get_bk_ty param_arena binding_arena tup
    in tup_ty <&> \case
        Type.Type'Tuple a _ -> a
        _ -> error $ "anfir TupleDestructure1 expr created with tuple of type " <> show tup_ty
expr_type param_arena binding_arena (Expr'TupleDestructure2 _ tup) =
    let tup_ty = get_bk_ty param_arena binding_arena tup
    in tup_ty <&> \case
        Type.Type'Tuple _ b -> b
        _ -> error $ "anfir TupleDestructure2 expr created with tuple of type " <> show tup_ty
expr_type _ _ (Expr'ADTDestructure _ ty _ _) = ty
expr_type param_arena binding_arena (Expr'Forall _ tyvars _ result) = Type.Type'Forall tyvars <$> get_bk_ty param_arena binding_arena result
expr_type _ _ (Expr'TypeApply _ ty _ _) = ty
expr_type _ _ (Expr'Poison _ ty) = ty

expr_id :: Expr -> ID
expr_id (Expr'Refer id _) = id
expr_id (Expr'Int id _) = id
expr_id (Expr'Float id _) = id
expr_id (Expr'Bool id _) = id
expr_id (Expr'Char id _) = id
expr_id (Expr'String id _) = id
expr_id (Expr'Tuple id _ _) = id
expr_id (Expr'MakeADT id _ _ _) = id
expr_id (Expr'Lambda id _ _ _ _) = id
expr_id (Expr'Param id _) = id
expr_id (Expr'Call id _ _) = id
expr_id (Expr'Match id _ _) = id
expr_id (Expr'TupleDestructure1 id _) = id
expr_id (Expr'TupleDestructure2 id _) = id
expr_id (Expr'ADTDestructure id _ _ _) = id
expr_id (Expr'Forall id _ _ _) = id
expr_id (Expr'TypeApply id _ _ _) = id
expr_id (Expr'Poison id _) = id

binding_type :: Arena.Arena Param ParamKey -> Arena.Arena Binding BindingKey -> Binding -> Maybe (Type.Type Void)
binding_type param_arena binding_arena = expr_type param_arena binding_arena . binding_initializer
binding_id :: Binding -> ID
binding_id = expr_id . binding_initializer

chunk_bindings :: BindingChunk -> [BindingKey]
chunk_bindings (SingleBinding b) = [b]
chunk_bindings (MutuallyRecursiveBindings bs) = bs
