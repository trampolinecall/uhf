module UHF.Data.BackendIR
    ( BackendIR (..)
    , CU (..)

    , BindingKey
    , ParamKey

    , TopologicallySorted (..)
    , HasLoops (..)
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

data BackendIR topological_sort_status ty poison_allowed
    = BackendIR
        (Arena.Arena (Type.ADT ty) ADTKey)
        (Arena.Arena (Type.TypeSynonym ty) TypeSynonymKey)
        (Arena.Arena Type.QuantVar Type.QuantVarKey)
        (Arena.Arena (Binding topological_sort_status ty poison_allowed) BindingKey)
        (Arena.Arena (Param ty) ParamKey)
        (CU topological_sort_status poison_allowed)

-- "compilation unit"
data CU topological_sort_status poison_allowed = CU { cu_main_function :: Either poison_allowed BindingKey, cu_bindings :: BindingGroup topological_sort_status, cu_adts :: [ADTKey], cu_type_synonyms :: [TypeSynonymKey] }

data Param ty = Param ID.VariableID ty deriving Show

data HasLoops = HasLoops deriving Show
data TopologicallySorted = TopologicallySorted deriving Show
data BindingGroup topological_sort_status = BindingGroup { binding_group_status :: topological_sort_status, binding_group_bindings :: [BindingKey] } deriving Show

data Binding topological_sort_status ty poison_allowed = Binding { binding_initializer :: Expr topological_sort_status ty poison_allowed }

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

data Expr topological_sort_status ty poison_allowed
    = Expr'Refer ID ty BindingKey
    | Expr'Intrinsic ID ty Intrinsics.IntrinsicBoundValue

    | Expr'Int ID ty Integer
    | Expr'Float ID ty Rational
    | Expr'Bool ID ty Bool
    | Expr'Char ID ty Char
    | Expr'String ID ty Text
    | Expr'Tuple ID ty BindingKey BindingKey
    | Expr'MakeADT ID ty Type.ADT.VariantIndex [ty] [BindingKey]

    | Expr'Lambda ID ty ParamKey (Set.Set BindingKey) (BindingGroup topological_sort_status) BindingKey
    | Expr'Param ID ty ParamKey

    | Expr'Call ID ty BindingKey BindingKey

    | Expr'Match ID ty (MatchTree topological_sort_status poison_allowed)

    | Expr'TupleDestructure1 ID ty BindingKey
    | Expr'TupleDestructure2 ID ty BindingKey
    | Expr'ADTDestructure ID ty BindingKey (Either poison_allowed Type.ADT.FieldIndex)

    | Expr'Forall ID ty QuantVarKey (BindingGroup topological_sort_status) BindingKey
    | Expr'TypeApply ID ty BindingKey ty

    | Expr'Poison ID ty poison_allowed
    deriving Show

data MatchTree topological_sort_status poison_allowed
    = MatchTree [([MatchClause poison_allowed], Either (MatchTree topological_sort_status poison_allowed) (BindingGroup topological_sort_status, BindingKey))]
    deriving Show
data MatchClause poison_allowed
    = MatchClause'Match BindingKey (MatchMatcher poison_allowed)
    | MatchClause'Binding BindingKey
    deriving Show
data MatchMatcher poison_allowed
    = Match'BoolLiteral Bool
    | Match'Tuple
    | Match'AnonADTVariant (Either poison_allowed Type.ADT.VariantIndex)
    deriving Show

expr_type :: Expr topological_sort_status ty poison_allowed -> ty
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
expr_type (Expr'Poison _ ty _) = ty

expr_id :: Expr topological_sort_status ty poison_allowed -> ID
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
expr_id (Expr'Poison id _ _) = id

binding_type :: Binding topological_sort_status ty poison_allowed -> ty
binding_type = expr_type . binding_initializer
binding_id :: Binding topological_sort_status ty poison_allowed -> ID
binding_id = expr_id . binding_initializer
