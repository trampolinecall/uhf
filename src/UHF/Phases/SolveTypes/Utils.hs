module UHF.Phases.SolveTypes.Utils where

import UHF.Prelude

import UHF.Phases.SolveTypes.Solver.TypeWithInferVar hiding (Type)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Phases.SolveTypes.Solver.TypeWithInferVar as TypeWithInferVar (Type (..))
import qualified UHF.Util.Arena as Arena

substitute :: InferVarArena -> Type.QuantVarKey -> TypeWithInferVar.Type -> TypeWithInferVar.Type -> TypeWithInferVar.Type
substitute unk_arena looking_for replacement ty@(TypeWithInferVar.Type'InferVar unk) =
    case Arena.get unk_arena unk of
        InferVar _ (Substituted unk_actual) -> substitute unk_arena looking_for replacement unk_actual
        InferVar _ Fresh -> ty -- TODO: reconsider if this is correct
substitute _ looking_for replacement ty@(TypeWithInferVar.Type'QuantVar v)
    | looking_for == v = replacement
    | otherwise = ty
substitute unk_arena looking_for replacement (TypeWithInferVar.Type'ADT adt_key params) = TypeWithInferVar.Type'ADT adt_key (map (substitute unk_arena looking_for replacement) params)
substitute _ _ _ ty@(TypeWithInferVar.Type'Synonym _) = ty -- TODO: replace in arguments
substitute _ _ _ TypeWithInferVar.Type'Int = TypeWithInferVar.Type'Int
substitute _ _ _ TypeWithInferVar.Type'Float = TypeWithInferVar.Type'Float
substitute _ _ _ TypeWithInferVar.Type'Char = TypeWithInferVar.Type'Char
substitute _ _ _ TypeWithInferVar.Type'String = TypeWithInferVar.Type'String
substitute _ _ _ TypeWithInferVar.Type'Bool = TypeWithInferVar.Type'Bool
substitute unk_arena looking_for replacement (TypeWithInferVar.Type'Function a r) = TypeWithInferVar.Type'Function (substitute unk_arena looking_for replacement a) (substitute unk_arena looking_for replacement r)
substitute unk_arena looking_for replacement (TypeWithInferVar.Type'Tuple a b) = TypeWithInferVar.Type'Tuple (substitute unk_arena looking_for replacement a) (substitute unk_arena looking_for replacement b)
substitute unk_arena looking_for replacement (TypeWithInferVar.Type'Forall vars ty) = TypeWithInferVar.Type'Forall vars (substitute unk_arena looking_for replacement ty)

-- basically useless function for converting Type Void to Type InferVarKey
-- TODO: remove eventually when replacing Type.Type InferVar with a separate type
void_unk_to_key :: Type.Type -> TypeWithInferVar.Type
void_unk_to_key (Type.Type'ADT k params) = TypeWithInferVar.Type'ADT k (map void_unk_to_key params)
void_unk_to_key (Type.Type'Synonym k) = TypeWithInferVar.Type'Synonym k
void_unk_to_key Type.Type'Int = TypeWithInferVar.Type'Int
void_unk_to_key Type.Type'Float = TypeWithInferVar.Type'Float
void_unk_to_key Type.Type'Char = TypeWithInferVar.Type'Char
void_unk_to_key Type.Type'String = TypeWithInferVar.Type'String
void_unk_to_key Type.Type'Bool = TypeWithInferVar.Type'Bool
void_unk_to_key (Type.Type'Function a r) = TypeWithInferVar.Type'Function (void_unk_to_key a) (void_unk_to_key r)
void_unk_to_key (Type.Type'Tuple a b) = TypeWithInferVar.Type'Tuple (void_unk_to_key a) (void_unk_to_key b)
void_unk_to_key (Type.Type'QuantVar v) = TypeWithInferVar.Type'QuantVar v
void_unk_to_key (Type.Type'Forall vars ty) = TypeWithInferVar.Type'Forall vars (void_unk_to_key ty)
