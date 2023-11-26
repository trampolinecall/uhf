module UHF.Phases.Type.Utils where

import UHF.Util.Prelude

import qualified Arena
import qualified UHF.Data.IR.Type as Type

import UHF.Phases.Type.Unknown

substitute :: TypeUnknownArena -> Type.TypeVarKey -> Type.Type TypeUnknownKey -> Type.Type TypeUnknownKey -> Type.Type TypeUnknownKey
substitute unk_arena looking_for replacement ty@(Type.Type'Unknown unk) =
    case Arena.get unk_arena unk of
        TypeUnknown _ (Substituted unk_actual) -> substitute unk_arena looking_for replacement unk_actual
        TypeUnknown _ Fresh -> ty -- TODO: reconsider if this is correct
substitute _ looking_for replacement ty@(Type.Type'Variable v)
    | looking_for == v = replacement
    | otherwise = ty
substitute unk_arena looking_for replacement (Type.Type'ADT adt_key params) = Type.Type'ADT adt_key (map (substitute unk_arena looking_for replacement) params)
substitute _ _ _ ty@(Type.Type'Synonym _) = ty -- TODO: replace in arguments
substitute _ _ _ Type.Type'Int = Type.Type'Int
substitute _ _ _ Type.Type'Float = Type.Type'Float
substitute _ _ _ Type.Type'Char = Type.Type'Char
substitute _ _ _ Type.Type'String = Type.Type'String
substitute _ _ _ Type.Type'Bool = Type.Type'Bool
substitute unk_arena looking_for replacement (Type.Type'Function a r) = Type.Type'Function (substitute unk_arena looking_for replacement a) (substitute unk_arena looking_for replacement r)
substitute unk_arena looking_for replacement (Type.Type'Tuple a b) = Type.Type'Tuple (substitute unk_arena looking_for replacement a) (substitute unk_arena looking_for replacement b)
substitute unk_arena looking_for replacement (Type.Type'Forall vars ty) = Type.Type'Forall vars (substitute unk_arena looking_for replacement ty)

-- basically useless function for converting Type Void to Type TypeUnknownKey
-- TODO: remove eventually when replacing Type.Type Unknown with a separate type
void_unk_to_key :: Type.Type Void -> Type.Type unk
void_unk_to_key (Type.Type'ADT k params) = Type.Type'ADT k (map void_unk_to_key params)
void_unk_to_key (Type.Type'Synonym k) = Type.Type'Synonym k
void_unk_to_key Type.Type'Int = Type.Type'Int
void_unk_to_key Type.Type'Float = Type.Type'Float
void_unk_to_key Type.Type'Char = Type.Type'Char
void_unk_to_key Type.Type'String = Type.Type'String
void_unk_to_key Type.Type'Bool = Type.Type'Bool
void_unk_to_key (Type.Type'Function a r) = Type.Type'Function (void_unk_to_key a) (void_unk_to_key r)
void_unk_to_key (Type.Type'Tuple a b) = Type.Type'Tuple (void_unk_to_key a) (void_unk_to_key b)
void_unk_to_key (Type.Type'Unknown void) = absurd void
void_unk_to_key (Type.Type'Variable v) = Type.Type'Variable v
void_unk_to_key (Type.Type'Forall vars ty) = Type.Type'Forall vars (void_unk_to_key ty)
