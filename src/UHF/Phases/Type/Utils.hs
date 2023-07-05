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
