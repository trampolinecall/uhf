module UHF.Data.IR.Type.PP
    ( define_adt
    , define_type_synonym
    , refer_adt
    , refer_type_synonym
    , refer_type
    , refer_type_m
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.PP as PP

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ID as ID

import UHF.IO.Located (Located (Located))

import Data.Functor.Identity (runIdentity)

define_adt :: Type.ADT ty -> PP.Token
define_adt (Type.ADT _ (Located _ name) _ _) = PP.List ["data ", PP.String name, ";"] -- TODO: variants and type vars

define_type_synonym :: (ty -> PP.Token) -> Type.TypeSynonym ty -> PP.Token
define_type_synonym show_ty (Type.TypeSynonym _ (Located _ name) expansion) = PP.List ["typesyn ", PP.String name, " = ", show_ty expansion, ";"]

refer_adt :: Type.ADT ty -> PP.Token
refer_adt (Type.ADT id _ _ _) = PP.String $ ID.stringify id

refer_type_synonym :: Type.TypeSynonym ty -> PP.Token
refer_type_synonym (Type.TypeSynonym id _ _) = PP.String $ ID.stringify id

-- TODO: construct an ast and print it
refer_type :: (tyunk -> PP.Token) -> Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.Var Type.TypeVarKey -> Type.Type tyunk -> PP.Token
refer_type show_tyunk adts type_synonyms vars ty = runIdentity $ refer_type_m (pure . show_tyunk) adts type_synonyms vars ty

refer_type_m :: Monad m => (tyunk -> m PP.Token) -> Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.Var Type.TypeVarKey -> Type.Type tyunk -> m PP.Token
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'ADT k params) =
    mapM (refer_type_m show_tyunk adts type_synonyms vars) params >>= \ params ->
    let params'
            | null params = ""
            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent params]
    in pure $ PP.List [refer_adt (Arena.get adts k), params']
refer_type_m _ _ type_synonyms _ (Type.Type'Synonym k) = pure $ refer_type_synonym $ Arena.get type_synonyms k
refer_type_m _ _ _ _ Type.Type'Int = pure "int"
refer_type_m _ _ _ _ Type.Type'Float = pure "float"
refer_type_m _ _ _ _ Type.Type'Char = pure "char"
refer_type_m _ _ _ _ Type.Type'String = pure "string"
refer_type_m _ _ _ _ Type.Type'Bool = pure "bool"
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Function a r) = do
    a_shown <- refer_type_m show_tyunk adts type_synonyms vars a
    r_shown <- refer_type_m show_tyunk adts type_synonyms vars r
    pure (PP.List [a_shown, " -> ", r_shown])
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Tuple a b) = do
    a_shown <- refer_type_m show_tyunk adts type_synonyms vars a
    b_shown <- refer_type_m show_tyunk adts type_synonyms vars b
    pure (PP.parenthesized_comma_list PP.Inconsistent [a_shown, b_shown])
refer_type_m show_tyunk _ _ _ (Type.Type'Unknown unk) = show_tyunk unk
refer_type_m _ _ _ vars (Type.Type'Variable var) =
    let (Type.Var (Located _ name)) = Arena.get vars var
    in pure $ PP.String name -- TODO: write id
refer_type_m show_tyunk adts type_synonyms vars (Type.Type'Forall new_vars ty) = do
    ty <- refer_type_m show_tyunk adts type_synonyms vars ty
    pure $ PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map (\ vk -> let (Type.Var (Located _ name)) = Arena.get vars vk in PP.String name) (toList new_vars)), " ", ty]
