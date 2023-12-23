module UHF.Data.IR.Type.PP
    ( define_adt
    , define_type_synonym
    , refer_adt
    , refer_type_synonym
    , refer_type
    ) where

import UHF.Prelude

import UHF.Source.Located (Located (Located))
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.IR.Type as Type
import qualified UHF.PP as PP

define_adt :: Type.ADT ty -> PP.Token
define_adt (Type.ADT _ (Located _ name) _ _) = PP.List ["data ", PP.String name, ";"] -- TODO: variants and type vars

define_type_synonym :: (ty -> PP.Token) -> Type.TypeSynonym ty -> PP.Token
define_type_synonym show_ty (Type.TypeSynonym _ (Located _ name) expansion) = PP.List ["typesyn ", PP.String name, " = ", show_ty expansion, ";"]

refer_adt :: Type.ADT ty -> PP.Token
refer_adt (Type.ADT id _ _ _) = PP.String $ ID.stringify id

refer_type_synonym :: Type.TypeSynonym ty -> PP.Token
refer_type_synonym (Type.TypeSynonym id _ _) = PP.String $ ID.stringify id

-- TODO: construct an ast and print it
-- TODO: precedence for this
refer_type :: Arena.Arena (Type.ADT ty) Type.ADTKey -> Arena.Arena (Type.TypeSynonym ty) Type.TypeSynonymKey -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Type.Type -> PP.Token
refer_type adts type_synonyms vars = go
    where
        go ty =
            case ty of
                Type.Type'ADT k params ->
                    let params' = map go params
                        params''
                            | null params' = ""
                            | otherwise = PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent params']
                    in PP.List [refer_adt (Arena.get adts k), params'']
                Type.Type'Synonym k -> refer_type_synonym $ Arena.get type_synonyms k
                Type.Type'Int -> "int"
                Type.Type'Float -> "float"
                Type.Type'Char -> "char"
                Type.Type'String -> "string"
                Type.Type'Bool -> "bool"
                Type.Type'Function a r ->
                    let a_shown = go a
                        r_shown = go r
                    in (PP.List [a_shown, " -> ", r_shown])
                Type.Type'Tuple a b ->
                    let a_shown = go a
                        b_shown = go b
                    in (PP.parenthesized_comma_list PP.Inconsistent [a_shown, b_shown])
                Type.Type'QuantVar var ->
                    let (Type.QuantVar (Located _ name)) = Arena.get vars var
                    in PP.String name -- TODO: write id
                Type.Type'Forall new_vars ty ->
                    let ty' = go ty
                    in PP.List ["#", PP.parenthesized_comma_list PP.Inconsistent (map (\ vk -> let (Type.QuantVar (Located _ name)) = Arena.get vars vk in PP.String name) (toList new_vars)), " ", ty']
                -- TODO: do kinds correctly
                Type.Type'Kind'Type -> PP.String "*" -- TODO: this does not seem right
                Type.Type'Kind'Arrow a b -> PP.List [refer_type adts type_synonyms vars a, PP.String " -># ", refer_type adts type_synonyms vars b] -- TODO: precedence
                Type.Type'Kind'Kind -> PP.String "<kind>" -- TODO: this is most definitely not correct

