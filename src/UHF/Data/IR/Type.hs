module UHF.Data.IR.Type
    ( Type (..)

    , ADT (..)
    , ADTKey

    , TypeSynonym (..)
    , TypeSynonymKey

    , QuantVar (..)
    , QuantVarKey

    , type_kind
    ) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Data.IR.Type.ADT
import UHF.Data.IR.Type.QuantVar
import UHF.Data.IR.Type.Synonym
import qualified UHF.Util.Arena as Arena

data Type
    = Type'ADT ADTKey [Type]
    | Type'Synonym TypeSynonymKey
    | Type'Int
    | Type'Float
    | Type'Char
    | Type'String
    | Type'Bool
    | Type'Function Type Type
    | Type'Tuple Type Type
    | Type'QuantVar QuantVarKey
    | Type'Forall (NonEmpty QuantVarKey) Type
    | Type'Kind'Type
    | Type'Kind'Arrow Type Type
    | Type'Kind'Kind
    deriving Show

type_kind :: Arena.Arena (ADT (t, Type)) ADTKey -> Arena.Arena (TypeSynonym (t, Type)) TypeSynonymKey -> Arena.Arena QuantVar QuantVarKey -> Type -> Type
type_kind adt_arena type_synonym_arena quant_var_arena = go
    where
        go :: Type -> Type
        go t = case t of
            Type'ADT adt_key applied ->
                let ADT _ _ quant_vars _ = Arena.get adt_arena adt_key
                in make_arrows (map quant_var_kind (drop (length applied) quant_vars)) Type'Kind'Type
            Type'Synonym ts_key ->
                let TypeSynonym _ _ (_, expansion) = Arena.get type_synonym_arena ts_key
                in go expansion -- TODO: need to modify this when type synonyms can be parameterized
            Type'Int -> Type'Kind'Type
            Type'Float -> Type'Kind'Type
            Type'Char -> Type'Kind'Type
            Type'String -> Type'Kind'Type
            Type'Bool -> Type'Kind'Type
            Type'Function _ _ -> Type'Kind'Type
            Type'Tuple _ _ -> Type'Kind'Type
            Type'QuantVar qvk -> quant_var_kind qvk
            Type'Forall quant_vars result -> make_arrows (map quant_var_kind (toList quant_vars)) result
            Type'Kind'Type -> Type'Kind'Kind
            Type'Kind'Arrow _ _ -> Type'Kind'Kind
            Type'Kind'Kind -> Type'Kind'Kind

        quant_var_kind :: QuantVarKey -> Type
        quant_var_kind qvk = Type'Kind'Type -- TODO: quant vars with different kinds

        make_arrows :: [Type] -> Type -> Type
        make_arrows [] res = res
        make_arrows (cur_arg:more_args) res = Type'Kind'Arrow cur_arg (make_arrows more_args res)
