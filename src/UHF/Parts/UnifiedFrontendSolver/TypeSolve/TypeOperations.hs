module UHF.Parts.UnifiedFrontendSolver.TypeSolve.TypeOperations (kind_of) where

import UHF.Prelude

import UHF.Source.Span (Span)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.SIR as SIR
import UHF.Data.SIR.Stage as Stage
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Type

kind_of :: Arena.Arena (SIR.ADT stage) Type.ADTKey -> Arena.Arena (SIR.TypeSynonym stage) Type.TypeSynonymKey -> Arena.Arena Type.QuantVar Type.QuantVarKey -> Type stage -> Type stage
kind_of adt_arena type_synonym_arena quant_var_arena = go
    where
        go :: Type stage -> Type stage
        go t = case t of
            Type'ADT adt_key applied ->
                let SIR.ADT _ _ quant_vars _ = Arena.get adt_arena adt_key
                in make_arrows (map quant_var_kind (drop (length applied) quant_vars)) Type'Kind'Type
            Type'Synonym ts_key ->
                let SIR.TypeSynonym _ _ expansion = Arena.get type_synonym_arena ts_key
                in go $ todo expansion -- TODO: need to modify this when type synonyms can be parameterized, also TODO: figure this out
            Type'Int -> Type'Kind'Type
            Type'Float -> Type'Kind'Type
            Type'Char -> Type'Kind'Type
            Type'String -> Type'Kind'Type
            Type'Bool -> Type'Kind'Type
            Type'Function _ _ -> Type'Kind'Type
            Type'Tuple _ _ -> Type'Kind'Type
            Type'QuantVar qvk -> quant_var_kind qvk
            Type'InferVar _ _ -> Type'Kind'Type -- TODO: infer vars with different kinds
            Type'Forall quant_vars result -> make_arrows (map quant_var_kind (toList quant_vars)) result
            Type'Kind'Type -> Type'Kind'Kind
            Type'Kind'Arrow _ _ -> Type'Kind'Kind
            Type'Kind'Kind -> Type'Kind'Kind

        quant_var_kind :: Type.QuantVarKey -> Type stage
        quant_var_kind qvk = Type'Kind'Type -- TODO: quant vars with different kinds

        make_arrows :: [Type stage] -> Type stage -> Type stage
        make_arrows [] res = res
        make_arrows (cur_arg:more_args) res = Type'Kind'Arrow cur_arg (make_arrows more_args res)
