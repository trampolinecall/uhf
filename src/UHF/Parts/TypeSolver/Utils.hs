module UHF.Parts.TypeSolver.Utils (substitute_quant_var) where

import UHF.Prelude

import UHF.Parts.TypeSolver.TypeWithInferVar
import qualified UHF.Parts.TypeSolver.SolveMonad as SolveMonad
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Util.Arena as Arena

-- TODO: find a better place to put this function
substitute_quant_var :: Monad under => Type.QuantVarKey -> Type -> Type -> SolveMonad.SolveMonad under Type
substitute_quant_var looking_for replacement ty = go ty
    where
        go ty@(Type'InferVar ifv) =
            SolveMonad.get_infer_vars >>= \ infer_vars ->
            case Arena.get infer_vars ifv of
                InferVar _ (Substituted sub) -> go sub
                InferVar _ Fresh -> pure ty
        go ty@(Type'QuantVar v)
            | v == looking_for = pure replacement
            | otherwise = pure ty
        go (Type'ADT adt_key params) = Type'ADT adt_key <$> mapM go params
        go ty@(Type'Synonym _) = pure ty -- TODO: replace in arguments
        go Type'Int = pure Type'Int
        go Type'Float = pure Type'Float
        go Type'Char = pure Type'Char
        go Type'String = pure Type'String
        go Type'Bool = pure Type'Bool
        go (Type'Function a r) = Type'Function <$> go a <*> go r
        go (Type'Tuple a b) = Type'Tuple <$> go a <*> go b
        go (Type'Forall vars ty) = Type'Forall vars <$> go ty
        go Type'Kind'Type = pure Type'Kind'Type
        go (Type'Kind'Arrow a b) = Type'Kind'Arrow <$> go a <*> go b
        go Type'Kind'Kind = pure Type'Kind'Kind
