module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.SubstituteQuantVar (substitute_quant_var) where

import UHF.Prelude

import UHF.Data.IR.TypeWithInferVar
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad)

-- TODO: find a better place to put this function
substitute_quant_var :: Type.QuantVarKey -> Type -> Type -> SolveMonad Type
substitute_quant_var looking_for replacement ty = go ty
    where
        go ty@(Type'InferVar ifv) = do
            (_, _, (_, infer_vars)) <- get
            case Arena.get infer_vars ifv of
                InferVar _ (Substituted sub) -> go sub
                InferVar _ Fresh -> pure ty -- TODO: this is not correct because if this infer var later gets substituted for something with looking_for then it wont get replaced
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
