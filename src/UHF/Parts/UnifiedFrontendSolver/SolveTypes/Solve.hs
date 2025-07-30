module UHF.Parts.UnifiedFrontendSolver.SolveTypes.Solve (solve) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Task (TypeSolveTask (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, ask_sir, get_type_expr_evaled_as_type, get_value_iden_resolved)
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver

solve :: TypeSolveTask -> SolveMonad (ProgressMade TypeSolveTask)
solve (ConstraintWhenTypeExprEvaledAsType tyeatk make_constraint) = do
    tyeat <- get_type_expr_evaled_as_type tyeatk
    case tyeat of
        Solved tyeat -> pure $ Successful [Constraint $ make_constraint tyeat]
        Inconclusive _ -> pure Unsuccessful
        Errored _ -> pure $ Successful []
-- TODO: rename Successful to ProgressMade and Unsuccessful to NoProgressMade

solve (Constraint constraint) = do
    SIR.SIR _ adts type_synonyms quant_vars _ _ <- ask_sir
    lift (lift $ TypeSolver.solve_constraint (todo adts) (todo type_synonyms) todo quant_vars constraint) >>= \case
        Just (Left e) -> do
            _ <- lift $ lift $ lift $ Compiler.tell_error (todo e)
            pure $ Successful []
        Just (Right ()) -> pure $ Successful []
        Nothing -> pure Unsuccessful
solve (DefinedToBeTypeOfValueRef ifvk vik) = do
    vi <- get_value_iden_resolved vik
    case vi of
        Solved vi -> pure $ Successful [Constraint $ TypeSolver.Expect todo (todo ifvk) (todo vi)]
        Inconclusive _ -> pure Unsuccessful
        Errored _ -> pure $ Successful []

solve (DefinedToBeTypeOfTypeExpr ifvk tyek) = do
    tye <- get_type_expr_evaled_as_type tyek
    case tye of
        Solved tye -> pure $ Successful [Constraint $ TypeSolver.Expect todo (todo ifvk) (todo tye)]
        Inconclusive _ -> pure Unsuccessful
        Errored _ -> pure $ Successful []
