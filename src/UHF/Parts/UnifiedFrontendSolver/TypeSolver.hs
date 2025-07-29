module UHF.Parts.UnifiedFrontendSolver.TypeSolver
    ( Constraint (..)
    , EqInWhat (..)
    , ExpectInWhat (..)

    , Type (..)
    , pp_type
    , run_infer_var_namer
    , kind_of

    , InferVar (..)
    , InferVarStatus (..)
    , InferVarArena
    , InferVarKey
    , InferVarForWhat (..)
    , infer_var_for_what_sp
    , infer_var_for_what_name
    -- , from_ir_type TODO

    , SolveMonad
    , SolverState (..)
    , run_solve_monad
    , run_solve_monad_with
    , new_infer_var
    , ApplyTypeResult (..)
    , apply_type

    , solve_constraint
    , solve_constraint_backlog
    , SolveError

    , substitute_quant_var
    ) where

-- TODO: clean up SolveTypes modules so that they import this instead of the submodules here

import UHF.Parts.UnifiedFrontendSolver.TypeSolver.Constraint
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.Solve
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.SolveError
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.SolveMonad
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar.PP
import UHF.Parts.UnifiedFrontendSolver.TypeSolver.Utils
