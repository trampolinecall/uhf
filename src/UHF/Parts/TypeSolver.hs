module UHF.Parts.TypeSolver
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

    , SolveMonad
    , HasConstraintBacklog
    , new_infer_var
    , push_backlog
    , apply_type_and_push_to_backlog

    , solve_constraint_
    , solve_constraint_and_push_to_backlog
    , solve_constraint_backlog
    , SolveError

    , substitute_quant_var
    ) where

-- TODO: clean up SolveTypes modules so that they import this instead of the submodules here

import UHF.Parts.TypeSolver.Constraint
import UHF.Parts.TypeSolver.Solve
import UHF.Parts.TypeSolver.SolveError
import UHF.Parts.TypeSolver.SolveMonad
import UHF.Parts.TypeSolver.TypeWithInferVar
import UHF.Parts.TypeSolver.TypeWithInferVar.PP
import UHF.Parts.TypeSolver.Utils
