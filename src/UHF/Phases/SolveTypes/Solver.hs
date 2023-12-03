module UHF.Phases.SolveTypes.Solver
    ( Constraint (..)

    , TypeWithInferVar

    , InferVarKey
    , InferVar (..)
    , InferVarForWhat (..)

    , solve
    ) where

-- TODO: clean up SolveTypes modules so that they import this instead of the submodules here

import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.Solve
import UHF.Phases.SolveTypes.Solver.TypeWithInferVar

type TypeWithInferVar = Type
