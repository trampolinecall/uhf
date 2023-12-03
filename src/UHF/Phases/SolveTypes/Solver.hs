module UHF.Phases.SolveTypes.Solver
    ( Constraint (..)

    , TypeInferVarKey
    , TypeInferVar (..)
    , TypeInferVarForWhat (..)

    , KindInferVarKey
    , KindInferVar (..)
    , KindInferVarForWhat (..)

    , solve
    ) where

import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Phases.SolveTypes.Solver.Solve
import UHF.Phases.SolveTypes.Solver.InferVar
