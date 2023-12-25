{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module UHF.Parts.TypeSolver.SolveMonad
    ( SolveMonad
    , HasConstraintBacklog
    , new_infer_var
    , push_backlog
    , take_backlog
    , put_backlog
    ) where

import UHF.Prelude

import UHF.Parts.TypeSolver.Constraint
import UHF.Parts.TypeSolver.TypeWithInferVar
import qualified UHF.Util.Arena as Arena

type SolveMonad m = MonadState InferVarArena m
type HasConstraintBacklog m = MonadState [Constraint] m

new_infer_var :: SolveMonad m => InferVarForWhat -> m InferVarKey
new_infer_var for_what =
    state $ \ vars -> let (k, vars') = Arena.put (InferVar for_what Fresh) vars in (k, vars')

push_backlog :: HasConstraintBacklog m => Constraint -> m ()
push_backlog new_constraint = modify $ \ backlog -> (new_constraint : backlog)

take_backlog :: HasConstraintBacklog m => m [Constraint]
take_backlog = state $ \ backlog -> (backlog, [])

put_backlog :: HasConstraintBacklog m => [Constraint] -> m ()
put_backlog = put
