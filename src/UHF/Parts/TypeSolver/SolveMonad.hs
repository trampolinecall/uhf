{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Parts.TypeSolver.SolveMonad
    ( SolveMonad (..)
    , SolverState (..)
    , run_solve_monad
    , run_solve_monad_with
    , new_infer_var
    , UHF.Parts.TypeSolver.SolveMonad.get
    , UHF.Parts.TypeSolver.SolveMonad.put
    , get_infer_vars
    , modify_infer_vars
    , take_backlog
    , put_backlog
    ) where

import UHF.Prelude

import Control.Monad.Trans.Class (MonadTrans)

import UHF.Parts.TypeSolver.Constraint
import UHF.Parts.TypeSolver.TypeWithInferVar
import qualified UHF.Util.Arena as Arena

newtype SolveMonad under a = SolveMonad (StateT SolverState under a) deriving (Functor, Applicative, Monad, MonadTrans)
data SolverState = SolverState [Constraint] InferVarArena

run_solve_monad :: SolveMonad under result -> under (result, SolverState)
run_solve_monad s = run_solve_monad_with s (SolverState [] Arena.new)

run_solve_monad_with :: SolveMonad under result -> SolverState -> under (result, SolverState)
run_solve_monad_with (SolveMonad s) old_state = runStateT s old_state

new_infer_var :: Monad under => InferVarForWhat -> SolveMonad under InferVarKey
new_infer_var for_what = SolveMonad $ state $ \ (SolverState constraints vars) -> let (k, vars') = Arena.put (InferVar for_what Fresh) vars in (k, SolverState constraints vars')

get :: Monad under => SolveMonad under SolverState
get = SolveMonad UHF.Prelude.get

put :: Monad under => SolverState -> SolveMonad under ()
put a = SolveMonad (UHF.Prelude.put a)

get_infer_vars :: Monad under => SolveMonad under InferVarArena
get_infer_vars = SolveMonad $ UHF.Prelude.get >>= \ (SolverState _ vars) -> pure vars

modify_infer_vars :: Monad under => (InferVarArena -> InferVarArena) -> SolveMonad under ()
modify_infer_vars f = SolveMonad $ UHF.Prelude.modify $ \ (SolverState constraints a) -> SolverState constraints (f a)

take_backlog :: Monad under => SolveMonad under [Constraint]
take_backlog = SolveMonad $ StateT $ \ (SolverState backlog ifvs) -> pure (backlog, SolverState [] ifvs)

put_backlog :: Monad under => [Constraint] -> SolveMonad under ()
put_backlog new_backlog = SolveMonad $ UHF.Prelude.modify $ \ (SolverState _ ifvars) -> SolverState new_backlog ifvars
