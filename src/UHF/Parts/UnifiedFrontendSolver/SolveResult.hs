module UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult) where

import UHF.Prelude

data SolveResult best_effort_error error result
    = Inconclusive best_effort_error
    | Errored error
    | Resolved result

-- TODO: does this actually even work? (because if we want to have a separate InconclusiveDependency constructor or something like that)
-- newtype SolveResultT best_effort_error error m r = SolveResultT (m (SolveResult best_effort_error error r))
-- deriving (Functor, Applicative, Monad, MonadTrans, Show, Eq, Ord)

-- run_solve_result_t :: Monad m => SolveResultT best_effort_error error m r -> m r
-- run_solve_result_t (SolveResultT m) = m
-- run_solve_result :: SolveResult best_effort_error error r -> r
-- run_solve_result = Identity.runIdentity . run_SolveResult_t

-- TODO: remove these instances?
instance Functor (SolveResult best_effort_error error) where
    fmap _ (Inconclusive bee) = Inconclusive bee
    fmap _ (Errored e) = Errored e
    fmap f (Resolved r) = Resolved $ f r

instance Applicative (SolveResult best_effort_error error) where
    pure = Resolved
    Inconclusive bee <*> _ = Inconclusive bee
    Errored e <*> _ = Errored e
    Resolved f <*> a = fmap f a

instance Monad (SolveResult best_effort_error error) where
    Inconclusive bee >>= _ = Inconclusive bee
    Errored e >>= _ = Errored e
    Resolved r >>= f = f r

if_inconclusive :: Monad m => SolveResult bee e result -> m (SolveResult bee2 e result) -> m (SolveResult bee2 e result)
if_inconclusive thing compute_new = case thing of
    Inconclusive _ -> compute_new
    Errored e -> pure $ Errored e
    Resolved r -> pure $ Resolved r
