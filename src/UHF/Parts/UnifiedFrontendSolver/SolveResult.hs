module UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..), if_inconclusive) where

import UHF.Prelude

data SolveResult best_effort_error error result
    = Inconclusive best_effort_error
    | Errored error
    | Solved result

-- TODO: remove these instances?
instance Functor (SolveResult best_effort_error error) where
    fmap _ (Inconclusive bee) = Inconclusive bee
    fmap _ (Errored e) = Errored e
    fmap f (Solved r) = Solved $ f r

instance Applicative (SolveResult best_effort_error error) where
    pure = Solved
    Inconclusive bee <*> _ = Inconclusive bee
    Errored e <*> _ = Errored e
    Solved f <*> a = fmap f a

instance Monad (SolveResult best_effort_error error) where
    Inconclusive bee >>= _ = Inconclusive bee
    Errored e >>= _ = Errored e
    Solved r >>= f = f r

-- TODO: is this actually used anywhere? presumably not now that everything keeps track of a task list
if_inconclusive :: Monad m => SolveResult bee e result -> m (SolveResult bee2 e result) -> m (SolveResult bee2 e result)
if_inconclusive thing compute_new = case thing of
    Inconclusive _ -> compute_new
    Errored e -> pure $ Errored e
    Solved r -> pure $ Solved r
