module UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult (ResolveResult (..), if_inconclusive) where

-- TODO: move this file into UnifiedFrontendSolver so that it can be shared between all of the phases

import UHF.Prelude

data ResolveResult best_effort_error error result
    = Inconclusive best_effort_error
    | Errored error
    | Resolved result

-- TODO: remove these instances?
instance Functor (ResolveResult best_effort_error error) where
    fmap _ (Inconclusive bee) = Inconclusive bee
    fmap _ (Errored e) = Errored e
    fmap f (Resolved r) = Resolved $ f r

instance Applicative (ResolveResult best_effort_error error) where
    pure = Resolved
    Inconclusive bee <*> _ = Inconclusive bee
    Errored e <*> _ = Errored e
    Resolved f <*> a = fmap f a

instance Monad (ResolveResult best_effort_error error) where
    Inconclusive bee >>= _ = Inconclusive bee
    Errored e >>= _ = Errored e
    Resolved r >>= f = f r

if_inconclusive :: Monad m => ResolveResult bee e result -> m (ResolveResult bee2 e result) -> m (ResolveResult bee2 e result)
if_inconclusive thing compute_new = case thing of
    Inconclusive _ -> compute_new
    Errored e -> pure $ Errored e
    Resolved r -> pure $ Resolved r
