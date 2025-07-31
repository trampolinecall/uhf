{-# LANGUAGE DeriveFunctor #-}

module UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..)) where

import UHF.Prelude

data ProgressMade task
    = NoProgressMade
    | ProgressMade [task]
    deriving Functor
