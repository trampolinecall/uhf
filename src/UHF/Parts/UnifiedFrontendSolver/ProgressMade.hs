module UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..)) where

data ProgressMade task
    = NoProgressMade
    | ProgressMade [task]
