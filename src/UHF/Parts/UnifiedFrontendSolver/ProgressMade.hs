module UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..)) where

data ProgressMade task
    = Unsuccessful
    | Successful [task]
