{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult (..), InfixGroupResults, InfixGroupFinalResults) where

import UHF.Prelude

import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (ValueRef)
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)

data InfixGroupResult = Call InfixGroupResult InfixGroupResult | Operand Int | Operator ValueRef

type InfixGroupResults = Map (SIR.ID.ID "BinaryOpsExpr") (SolveResult () () InfixGroupResult)
type InfixGroupFinalResults = Map (SIR.ID.ID "BinaryOpsExpr") (Maybe InfixGroupResult)
