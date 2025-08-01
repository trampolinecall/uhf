{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Solve (group) where

import UHF.Prelude

import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult (..))
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (ValueRef)
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, get_value_iden_resolved)
import qualified UHF.Util.Arena as Arena

group :: InfixGroupTask -> SolveMonad (ProgressMade InfixGroupTask)
group (InfixGroupTask operators result_key) = do
    let first = Operand 0
        more = zip operators [1 ..]
    res <- go first more 0

    case res of
        Inconclusive _ -> pure NoProgressMade
        Errored () -> do
            -- TODO: this is very similar to put_result from NameResolve so maybe there is a general version that can be put into UnifiedFrontendSolver.Solving?
            modify $
                \(nr_things, result_arena, infer_vars) ->
                    ( nr_things
                    , Arena.modify
                        result_arena
                        result_key
                        ( \case
                            Inconclusive _ -> Errored ()
                            _ -> Errored () -- TODO: internal warning because there was already a result here and it was recomputed?
                        )
                    , infer_vars
                    )
            pure $ ProgressMade []
        Solved (res, a) -> do
            when (not $ null a) $ error "internal error: still operations to group after grouping binary ops"

            modify $
                \(nr_things, result_arena, infer_vars) ->
                    ( nr_things
                    , Arena.modify
                        result_arena
                        result_key
                        ( \case
                            Inconclusive _ -> Solved res
                            _ -> Solved res -- TODO: internal warning because there was already a result here and it was recomputed?
                        )
                    , infer_vars
                    )
            pure $ ProgressMade []
    where
        go :: InfixGroupResult -> [(SIR.ID.ID "ValueIden", Int)] -> Int -> SolveMonad (SolveResult () () (InfixGroupResult, [(SIR.ID.ID "ValueIden", Int)]))
        go left more@((first_op, first_rhs) : after_first_op) cur_precedence = do
            first_op <- get_value_iden_resolved first_op
            case first_op of
                Solved first_op -> do
                    let op_prec = get_op_prec first_op
                    -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
                    if op_prec > cur_precedence
                        then do
                            -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                            rhs_grouped <- go (Operand first_rhs) after_first_op op_prec -- TODO: associativity
                            case rhs_grouped of
                                Solved (rhs, after) -> do
                                    let left' = Call (Call (Operator first_op) left) rhs
                                    go left' after cur_precedence
                                Errored _ -> pure $ Errored ()
                                Inconclusive _ -> pure $ Inconclusive ()
                        else pure $ Solved (left, more)
                Errored _ -> pure $ Errored ()
                Inconclusive _ -> pure $ Inconclusive ()
        go left [] _ = pure $ Solved (left, [])

get_op_prec :: ValueRef -> Int
get_op_prec = const 1 -- TODO: precedence
