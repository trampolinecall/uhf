{-# LANGUAGE OverloadedLists #-}

module UHF.Parts.ToRIR.TopologicalSort (BindingsHaveLoopError, sort_bindings, get_captures) where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

import UHF.Source.Span (Span)
import qualified UHF.Data.RIR as RIR
import qualified UHF.Diagnostic as Diagnostic

data Dependency = NeedsInitialized RIR.VariableKey | NeedsCallable RIR.VariableKey deriving (Eq, Ord)

data BindingsHaveLoopError
    = BindingsHaveLoop (Map RIR.VariableKey Span) [Dependency]

instance Diagnostic.ToError BindingsHaveLoopError where
    -- TODO: improve spans: make them point to the variable and not the initializer
    to_error (BindingsHaveLoop spans deps@(loop_start:more)) =
        let middle_deps = init more
            last_dep = last more
        in Diagnostic.Error (Just (dep_span loop_start)) "illegal loop in bindings"
            ( loop_start_message
                : (middle_deps & zip [2..] & map (uncurry more_dep_message))
                ++ [last_dep_message last_dep])
            []
        where
            make_counter i = "(" <> show i <> "/" <> show (length deps) <> ")"

            dep_span dep = spans Map.! get_dependency_vk dep

            loop_start_message = (Just (dep_span loop_start), Diagnostic.MsgError, Just $ make_counter 1 <> " the " <> dep_word_first loop_start <> " of this variable requires that...")
            more_dep_message i dep = (Just (dep_span dep), Diagnostic.MsgNote, Just $ make_counter i <> " this variable is " <> dep_word dep <> ", which further requires that...")
            last_dep_message dep = (Just (dep_span dep), Diagnostic.MsgNote, Just $ make_counter (length deps) <> " this variable is " <> dep_word dep <> ", completing the cycle")
            -- the last one completes the cycle because the same dependency is at the beginning and at the end

            dep_word (NeedsInitialized _) = "already initialzied"
            dep_word (NeedsCallable _) = "ready to be called"

            dep_word_first (NeedsInitialized _) = "initialization"
            dep_word_first (NeedsCallable _) = "callability"

    to_error (BindingsHaveLoop _ []) = error "empty loop in BindingsHaveLoop error"

-- TODO: remove?
get_dependency_vk :: Dependency -> RIR.VariableKey
get_dependency_vk (NeedsInitialized vk) = vk
get_dependency_vk (NeedsCallable vk) = vk

get_captures :: RIR.Expr -> Set RIR.VariableKey
get_captures = Set.map get_dependency_vk . get_execute_dependencies -- the captures of a lambda are the execute dependencies of its body

-- get the dependencies needed to evaluate an expression
get_execute_dependencies :: RIR.Expr -> Set Dependency
get_execute_dependencies = go
    where
        go (RIR.Expr'Identifier _ _ _ (Just i)) = [NeedsInitialized i]
        go (RIR.Expr'Identifier _ _ _ Nothing) = []
        go (RIR.Expr'Char _ _ _) = []
        go (RIR.Expr'String _ _ _) = []
        go (RIR.Expr'Int _ _ _) = []
        go (RIR.Expr'Float _ _ _) = []
        go (RIR.Expr'Bool _ _ _) = []
        go (RIR.Expr'Tuple _ _ a b) = get_execute_dependencies a <> get_execute_dependencies b
        go (RIR.Expr'Lambda _ _ _ _ _) =
            -- to execute the lambda (not execute its body, just define the body), we dont have any dependencies
            -- we dont require that the captures have been executed because that prevents recursive functions from working (because recursive functions capture themselves)
            []
        -- a let's execute dependencies is all of the execute dependencies of its result, and the execute dependencies of all of the bindings defined in the let
        -- we filter out any dependencies that are defined in the let because those are not dependencies needed to execute the let expression
        go (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) =
            let bindings_defined_in_this = Set.fromList $ map (\ (RIR.Binding vk _) -> vk) bindings
            in (Set.unions (map (\ (RIR.Binding _ e) -> get_execute_dependencies e) bindings) <> get_execute_dependencies result) & (Set.filter (not . (`Set.member` bindings_defined_in_this) . get_dependency_vk))
        go (RIR.Expr'Call _ _ callee arg) = get_call_dependencies callee <> get_call_dependencies arg -- because a call expression calls an arbitrary function, it can do arbitrary things with the argument (including calling it), so we conservatively require that the argument is callable
        go (RIR.Expr'Match _ _ _ tree) = go_through_tree tree
            where
                go_through_tree (RIR.MatchTree arms) =
                    arms
                        & map
                            (\ (clauses, result) ->
                                let (clause_deps, defined_in_clauses) = clauses & map go_through_clause & unzip
                                    remove_defined_in_clauses = Set.filter (\ dep -> not $ Set.member (get_dependency_vk dep) (Set.unions defined_in_clauses))

                                    result_dependencies = case result of
                                        Left subtree -> go_through_tree subtree
                                        Right e -> get_execute_dependencies e

                                in remove_defined_in_clauses $ Set.unions clause_deps <> result_dependencies
                            )
                        & Set.unions

                -- first element is execute dependencies, second element is variables defined
                go_through_clause (RIR.MatchClause'Match binding _) = ([NeedsInitialized binding], [])
                go_through_clause (RIR.MatchClause'Assign vk rhs) = (go_match_assign_rhs rhs, [vk])

                go_match_assign_rhs (RIR.MatchAssignRHS'OtherVar vk) = [NeedsInitialized vk]
                go_match_assign_rhs (RIR.MatchAssignRHS'TupleDestructure1 _ vk) = [NeedsInitialized vk]
                go_match_assign_rhs (RIR.MatchAssignRHS'TupleDestructure2 _ vk) = [NeedsInitialized vk]
                go_match_assign_rhs (RIR.MatchAssignRHS'AnonADTVariantField _ vk _) = [NeedsInitialized vk]

        go (RIR.Expr'Forall _ _ _ e) = get_execute_dependencies e
        go (RIR.Expr'TypeApply _ _ _ e _) = get_execute_dependencies e
        go (RIR.Expr'MakeADT _ _ _ _ args) = Set.unions $ map get_execute_dependencies args
        go (RIR.Expr'Poison _ _ _) = []

get_call_dependencies :: RIR.Expr -> Set Dependency
get_call_dependencies = go
    where
        -- these expressions do not store code and therefore have no call dependencies
        go (RIR.Expr'Char _ _ _) = []
        go (RIR.Expr'String _ _ _) = []
        go (RIR.Expr'Int _ _ _) = []
        go (RIR.Expr'Float _ _ _) = []
        go (RIR.Expr'Bool _ _ _) = []
        go (RIR.Expr'Poison _ _ _) = []
        go (RIR.Expr'Identifier _ _ _ Nothing) = []

        -- these expressions are only callable if the expressions it refers to are also callable
        go (RIR.Expr'Identifier _ _ _ (Just i)) = [NeedsCallable i]
        go (RIR.Expr'Call _ _ callee arg) = get_call_dependencies callee <> get_call_dependencies arg -- TODO: this is probably not correct
        go (RIR.Expr'Tuple _ _ a b) = get_call_dependencies a <> get_call_dependencies b
        -- go (RIR.Expr'TupleDestructure1 _ _ tup) = get_call_dependencies tup
        -- go (RIR.Expr'TupleDestructure2 _ _ tup) = get_call_dependencies tup
        -- go (RIR.Expr'ADTDestructure _ _ v _) = get_call_dependencies v
        go (RIR.Expr'TypeApply _ _ _ e _) = get_call_dependencies e
        go (RIR.Expr'MakeADT _ _ _ _ args) = Set.unions $ map get_call_dependencies args
        go (RIR.Expr'Forall _ _ _ e) = get_call_dependencies e

        -- a let is callable if its result is callable
        -- it also requires all of its bindings to be callable but this can be improved upon by taking the call dependencies of the result and rewriting any dependencies defined in the let to their call dependencies (see the commented out code below)
        go (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) = get_call_dependencies result <> Set.unions (map (\ (RIR.Binding _ e) -> get_execute_dependencies e) bindings) <> get_execute_dependencies result
        {-
        go (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) = get_call_dependencies result & rewrite_bindings_defined_in_this bindings_defined_in_this
            where
                bindings_defined_in_this = Set.fromList $ map (\ (RIR.Binding vk _) -> vk) bindings
                exec_deps_of_bindings_defined_in_this = Map.fromList $ map (\ (RIR.Binding vk e) -> (vk, get_call_dependencies e)) bindings
                call_deps_of_bindings_defined_in_this = Map.fromList $ map (\ (RIR.Binding vk e) -> (vk, get_call_dependencies e)) bindings

                -- TODO: this needs to happen repeatedly to handle bindings that depend on other bindings, but this needs to also consider if the binding group has loops, because that can make this code infinitely loop
                rewrite_bindings_defined_in_this =
                    Set.map
                        (\ dep ->
                            if Set.member (get_dependency_vk dep) bindings_defined_in_this
                                then case dep of
                                        NeedsCallable dep_vk -> call_deps_of_bindings_defined_in_this Map.! dep_vk
                                        NeedsInitialized _ -> [] -- the dependency is defined in this let binding, so it is already initialized because this let is assumed to already have been initialized
                                else [dep]
                        )
        -}

        -- a match is callable if all of its arms are callable
        go (RIR.Expr'Match _ _ _ tree) = go_through_tree tree
            where
                go_through_tree (RIR.MatchTree arms) =
                    arms
                        & map
                            (\ (clauses, result) ->
                                let defined_in_clauses = clauses & map go_through_clause & Set.unions
                                    rewrite_vars_defined_in_clauses = todo

                                    result_dependencies = case result of
                                        Left subtree -> go_through_tree subtree
                                        Right e -> get_call_dependencies e
                                in rewrite_vars_defined_in_clauses result_dependencies
                            )
                        & Set.unions

                -- returns the variables defined in the clauses
                go_through_clause (RIR.MatchClause'Match _ _) = []
                go_through_clause (RIR.MatchClause'Assign vk rhs) = [vk]
                -- TODO: this is probably not correct and should also require all of the rhss to be callable too

        -- lambdas store code
        go (RIR.Expr'Lambda _ _ _ captures result) =
            -- in order for a lambda to be callable, its result must be executable and its captures must be callable
            get_execute_dependencies result <> Set.map NeedsCallable captures

sort_bindings :: [RIR.Binding] -> Either (NonEmpty BindingsHaveLoopError, RIR.Bindings) RIR.Bindings
sort_bindings bindings =
    case find_loops bindings of
        [] -> Right $ RIR.Bindings RIR.TopologicallySorted (topological_sort [] bindings)
        a:more ->
            let var_spans = Map.fromList $ map (\ (RIR.Binding vk expr) -> (vk, RIR.expr_span expr)) bindings
            in Left (NonEmpty.map (BindingsHaveLoop var_spans) (a:|more), RIR.Bindings RIR.HasLoops bindings)
    where
        binding_keys_from_bindings = Set.fromList . map (\ (RIR.Binding vk _) -> vk)
        bindings_defined_here = binding_keys_from_bindings bindings

        exec_dependencies :: Map.Map RIR.VariableKey (Set Dependency)
        exec_dependencies =
            bindings
                & map
                    (\ (RIR.Binding var_key initializer) ->
                        ( var_key
                        , get_execute_dependencies initializer
                            & Set.filter (\ dep -> Set.member (get_dependency_vk dep) bindings_defined_here) -- filter to only dependencies that are defined here because this topological sort cant do anything about dependencies defined elsewhere
                        )
                    )
                & Map.fromList
        call_dependencies :: Map.Map RIR.VariableKey (Set Dependency)
        call_dependencies =
            bindings
                & map
                    (\ (RIR.Binding var_key initializer) ->
                        ( var_key
                        , get_call_dependencies initializer
                            & Set.filter (\ dep -> Set.member (get_dependency_vk dep) bindings_defined_here) -- same comment as above
                        )
                    )
                & Map.fromList

        topological_sort done [] = done
        topological_sort done left =
            case List.partition (exec_dependencies_satisfied (binding_keys_from_bindings done)) left of
                ([], _) -> error "topological sort called on bindings with loops"
                (ready, waiting) -> topological_sort (done ++ ready) waiting

        exec_dependencies_satisfied executed (RIR.Binding vk _) = and $ Set.map (dependency_satisfied executed) (exec_dependencies Map.! vk)
        dependency_satisfied executed dep = case dep of
            NeedsInitialized depvk -> depvk `List.elem` executed -- check if dep has been executed

            -- dep needs to be callable, so check if all of its call dependencies are satisfied
            -- also check that dep has been initialized because it needs to be initialized before it is callable
            NeedsCallable depvk -> depvk `List.elem` executed && and (Set.map (dependency_satisfied executed) (call_dependencies Map.! depvk))

        find_loops bindings = trace_from_first (Set.fromList $ map (\ (RIR.Binding vk _) -> NeedsInitialized vk) bindings)
            where
                trace_from_first deps
                    | Set.null deps = []
                    | otherwise =
                        let first = Set.findMin deps
                            (loops, explored) = runWriter $ trace [] first
                        in loops ++ trace_from_first (deps Set.\\ explored)

                trace explored_stack current = do
                    -- TODO: this often outputs multiple slightly different errors for what is actually the same loop
                    tell [current]
                    case List.elemIndex current explored_stack of
                         Just index -> pure [reverse $ current : take (index + 1) explored_stack] -- found loop; the returned value contains the same dependency as its first and last elements
                         _ -> do
                            let deps = case current of
                                    NeedsInitialized depvk -> exec_dependencies Map.! depvk
                                    NeedsCallable depvk -> exec_dependencies Map.! depvk <> call_dependencies Map.! depvk
                            concat <$> mapM (trace (current:explored_stack)) (toList deps)
