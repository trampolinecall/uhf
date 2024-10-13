{-# LANGUAGE OverloadedLists #-}

module UHF.Parts.ToRIR.TopologicalSort (BindingsHaveLoopError, sort_bindings, get_captures) where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified UHF.Data.RIR as RIR
import qualified UHF.Diagnostic as Diagnostic
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

data Dependency = NeedsInitialized RIR.VariableKey | NeedsCallable RIR.VariableKey deriving (Eq, Ord, Show)

data BindingsHaveLoopError
    = BindingsHaveLoop (Map RIR.VariableKey Span) [Dependency]

instance Diagnostic.ToError BindingsHaveLoopError where
    -- TODO: improve this error message
    to_error (BindingsHaveLoop spans deps@(loop_start : more)) =
        let middle_deps = init more
            last_dep = last more
        in Diagnostic.Error
            (Just $ dep_span loop_start)
            "illegal loop in bindings"
            ( first_dep_message
                : (middle_deps & zip [2 ..] & map (uncurry more_dep_message))
                ++ [last_dep_message last_dep]
            )
            []
        where
            make_counter i = "(" <> show i <> "/" <> show (length deps) <> ")"

            dep_span dep = spans Map.! get_dependency_vk dep

            first_dep_message =
                ( Just (dep_span loop_start)
                , Diagnostic.MsgError
                , Just $ make_counter 1 <> " the " <> dep_word_first loop_start <> " of this variable requires that..."
                )
            more_dep_message i dep = (Just (dep_span dep), Diagnostic.MsgNote, Just $ make_counter i <> " this variable is " <> dep_word dep <> ", which further requires that...")
            last_dep_message dep = (Just (dep_span dep), Diagnostic.MsgNote, Just $ make_counter (length deps) <> " this variable is " <> dep_word dep <> ", completing the cycle")
            -- the last one completes the cycle because the same dependency is at the beginning and at the end

            -- TODO: especially these words
            dep_word (NeedsInitialized _) = "already initialzied"
            dep_word (NeedsCallable _) = "ready to be called"

            dep_word_first (NeedsInitialized _) = "initialization"
            dep_word_first (NeedsCallable _) = "callability"
    to_error (BindingsHaveLoop _ []) = error "empty loop in BindingsHaveLoop error"

-- TODO: remove?
get_dependency_vk :: Dependency -> RIR.VariableKey
get_dependency_vk (NeedsInitialized vk) = vk
get_dependency_vk (NeedsCallable vk) = vk

get_captures :: RIR.VariableKey -> RIR.Expr -> Set RIR.VariableKey
-- the captures of a lambda are just any variables that it references that it does not define
-- we have to make sure to exclude the parameter that the lambda has because it also defines that
get_captures param expr = get_outside_references expr Set.\\ Set.singleton param

-- get all variables that are not defined within this expression and that are referred to by identifier expressions
-- "an outside reference" = a reference to some variable defined outside of this expression
get_outside_references :: RIR.Expr -> Set RIR.VariableKey
get_outside_references (RIR.Expr'Identifier _ _ _ (Just i)) = [i]
get_outside_references (RIR.Expr'Identifier _ _ _ Nothing) = []
get_outside_references (RIR.Expr'Intrinsic _ _ _ _) = []
get_outside_references (RIR.Expr'Char _ _ _) = []
get_outside_references (RIR.Expr'String _ _ _) = []
get_outside_references (RIR.Expr'Int _ _ _) = []
get_outside_references (RIR.Expr'Float _ _ _) = []
get_outside_references (RIR.Expr'Bool _ _ _) = []
get_outside_references (RIR.Expr'Tuple _ _ a b) = get_outside_references a <> get_outside_references b
get_outside_references (RIR.Expr'Lambda _ _ param captures body) = (captures <> get_outside_references body) Set.\\ Set.singleton param -- TODO: using captures is redundant, so remove? also double check if it is actually redundant or if i just think that
get_outside_references (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) =
    let variables_defined_in_this_let = Set.fromList $ map (\(RIR.Binding vk _) -> vk) bindings

        variable_initializer_references = Set.unions $ map (\(RIR.Binding _ e) -> get_outside_references e) bindings
        result_references = get_outside_references result

        all_references = variable_initializer_references <> result_references
    in all_references Set.\\ variables_defined_in_this_let
get_outside_references (RIR.Expr'Call _ _ callee arg) = get_outside_references callee <> get_outside_references arg
get_outside_references (RIR.Expr'Match _ _ _ tree) = go_through_tree tree
    where
        go_through_tree (RIR.MatchTree arms) =
            arms
                & map
                    ( \(clauses, result) ->
                        let (clause_refs, defined_in_clauses) = clauses & map go_through_clause & unzip

                            result_refs = case result of
                                Left subtree -> go_through_tree subtree
                                Right e -> get_outside_references e
                        in (Set.unions clause_refs <> result_refs) Set.\\ Set.unions defined_in_clauses
                    )
                & Set.unions

        -- first element is references, second element is variables defined
        go_through_clause (RIR.MatchClause'Match binding _) = ([binding], [])
        go_through_clause (RIR.MatchClause'Assign vk rhs) = (go_match_assign_rhs rhs, [vk])

        go_match_assign_rhs (RIR.MatchAssignRHS'OtherVar vk) = [vk]
        go_match_assign_rhs (RIR.MatchAssignRHS'TupleDestructure1 _ vk) = [vk]
        go_match_assign_rhs (RIR.MatchAssignRHS'TupleDestructure2 _ vk) = [vk]
        go_match_assign_rhs (RIR.MatchAssignRHS'AnonADTVariantField _ vk _) = [vk]
get_outside_references (RIR.Expr'Forall _ _ _ e) = get_outside_references e
get_outside_references (RIR.Expr'TypeApply _ _ _ e _) = get_outside_references e
get_outside_references (RIR.Expr'MakeADT _ _ _ _ args) = Set.unions $ map get_outside_references args
get_outside_references (RIR.Expr'Poison _ _ _) = []

-- get the dependencies needed to evaluate an expression
get_execute_dependencies :: RIR.Expr -> Set Dependency
get_execute_dependencies = go
    where
        go (RIR.Expr'Identifier _ _ _ (Just i)) = [NeedsInitialized i]
        go (RIR.Expr'Identifier _ _ _ Nothing) = []
        go (RIR.Expr'Intrinsic _ _ _ _) = []
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
        -- a let's execute dependencies is all of the execute dependencies of its result and the execute dependencies of all of the initializers of the bindings defined in the let
        -- we also rewrite any dependnecies on variables defined in the bindings as the dependencies of those bindings themselves
        -- for example, in the let expression
        --      let a = b x
        --      a(c)
        -- instead of returning a dependency IsCallable a, we rewrite that dependency to the call dependencies of a itself, namely [IsCallable b, IsInitialized x]
        -- this is because the topological sort handling the topmost let does not have control over the topological sorting of the bindings defined in the let, so it will not work properly
        go (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) =
            let variables_defined_here = map (\(RIR.Binding vk e) -> (vk, get_execute_dependencies e, get_call_dependencies e)) bindings
            in (Set.unions (map (\(RIR.Binding _ e) -> get_execute_dependencies e) bindings) <> get_execute_dependencies result)
                & rewrite_dependencies variables_defined_here
        -- because a call expression calls an arbitrary function, it can do arbitrary things with the argument, including calling it, so we conservatively require that the argument is callable
        go (RIR.Expr'Call _ _ callee arg) = get_call_dependencies callee <> get_call_dependencies arg
        go (RIR.Expr'Match _ _ _ tree) = go_through_tree tree
            where
                go_through_tree (RIR.MatchTree arms) =
                    arms
                        & map
                            ( \(clauses, result) ->
                                let vars_defined_in_clauses = concatMap get_vars_defined_in_clause clauses
                                    clause_deps = map get_clause_deps clauses

                                    result_dependencies = case result of
                                        Left subtree -> go_through_tree subtree
                                        Right e -> get_execute_dependencies e
                                in (Set.unions clause_deps <> result_dependencies) & rewrite_dependencies vars_defined_in_clauses
                            )
                        & Set.unions

                get_vars_defined_in_clause (RIR.MatchClause'Match _ _) = []
                get_vars_defined_in_clause (RIR.MatchClause'Assign vk rhs) = [(vk, match_rhs_exec_deps rhs, match_rhs_call_deps rhs)]

                get_clause_deps (RIR.MatchClause'Match binding _) = [NeedsInitialized binding]
                get_clause_deps (RIR.MatchClause'Assign _ rhs) = match_rhs_exec_deps rhs
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
        go (RIR.Expr'Intrinsic _ _ _ _) = []
        -- these expressions are only callable if the expressions it refers to are also callable
        go (RIR.Expr'Identifier _ _ _ (Just i)) = [NeedsCallable i]
        go (RIR.Expr'Call _ _ callee arg) =
            -- the callee has to be callable because it is being called
            -- the argument passed has to also be callable because the function that it is passed to can do arbitrary things to it, so we conservatively enforce that it is also callable in case the function does call it
            -- the result of the call has to be callable, but we cannot enforce that here and instead must enforce that all lambdas must return a callable expression (see below in go for lambdas)
            get_call_dependencies callee <> get_call_dependencies arg
        go (RIR.Expr'Tuple _ _ a b) = get_call_dependencies a <> get_call_dependencies b
        go (RIR.Expr'TypeApply _ _ _ e _) = get_call_dependencies e
        go (RIR.Expr'MakeADT _ _ _ _ args) = Set.unions $ map get_call_dependencies args
        go (RIR.Expr'Forall _ _ _ e) = get_call_dependencies e
        -- a let is callable if its result is callable
        go (RIR.Expr'Let _ _ (RIR.Bindings _ bindings) _ _ result) =
            let variables_defined_here = map (\(RIR.Binding vk e) -> (vk, get_execute_dependencies e, get_call_dependencies e)) bindings
            in (get_call_dependencies result <> Set.unions (map (\(RIR.Binding _ e) -> get_call_dependencies e) bindings))
                & rewrite_dependencies variables_defined_here
        go (RIR.Expr'Match _ _ _ tree) = go_through_tree tree
            where
                go_through_tree (RIR.MatchTree arms) =
                    arms
                        & map
                            ( \(clauses, result) ->
                                let vars_defined_in_clauses = concatMap get_vars_defined_in_clause clauses
                                    clause_deps = map get_clause_deps clauses

                                    result_dependencies = case result of
                                        Left subtree -> go_through_tree subtree
                                        Right e -> get_call_dependencies e
                                in Set.unions clause_deps <> result_dependencies & rewrite_dependencies vars_defined_in_clauses
                            )
                        & Set.unions

                get_vars_defined_in_clause :: RIR.MatchClause -> [(RIR.VariableKey, Set Dependency, Set Dependency)]
                get_vars_defined_in_clause (RIR.MatchClause'Match _ _) = []
                get_vars_defined_in_clause (RIR.MatchClause'Assign vk rhs) = [(vk, match_rhs_exec_deps rhs, match_rhs_call_deps rhs)]

                get_clause_deps (RIR.MatchClause'Match binding _) = [NeedsCallable binding]
                get_clause_deps (RIR.MatchClause'Assign _ rhs) = match_rhs_call_deps rhs

        -- lambdas store code
        go (RIR.Expr'Lambda _ _ param _ body) =
            -- in order for a lambda to be callable, its body must be executable
            -- because this lambda result can also go into a call expression, we conservatively require that it is also callable here because we cannot enforce that at the call expression
            -- we filter out the parameter because the topological sort can't do anything about that
            -- if the body depends on the parameter being executable or callable, that is ensured by the requirement that all call expressions' arguments must be callable
            (get_execute_dependencies body <> get_call_dependencies body) & Set.filter (\dep -> get_dependency_vk dep /= param)

match_rhs_exec_deps :: RIR.MatchAssignRHS -> Set Dependency
match_rhs_exec_deps (RIR.MatchAssignRHS'OtherVar vk) = [NeedsInitialized vk]
match_rhs_exec_deps (RIR.MatchAssignRHS'TupleDestructure1 _ vk) = [NeedsInitialized vk]
match_rhs_exec_deps (RIR.MatchAssignRHS'TupleDestructure2 _ vk) = [NeedsInitialized vk]
match_rhs_exec_deps (RIR.MatchAssignRHS'AnonADTVariantField _ vk _) = [NeedsInitialized vk]

match_rhs_call_deps :: RIR.MatchAssignRHS -> Set Dependency
match_rhs_call_deps (RIR.MatchAssignRHS'OtherVar vk) = [NeedsCallable vk]
match_rhs_call_deps (RIR.MatchAssignRHS'TupleDestructure1 _ vk) = [NeedsCallable vk]
match_rhs_call_deps (RIR.MatchAssignRHS'TupleDestructure2 _ vk) = [NeedsCallable vk]
match_rhs_call_deps (RIR.MatchAssignRHS'AnonADTVariantField _ vk _) = [NeedsCallable vk]

rewrite_dependencies :: [(RIR.VariableKey, Set Dependency, Set Dependency)] -> Set Dependency -> Set Dependency
rewrite_dependencies variables deps =
    let bindings_false = Map.fromList $ map (\(vk, _, _) -> (vk, False)) variables
    in go bindings_false bindings_false deps
    where
        (variables_here, exec_dependencies, call_dependencies) = (Set.fromList v, Map.fromList (zip v e), Map.fromList (zip v c))
            where
                (v, e, c) = unzip3 variables

        go exec_dependencies_included call_dependencies_included deps =
            let (added_exec_deps, added_call_deps, deps') =
                    deps
                        & Set.toList
                        & map
                            ( \dep ->
                                if Set.member (get_dependency_vk dep) variables_here
                                    then case dep of
                                        NeedsCallable dep_vk ->
                                            if call_dependencies_included Map.! dep_vk
                                                then ([], [], Set.empty)
                                                else ([], [dep_vk], call_dependencies Map.! dep_vk)
                                        NeedsInitialized dep_vk ->
                                            if exec_dependencies_included Map.! dep_vk
                                                then ([], [], Set.empty)
                                                else ([dep_vk], [], exec_dependencies Map.! dep_vk)
                                    else ([], [], Set.singleton dep)
                            )
                        & unzip3
                deps'' = Set.unions deps'

                exec_dependencies_included' = foldl' (\m d -> Map.insert d True m) exec_dependencies_included (concat added_exec_deps)
                call_dependencies_included' = foldl' (\m d -> Map.insert d True m) call_dependencies_included (concat added_call_deps)
            in if null (concat added_exec_deps) && null (concat added_call_deps) then deps'' else go exec_dependencies_included' call_dependencies_included' deps''

sort_bindings :: Arena.Arena RIR.Variable RIR.VariableKey -> [RIR.Binding] -> Either (NonEmpty BindingsHaveLoopError, RIR.Bindings) RIR.Bindings
sort_bindings vars bindings =
    case find_loops bindings of
        [] -> Right $ RIR.Bindings RIR.TopologicallySorted (topological_sort [] bindings)
        a : more ->
            let var_spans =
                    Map.fromList $
                        map
                            ( \(RIR.Binding vk _) ->
                                let (RIR.Variable _ _ var_sp) = Arena.get vars vk
                                in (vk, var_sp)
                            )
                            bindings
            in Left (NonEmpty.map (BindingsHaveLoop var_spans) (a :| more), RIR.Bindings RIR.HasLoops bindings)
    where
        binding_keys_from_bindings = Set.fromList . map (\(RIR.Binding vk _) -> vk)
        bindings_defined_here = binding_keys_from_bindings bindings

        exec_dependencies :: Map.Map RIR.VariableKey (Set Dependency)
        exec_dependencies =
            bindings
                & map
                    ( \(RIR.Binding var_key initializer) ->
                        ( var_key
                        , get_execute_dependencies initializer
                            & Set.filter (\dep -> Set.member (get_dependency_vk dep) bindings_defined_here) -- filter to only dependencies that are defined here because this topological sort cant do anything about dependencies defined elsewhere
                        )
                    )
                & Map.fromList
        call_dependencies :: Map.Map RIR.VariableKey (Set Dependency)
        call_dependencies =
            bindings
                & map
                    ( \(RIR.Binding var_key initializer) ->
                        ( var_key
                        , get_call_dependencies initializer
                            & Set.filter (\dep -> Set.member (get_dependency_vk dep) bindings_defined_here) -- same comment as above
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

        find_loops bindings = trace_from_first (Set.fromList $ map (\(RIR.Binding vk _) -> NeedsInitialized vk) bindings)
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
                        Just index -> pure [reverse $ current : take (index + 1) explored_stack] -- found loop; the returned value contains the loop with its first and last elements as the same dependency
                        _ -> do
                            let deps = case current of
                                    NeedsInitialized depvk -> exec_dependencies Map.! depvk
                                    NeedsCallable depvk -> exec_dependencies Map.! depvk <> call_dependencies Map.! depvk
                            concat <$> mapM (trace (current : explored_stack)) (toList deps)
