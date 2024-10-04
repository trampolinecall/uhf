{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.Parser.Generate
    ( StateTable (..)
    , State (..)
    , Action (..)
    , generate_table
    , make_parse_fn
    ) where

import UHF.Prelude hiding (State)

import Control.Arrow (first, second)
import Control.Monad ()
import qualified Control.Monad.State as StateMonad
import qualified Data.Data as Data
import qualified Data.Dynamic as Dynamic
import qualified Data.InfList as InfList
import Data.List (findIndex, (!!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.String (String)
import qualified Data.Text as Text
import Data.Typeable (cast)
import qualified Data.Typeable as Typeable
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH.Syntax
import qualified Safe

import qualified UHF.Data.Token as Token
import qualified UHF.Parts.Parser.Error as Error
import UHF.Parts.Parser.Grammar
import qualified UHF.Source.Located as Located

-- first sets {{{1
type FirstSet = Set TerminalOrEpsilon
type FirstSets = Map Nonterminal FirstSet

data TerminalOrEpsilon
    = ToE'Terminal Terminal
    | ToE'Epsilon
    deriving (Eq, Ord)

find_firsts :: Grammar -> FirstSets
find_firsts (Grammar _ rules _ _) = repeat_until_unchanging add_firsts Map.empty
    where
        add_firsts firsts =
            rules
                & map (\((Rule _ nt production)) -> Map.singleton nt (firsts_of_sequence firsts production))
                & Map.unionsWith (<>)
                & Map.unionWith (<>) firsts

firsts_of_sequence :: FirstSets -> [Symbol] -> FirstSet
firsts_of_sequence _ [] = Set.singleton ToE'Epsilon
firsts_of_sequence firsts sequence =
    let sequence_firsts =
            sequence
                & map
                    ( \case
                        (S'NT nt) -> Map.findWithDefault Set.empty nt firsts
                        (S'T t) -> Set.singleton $ ToE'Terminal t
                    )

        (has_epsilon_prefix, after) = span (Set.member ToE'Epsilon) sequence_firsts
    in (Set.unions has_epsilon_prefix)
        -- include the first symbol that does not have epsilon in its first set.
        -- if it does not exist, that means that all of the symbols in the sequence have epsilon in their first set, so the overall first set of the
        -- sequence has to also include epsilon.
        <> (if null after then Set.singleton ToE'Epsilon else head after)

repeat_until_unchanging :: Eq a => (a -> a) -> a -> a
repeat_until_unchanging change initial =
    let next = change initial
    in if initial == next then next else repeat_until_unchanging change next

-- items and item set things {{{1
data ItemSet = ItemSet Int (Set Item) (Set Item)
data Item = Item Rule Int Terminal deriving (Eq, Ord)

instance Format Item where
    format (Item (Rule _ nt pr) i l) =
        let (before, after) = splitAt i pr
        in format nt <> " -> " <> Text.intercalate " " (map format before <> ["."] <> map format after) <> " {" <> format l <> "}"
instance Format ItemSet where
    format (ItemSet n k c) =
        "item set "
            <> show n
            <> " { "
            <> Text.intercalate ", " (map format (Set.toList k))
            <> " ; "
            <> Text.intercalate "," (map format (Set.toList c))
            <> " }"

new_item :: Rule -> Terminal -> Item
new_item r l = Item r 0 l

new_item_set :: Grammar -> Int -> Set Item -> ItemSet
new_item_set grammar number kernel = ItemSet number kernel (find_closure grammar kernel)

move_forward :: Item -> Maybe Item
move_forward (Item r@((Rule _ _ production)) i l)
    | i < length production = Just (Item r (i + 1) l)
    | otherwise = Nothing

symbol_after_dot :: Item -> Maybe Symbol
symbol_after_dot (Item ((Rule _ _ production)) index _) = Safe.atMay production index

all_items :: ItemSet -> Set Item
all_items (ItemSet _ k c) = k <> c

find_closure :: Grammar -> Set Item -> Set Item
find_closure grammar kernel = go Set.empty (Set.toList kernel)
    where
        go current_closure [] = current_closure
        go current_closure (current_item@(Item ((Rule _ _ production)) index current_item_lookahead) : more) =
            case symbol_after_dot current_item of
                Just ((S'NT nt_after_dot)) ->
                    let next_symbols = drop (index + 1) production
                        lookaheads =
                            firsts_of_sequence first_sets next_symbols
                                & Set.map
                                    ( \case
                                        ToE'Terminal t -> t
                                        ToE'Epsilon -> current_item_lookahead
                                    )
                        to_add_to_closure = (Set.fromList $ filter_rules_with_nt nt_after_dot grammar) & Set.map (\r -> Set.map (\l -> new_item r l) lookaheads) & Set.unions
                    in go
                        (current_closure <> to_add_to_closure)
                        (more ++ filter (\i -> not (Set.member i kernel) && not (Set.member i current_closure)) (Set.toList to_add_to_closure))
                _ -> go current_closure more

        first_sets = find_firsts grammar

-- state table definition {{{1
data StateTable = StateTable NTResultTypes ReduceFnMap (Map Int State)
data State = State Int ItemSet (Map Terminal Action) (Map Nonterminal Int)
data Action = Shift Int | Reduce Rule | Accept deriving Show
instance Format Action where
    format (Shift n) = "s" <> format n
    format (Reduce (Rule n _ _)) = "r" <> format n
    format Accept = "acc"

data ActionOrConflict t = SingleAction t | Conflict [t] deriving Functor
instance Semigroup (ActionOrConflict a) where
    (SingleAction a) <> (SingleAction b) = Conflict ([a, b])
    (SingleAction as) <> (Conflict bs) = Conflict (as : bs)
    (Conflict as) <> (SingleAction bs) = Conflict (bs : as)
    (Conflict as) <> (Conflict bs) = Conflict (as ++ bs)

instance Format a => Format (ActionOrConflict a) where
    format (SingleAction a) = format a
    format (Conflict as) = Text.intercalate "/" (map format as)

-- state table generation {{{1
type TerminalTransitions = Map.Map Terminal Int
type NonterminalTransitions = Map.Map Nonterminal Int
type Transitions = (TerminalTransitions, NonterminalTransitions)
type SetWithTransitions = (ItemSet, TerminalTransitions, NonterminalTransitions)
find_sets ::
    Grammar ->
    Map.Map Int Transitions ->
    [Int] ->
    StateMonad.State [ItemSet] [SetWithTransitions]
find_sets grammar item_set_tables (current_set_number : to_process) = do
    current_set <- (!! current_set_number) <$> get

    let symbols_after_dots =
            all_items current_set
                & Set.map (\item -> Map.singleton (symbol_after_dot item) (Set.singleton item))
                & Map.unionsWith (<>)

    (shifts, new_sets_from_shifts) <-
        symbols_after_dots
            & Map.toList
            & mapM
                ( \(symbol_after_dot, items) ->
                    case symbol_after_dot of
                        Just (S'T term) -> do
                            -- fromJust should not error because there is a terminal after the dot
                            let new_kernel = Set.map (fromJust . move_forward) items
                            let closure = find_closure grammar new_kernel
                            (next_set_number, set_needs_tables) <- intern new_kernel closure
                            pure (Map.singleton term next_set_number, if set_needs_tables then [next_set_number] else [])
                        _ -> pure (Map.empty, [])
                )
            & fmap unzip
            & fmap (first (Map.unionsWith (\_ _ -> error "unreachable state: multiple shift actions in state")))
            & fmap (second concat)

    (gotos, new_sets_from_gotos) <-
        symbols_after_dots
            & Map.toList
            & mapM
                ( \(symbol_after_dot, items) ->
                    case symbol_after_dot of
                        Just (S'NT nt) -> do
                            -- fromJust should not error because there is a terminal after the dot
                            let new_kernel = Set.map (fromJust . move_forward) items
                            let closure = find_closure grammar new_kernel
                            (next_set_number, set_needs_tables) <- intern new_kernel closure
                            pure (Map.singleton nt next_set_number, if set_needs_tables then [next_set_number] else [])
                        _ -> pure (Map.empty, [])
                )
            & fmap unzip
            & fmap (first (Map.unionsWith (\_ _ -> error "unreachable state: multiple goto table entries in state")))
            & fmap (second concat)

    find_sets
        grammar
        (Map.insert current_set_number (shifts, gotos) item_set_tables)
        (to_process ++ new_sets_from_shifts ++ new_sets_from_gotos)
    where
        intern kernel closure = state $ \sets ->
            case findIndex (lalr_intern_check_equal kernel closure) sets of
                Just found_set_index ->
                    let set_original = sets !! found_set_index
                        set_merged = merge kernel closure set_original
                        set_modified = all_items set_original /= all_items set_merged
                    in -- if the set was modified, we return True for set_needs_tables so that we recompute the shift and goto tables for that state
                       ((found_set_index, set_modified), replace_at found_set_index set_merged sets)
                Nothing ->
                    let new_item_set = ItemSet (length sets) kernel closure
                    in ((length sets, True), (sets ++ [new_item_set]))
            where
                remove_lookaheads (Item rule ind _) = (rule, ind)

                lalr_intern_check_equal kernel closure (ItemSet _ f_kernel f_closure) = Set.map remove_lookaheads (f_kernel <> f_closure) == Set.map remove_lookaheads (kernel <> closure)
                -- lr1_intern_check_equal kernel closure (ItemSet _ f_kernel f_closure) = (f_kernel <> f_closure) == (kernel <> closure)

                replace_at :: Int -> a -> [a] -> [a]
                replace_at n _ [] = error $ "cannot modify at index " <> show n <> " in list of length 0 (this is a bug in the parser generator)"
                replace_at 0 new (_ : more) = new : more
                replace_at n new l@(first : more)
                    | n >= length l =
                        error $ "cannot modify at index " <> show n <> " in list of length " <> show (length l) <> " (this is a bug in the parser generator)"
                    | otherwise = first : (replace_at (n - 1) new more)

                merge kernel closure (ItemSet number already_kernel already_closure) = ItemSet number (Set.union already_kernel kernel) (Set.union already_closure closure)
find_sets _ tables [] = do
    sets <- get
    pure $ Map.toAscList tables & map (\(index, (shifts, gotos)) -> (sets !! index, shifts, gotos))

convert_to_state_table :: NTResultTypes -> ReduceFnMap -> [SetWithTransitions] -> StateTable
convert_to_state_table nt_result_types reduce_fn_map item_sets =
    item_sets
        & map to_state
        & foldl'
            ( \state_table (state_number, tables) -> case Map.lookup state_number state_table of
                Just _ -> error "unreachable state: duplicate state in lr parser generation"
                Nothing -> Map.insert state_number tables state_table
            )
            Map.empty
        & remove_conflicts
        & StateTable nt_result_types reduce_fn_map
    where
        to_state :: (ItemSet, Map Terminal Int, Map Nonterminal Int) -> (Int, (ItemSet, Map Terminal (ActionOrConflict Action), Map Nonterminal Int))
        to_state (item_set@(ItemSet number _ _), shift_table, goto_table) =
            (number, (item_set, Map.unionWith (<>) (Map.map (SingleAction . Shift) shift_table) reduce_actions, goto_table))
            where
                reduce_actions =
                    all_items item_set
                        & Set.toList
                        & filter (isNothing . symbol_after_dot)
                        & map
                            ( ( \(Item rule@((Rule _ r_nt _)) _ lookahead) ->
                                    if cast r_nt == Just Augment
                                        then Map.singleton lookahead (SingleAction Accept)
                                        else Map.singleton lookahead (SingleAction $ Reduce rule)
                              )
                            )
                        & Map.unionsWith (<>)

        remove_conflicts ::
            Map Int (ItemSet, Map Terminal (ActionOrConflict Action), Map Nonterminal Int) -> Map Int State
        remove_conflicts states =
            Map.map
                ( \(item_set@(ItemSet set_number _ _), action, goto) ->
                    State
                        set_number
                        item_set
                        ( action
                            & Map.map
                                ( \case
                                    SingleAction a -> a
                                    as@(Conflict _) ->
                                        error $
                                            convert_str $
                                                "conflict in parsing table: "
                                                    <> format as
                                                    <> "\nitem set: "
                                                    <> format item_set
                                                    <> "\nstates:\n"
                                                    <> Text.intercalate
                                                        "\n"
                                                        ( map
                                                            (\(n, (item_set, actions, gotos)) -> show n <> " | " <> format item_set <> " | " <> format_map actions <> " " <> format_map gotos)
                                                            (Map.toList states)
                                                        )
                                )
                        )
                        goto
                )
                states

        format_map m = "{ " <> Text.intercalate ", " (map (\(k, v) -> format k <> ": " <> format v) (Map.toList m)) <> " }"

generate_table :: Grammar -> StateTable
generate_table grammar =
    convert_to_state_table (nt_result_types grammar) (reduce_fn_map grammar) $
        StateMonad.evalState
            (find_sets grammar Map.empty [0])
            [new_item_set grammar 0 (Set.singleton $ new_item (augment_rule grammar) Token.TT'EOF)]

-- th function {{{1
type ReduceFunction = [Int] -> [Dynamic.Dynamic] -> ([Int], [Dynamic.Dynamic])
data ParserAction = PA'Shift Int | PA'Reduce ReduceFunction | PA'Accept

from_just_with_message :: String -> Maybe a -> a
from_just_with_message _ (Just a) = a
from_just_with_message msg Nothing = error msg

force_cast :: forall a. Typeable.Typeable a => String -> Dynamic.Dynamic -> a
force_cast when a = case Dynamic.fromDynamic a of
    Just a -> a
    Nothing -> error $ "invalid cast " <> when <> ": expected " <> show (Typeable.typeRep (Nothing :: Maybe a)) <> " but got " <> show (Dynamic.dynTypeRep a)

pop_from_stack :: Int -> [Int] -> [Dynamic.Dynamic] -> (Int, [Int], [Dynamic.Dynamic], [Dynamic.Dynamic])
pop_from_stack n state_stack ast_stack =
    let state_stack_popped = drop n state_stack
        last_state = if null state_stack_popped then error "empty tate stack during parser (this is a bug in the parser)" else head state_stack_popped
        (asts_popped, ast_stack_after_popping) = splitAt n ast_stack
    in (last_state, state_stack_popped, asts_popped, ast_stack_after_popping)

parse_loop ::
    Typeable.Typeable a =>
    (Token.LToken -> Int -> Token.TokenType -> Either Error.Error ParserAction) ->
    [Int] ->
    [Dynamic.Dynamic] ->
    InfList.InfList Token.LToken ->
    Either Error.Error a
parse_loop action_table = helper
    where
        helper state_stack@(current_state : _) ast_stack tokens@(current_token InfList.::: more_tokens) =
            case action_table current_token current_state (Token.to_token_type $ Located.unlocate current_token) of
                Right (PA'Shift n) -> helper (n : state_stack) (Dynamic.toDyn current_token : ast_stack) more_tokens
                Right (PA'Reduce reduce) ->
                    let (next_state_stack, next_ast_stack) = reduce state_stack ast_stack
                    in helper next_state_stack next_ast_stack tokens
                Right PA'Accept -> Right $ force_cast "when popping last ast node from stack in accept action" (head ast_stack)
                Left e -> Left e
        helper [] _ _ = error "empty state stack while parsing (this is a bug in the parser)"

make_parse_fn :: String -> TH.Q TH.Type -> StateTable -> TH.Q [TH.Dec]
make_parse_fn name res_ty (StateTable nt_ty_map reduce_fn_map table) = do
    goto_table_name <- TH.newName "goto_table"
    goto_table_decl <- goto_table_function goto_table_name

    (reduce_fn_names, reduce_fn_decls) <-
        runWriterT $
            sequence $
                Map.mapWithKey
                    ( \(Rule rule_num nt prod) body -> do
                        body <- lift body

                        -- create_ast function
                        -- ie. something like
                        --     rule32_create_ast :: A -> B -> C -> AST
                        --     rule32_create_ast = [\ a b c -> AST a b c]
                        -- where the part in brackets is spliced in from the grammar
                        create_ast_name <- lift $ TH.newName $ "rule" ++ show rule_num ++ "_create_ast"
                        do
                            nt_result_ty <- lift $ nt_ty_map Map.! nt
                            create_ast_ty <- foldrM (\sym t -> lift [t|$(sym_ty sym) -> $(pure t)|]) nt_result_ty prod

                            tell [TH.SigD create_ast_name create_ast_ty, TH.FunD create_ast_name [TH.Clause [] (TH.NormalB body) []]]

                        -- reduce_rule function
                        -- ie. something like
                        --     rule32_reduce :: [Int] -> [Dynamic] -> ([Int], [Dynamic])
                        --     rule32_reduce state_stack ast_stack =
                        --         let (last_state, state_stack_popped, asts_popped, ast_stack_popped) = pop_from_stack 3 state_stack ast_stack
                        --
                        --             next_state = goto_table Map.! last_state Map.! (Nonterminal "ast")
                        --             new_ast = rule32_create_ast (fromJust $ dynCast $ asts_popped !! 2) (fromJust $ dynCast $ asts_popped !! 1) (fromJust $ dynCast $ asts_popped !! 0)
                        --         in (next_state : state_stack_popped, new_ast : ast_stack_popped)
                        -- where any Dynamic values are the ast nodes currently on the stack
                        reduce_rule_name <- lift $ TH.newName $ "rule" ++ show rule_num ++ "_reduce"
                        do
                            reduce_rule_type <- lift [t|ReduceFunction|]

                            state_stack_name <- lift $ TH.newName "state_stack"
                            ast_stack_name <- lift $ TH.newName "ast_stack"
                            reduce_rule_dec <- do
                                let prod_len = length prod
                                lift $
                                    TH.funD
                                        reduce_rule_name
                                        [ TH.clause
                                            [TH.varP state_stack_name, TH.varP ast_stack_name]
                                            ( TH.normalB
                                                [|
                                                    let (last_state, state_stack_popped, asts_popped, ast_stack_popped) = pop_from_stack $(TH.litE $ TH.IntegerL $ toInteger prod_len) $(TH.varE state_stack_name) $(TH.varE ast_stack_name)

                                                        next_state = $(TH.varE goto_table_name) last_state $(TH.Syntax.lift $ Text.unpack $ stringify_nt nt)
                                                        new_ast =
                                                            $( foldlM
                                                                ( \e (sym_i, _) ->
                                                                    TH.AppE e <$> [|force_cast "popping ast node from stack to reduce" (asts_popped !! $(TH.litE $ TH.IntegerL sym_i))|]
                                                                )
                                                                (TH.VarE create_ast_name)
                                                                (zip (reverse [0 .. (toInteger prod_len - 1)]) prod)
                                                             )
                                                    in (next_state : state_stack_popped, Dynamic.toDyn new_ast : ast_stack_popped)
                                                    |]
                                            )
                                            []
                                        ]

                            tell [TH.SigD reduce_rule_name reduce_rule_type, reduce_rule_dec]

                        pure reduce_rule_name
                    )
                    reduce_fn_map

    action_table_name <- TH.newName "action_table"
    action_table_decl <- action_table_function reduce_fn_names action_table_name

    let fn_name = TH.mkName name
    top_sig <- TH.SigD fn_name <$> [t|InfList.InfList Token.LToken -> Either Error.Error $(res_ty)|]
    top_decl <- TH.funD fn_name [TH.clause [] (TH.normalB [|parse_loop $(TH.varE action_table_name) [0 :: Int] []|]) []]

    pure $ reduce_fn_decls ++ goto_table_decl ++ action_table_decl ++ [top_sig, top_decl]
    where
        map_flatten_map :: (k1 -> k2 -> v -> r) -> Map k1 (Map k2 v) -> [r]
        map_flatten_map f m = concatMap (\(k1, submap) -> map (uncurry $ f k1) (Map.toAscList submap)) (Map.toAscList m)

        goto_table_function :: TH.Name -> TH.Q [TH.Dec]
        goto_table_function name = do
            let goto_table = Map.map (\(State _ _ _ g) -> g) table

            sig <- TH.sigD name [t|Int -> String -> Int|]
            decl <-
                TH.funD
                    name
                    ( map_flatten_map
                        ( \state nt goto ->
                            TH.clause
                                [TH.litP $ TH.IntegerL $ toInteger state, TH.litP $ TH.StringL $ Text.unpack $ stringify_nt nt]
                                (TH.normalB $ TH.litE $ TH.IntegerL $ toInteger goto)
                                []
                        )
                        goto_table
                    )

            pure [sig, decl]

        action_table_function :: Map Rule TH.Name -> TH.Name -> TH.Q [TH.Dec]
        action_table_function reduce_fn_names name = do
            let action_table = Map.map (\(State _ _ a _) -> a) table

            sig <- TH.sigD name [t|Token.LToken -> Int -> Token.TokenType -> Either Error.Error ParserAction|]
            decl <-
                TH.funD
                    name
                    ( Map.toAscList action_table
                        & concatMap
                            ( \(state, actions) -> do
                                let state_pattern = TH.litP $ TH.IntegerL $ toInteger state

                                let default_clause = do
                                        tok_name <- TH.newName "tok"
                                        TH.clause
                                            [TH.varP tok_name, state_pattern, [p|_|]]
                                            (TH.normalB [|Left (Error.BadToken $(TH.litE $ TH.IntegerL $ toInteger state) $(TH.Syntax.lift $ Map.keys actions) $(TH.varE tok_name))|])
                                            []

                                actions
                                    & Map.toAscList
                                    & map
                                        ( \(terminal, action) -> do
                                            let terminal_ctor_name_str = "Token.TT'" <> term_name terminal
                                            terminal_ctor_name <-
                                                from_just_with_message ("constructor name not in scope: " <> terminal_ctor_name_str) <$> TH.lookupValueName terminal_ctor_name_str

                                            let action' = case action of
                                                    Shift n -> [|Right (PA'Shift n)|]
                                                    Reduce r -> [|Right (PA'Reduce $(TH.varE $ reduce_fn_names Map.! r))|]
                                                    Accept -> [|Right (PA'Accept)|]

                                            TH.clause [[p|_|], state_pattern, TH.conP terminal_ctor_name []] (TH.normalB action') []
                                        )
                                    & (++ [default_clause])
                            )
                    )

            pure [sig, decl]

        term_name t = drop 3 $ show $ Data.toConstr t -- drop 3 to drop "TT'"
        term_ty t =
            let type_name = "Token." ++ term_name t
            in [t|Located.Located $(TH.ConT <$> from_just_with_message ("type '" <> type_name <> "' not in scope") <$> TH.lookupTypeName type_name)|]

        sym_ty (S'T t) = term_ty t
        sym_ty (S'NT nt) = nt_ty_map Map.! nt

        stringify_nt :: Nonterminal -> Text
        stringify_nt (Nonterminal nt) = "N" <> nt
        stringify_nt Augment = "AUGMENT"
