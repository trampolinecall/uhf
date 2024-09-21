{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.Parser.Generate
-- TODO: reorder these exports
    ( StateTable (..)
    , Action (..)
    , ActionOrConflict (..)
    , generate_table
    , make_parse_fn
    ) where

import UHF.Prelude

import Control.Arrow (first, second)
import Control.Monad ()
import qualified Control.Monad.State as State
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

-- state table generation {{{1
-- this is an lalr(1) parser generator
data ItemSet = ItemSet Int (Set Item) (Set Item)
data Item = Item Rule Int Terminal deriving (Eq, Ord)

instance Format Item where
    format (Item (Rule _ nt pr) i l) =
        let (before, after) = splitAt i pr
        in format nt <> " -> " <> Text.intercalate " " (map format before <> ["."] <> map format after) <> " {" <> format l <> "}"
instance Format ItemSet where
    format (ItemSet n k c) = "item set " <> show n <> " { " <> Text.intercalate ", " (map format (Set.toList k)) <> ";" <> Text.intercalate "," (map format (Set.toList c)) <> " }"

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

data StateTable = StateTable NTResultTypes ReduceFnMap (Map Int (Map Terminal Action, Map Nonterminal Int))
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

find_sets ::
    Grammar ->
    Map.Map Int (Map.Map Terminal Int, Map.Map Nonterminal Int) ->
    [Int] ->
    State [ItemSet] ([(ItemSet, Map.Map Terminal Int, Map.Map Nonterminal Int)])
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
                        Just ((S'T term)) -> do
                            -- fromJust should not error because there is a terminal after the dot
                            let new_kernel = Set.map (fromJust . move_forward) items
                            let closure = find_closure grammar new_kernel
                            (next_set_number, set_is_new) <- intern new_kernel closure
                            pure (Map.singleton term next_set_number, if set_is_new then [next_set_number] else [])
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
                        Just ((S'NT nt)) -> do
                            -- fromJust should not error because there is a terminal after the dot
                            let new_kernel = Set.map (fromJust . move_forward) items
                            let closure = find_closure grammar new_kernel
                            (next_set_number, set_is_new) <- intern new_kernel closure

                            pure (Map.singleton nt next_set_number, if set_is_new then [next_set_number] else [])
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
            case findIndex
                (\(ItemSet _ f_kernel f_closure) -> Set.map remove_lookaheads (f_kernel <> f_closure) == Set.map remove_lookaheads (kernel <> closure))
                sets of
                Just found_set_index -> ((found_set_index, False), modify_at found_set_index (merge kernel closure) sets)
                Nothing ->
                    let new_item_set = ItemSet (length sets) kernel closure
                    in ((length sets, True), (sets ++ [new_item_set]))
            where
                remove_lookaheads (Item rule ind _) = (rule, ind)

                modify_at :: Int -> (a -> a) -> [a] -> [a]
                modify_at n _ [] = error $ "cannot modify at index " <> show n <> " in list of length 0 (this is a bug in the parser generator)"
                modify_at 0 change (first : more) = change first : more
                modify_at n change l@(first : more)
                    | n >= length l =
                        error $ "cannot modify at index " <> show n <> " in list of length " <> show (length l) <> " (this is a bug in the parser generator)"
                    | otherwise = first : (modify_at (n - 1) change more)

                merge kernel closure (ItemSet number already_kernel already_closure) = ItemSet number (Set.union already_kernel kernel) (Set.union already_closure closure)
find_sets _ tables [] = do
    sets <- get
    pure $ Map.toAscList tables & map (\(index, (shifts, gotos)) -> (sets !! index, shifts, gotos))

convert_to_state_table :: NTResultTypes -> ReduceFnMap -> [(ItemSet, Map Terminal Int, Map Nonterminal Int)] -> StateTable
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

        remove_conflicts :: Map Int (ItemSet, Map Terminal (ActionOrConflict Action), Map Nonterminal Int) -> Map Int (Map Terminal Action, Map Nonterminal Int)
        remove_conflicts states =
            Map.map
                ( \(item_set, action, goto) ->
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
                    , goto
                    )
                )
                states

        format_map m = "{ " <> Text.intercalate ", " (map (\(k, v) -> format k <> ": " <> format v) (Map.toList m)) <> " }"

generate_table :: Grammar -> StateTable
generate_table grammar =
    convert_to_state_table (nt_result_types grammar) (reduce_fn_map grammar) $
        State.evalState
            (find_sets grammar Map.empty [0])
            [new_item_set grammar 0 (Set.singleton $ new_item (augment_rule grammar) (Token.EOF ()))]

-- th function {{{1
from_just_with_message :: String -> Maybe a -> a
from_just_with_message _ (Just a) = a
from_just_with_message msg Nothing = error msg

make_parse_fn :: String -> TH.Q TH.Type -> StateTable -> TH.Q [TH.Dec]
make_parse_fn name res_ty (StateTable nt_ty_map reduce_fn_map table) = do
    let fn_name = TH.mkName name
    top_sig <- TH.SigD fn_name <$> [t|InfList.InfList Token.LToken -> $(res_ty)|]
    top_decl <- do
        goto_table_name <- TH.newName "goto_table"
        goto_table_decl <- [d|$(pure $ TH.VarP goto_table_name) = $(TH.Syntax.lift (Map.map snd table))|]

        sub_name <- TH.newName "parse'"
        sub_decl <- do
            sub_clauses <- mapM (make_function_clause goto_table_name sub_name) (Map.toAscList table) <&> concat
            default_clause <- default_clause
            pure $ TH.FunD sub_name (sub_clauses ++ [default_clause])

        top_expr <- [e|$(pure $ TH.VarE sub_name) [0 :: Int] []|]
        pure $ TH.FunD fn_name [TH.Clause [] (TH.NormalB top_expr) (sub_decl : goto_table_decl)]

    pure [top_sig, top_decl]
    where
        make_function_clause :: TH.Name -> TH.Name -> (Int, (Map Terminal Action, Map Nonterminal Int)) -> TH.Q [TH.Clause]
        make_function_clause goto_table_name recurse_name (state_number, (action_table, _)) =
            action_table
                & Map.toAscList
                & mapM
                    ( \case
                        (lookahead, Shift new_state) -> make_clause state_number lookahead $ \state_stack_name ast_stack_name _ input_cur_tok_name input_more_name ->
                            [|
                                $(pure $ TH.VarE recurse_name)
                                    ($(pure $ TH.LitE $ TH.IntegerL $ toInteger new_state) : $(pure $ TH.VarE state_stack_name))
                                    (Dynamic.toDyn $(pure $ TH.VarE input_cur_tok_name) : $(pure $ TH.VarE ast_stack_name))
                                    $(pure $ TH.VarE input_more_name)
                                |]
                        (lookahead, Reduce (Rule rule_num nt prod)) ->
                            let prod_len = length prod
                            in make_clause state_number lookahead $ \state_stack_name ast_stack_name input_stream_name _ _ ->
                                [|
                                    let state_stack_popped@(last_state : _) = drop $(pure $ TH.LitE $ TH.IntegerL $ toInteger prod_len) $(pure $ TH.VarE state_stack_name)
                                        (asts_popped, ast_stack_popped) = splitAt $(pure $ TH.LitE $ TH.IntegerL $ toInteger prod_len) $(pure $ TH.VarE ast_stack_name)

                                        next_state = $(pure $ TH.VarE goto_table_name) Map.! last_state Map.! $(TH.Syntax.lift nt)

                                        new_ast :: $(nt_ty_map Map.! nt)
                                        new_ast =
                                            $( do
                                                result_ty <- nt_ty_map Map.! nt
                                                raw_reduce_fn <- reduce_fn_map Map.! rule_num
                                                reduce_fn_ty <-
                                                    ( foldrM
                                                            ( \sym t -> do
                                                                st <- sym_ty sym
                                                                pure $ TH.ArrowT `TH.AppT` st `TH.AppT` t
                                                            )
                                                            result_ty
                                                            (prod) ::
                                                            TH.Q TH.Type
                                                        )

                                                foldlM
                                                    ( \e (sym_i, sym) ->
                                                        TH.AppE e
                                                            <$> [|
                                                                let popped = asts_popped !! $(pure $ TH.LitE $ TH.IntegerL sym_i)
                                                                in from_just_with_message
                                                                    ( "invalid cast when popping ast node from stack: expected "
                                                                        ++ show (Typeable.typeRep (Nothing :: Maybe $(sym_ty sym)))
                                                                        ++ " but got "
                                                                        ++ show (Dynamic.dynTypeRep popped)
                                                                    )
                                                                    (Dynamic.fromDynamic popped) ::
                                                                    $(sym_ty sym)
                                                                |]
                                                    )
                                                    (TH.SigE raw_reduce_fn reduce_fn_ty)
                                                    (zip (reverse [0 .. (toInteger prod_len - 1)]) prod)
                                             )
                                    in $(pure $ TH.VarE recurse_name)
                                        (next_state : state_stack_popped)
                                        (Dynamic.toDyn new_ast : ast_stack_popped)
                                        $(pure $ TH.VarE input_stream_name)
                                    |]
                        (lookahead, Accept) -> make_clause state_number lookahead $ \_ ast_stack_name _ _ _ ->
                            [|
                                from_just_with_message "invalid cast when popping ast node from stack in accept action" (Dynamic.fromDynamic $ head $(pure $ TH.VarE ast_stack_name))
                                |]
                    )

        default_clause :: TH.Q TH.Clause
        default_clause = do
            body <- [|error "invalid state in parser / parse error"|] -- TODO: parse errors
            pure $ TH.Clause [TH.WildP, TH.WildP, TH.WildP] (TH.NormalB body) []

        make_clause :: Int -> Terminal -> (TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Name -> TH.Q TH.Exp) -> TH.Q TH.Clause
        make_clause state_number lookahead body = do
            state_stack_name <- TH.newName "state_stack"
            ast_stack_name <- TH.newName "ast_stack"
            input_stream_name <- TH.newName "input"
            input_cur_tok_name <- TH.newName "input_cur"
            input_more_name <- TH.newName "input_more"

            state_stack_pat <- [p|$(pure $ TH.LitP $ TH.IntegerL $ toInteger state_number) : _|]
            input_pat <- [p|$(pure $ TH.VarP input_cur_tok_name) InfList.::: $(pure $ TH.VarP input_more_name)|]
            guard <- [e|Token.is_tt $(TH.Syntax.lift lookahead) (Located.unlocate $(pure $ TH.VarE input_cur_tok_name))|]
            body <- body state_stack_name ast_stack_name input_stream_name input_cur_tok_name input_more_name

            pure $
                TH.Clause
                    [TH.AsP state_stack_name state_stack_pat, TH.VarP ast_stack_name, TH.AsP input_stream_name input_pat]
                    (TH.GuardedB [(TH.NormalG guard, body)])
                    []

        sym_ty (S'T _) = [t|Token.LToken|]
        sym_ty (S'NT nt) = nt_ty_map Map.! nt
