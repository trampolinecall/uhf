{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UHF.Lexer.DFA
    ( DFA(..)
    , State(..)
    , Transition(..)
    , run_dfa

    , tests
    ) where

import Test.HUnit (Test, test, (~:), (~=?))

import qualified Data.Text as Text

data DFA r = DFA [State r] (r -> Maybe Char -> Transition r)
data State r = State { state_transition :: (r -> Maybe Char -> Transition r) }
data Transition r = StateNum Int r | Accept r | Reject

data CurState = Begin | CurStateNum Int

run_dfa :: DFA r -> Text.Text -> r -> Maybe (Int, r)
run_dfa dfa = run_dfa' dfa Begin 0

run_dfa' :: DFA r -> CurState -> Int -> Text.Text -> r -> Maybe (Int, r)
run_dfa' dfa@(DFA states begin_transition) cur_state cur_len_consumed input cur_res =
    let cur_transition = case cur_state of
            Begin -> begin_transition
            CurStateNum i -> state_transition $ states !! i

        cur_char = fst <$> Text.uncons input

    in case cur_transition cur_res cur_char of
        Reject -> Nothing
        Accept res -> Just (cur_len_consumed, res)

        StateNum next_state next_res ->
            let (next_len_consumed, next_input) =
                    case Text.uncons input of
                        Just (_, more) -> (cur_len_consumed + 1, more)
                        Nothing -> (cur_len_consumed, input)

            in run_dfa' dfa (CurStateNum next_state) next_len_consumed next_input next_res

tests :: Test
tests = test
    [ "run_dfa" ~:
        let dfa = DFA
              [ State
                  (\ r -> \case
                      Just 'a' -> StateNum 0 (r + 1)
                      Just 'b' -> StateNum 1 r
                      _ -> Reject
                  )
              , State
                  (\ r -> \case
                      Just 'b' -> StateNum 1 r
                      Nothing -> Accept r
                      _ -> Reject
                  )
              ]
              (\ r c -> if c == Just 'a' then StateNum 0 r else Reject)
        in
            [ Just (4, 3 :: Int) ~=? run_dfa dfa "aaaabb" 0
            , Nothing ~=? run_dfa dfa "" 0
            , Nothing ~=? run_dfa dfa "aa" 0
            , Nothing ~=? run_dfa dfa "aba" 0
            ]
    ]
