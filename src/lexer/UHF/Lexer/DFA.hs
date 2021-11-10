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

data DFA r = DFA [State r] (Maybe Char -> Transition)
data State r = State { state_transition :: (Maybe Char -> Transition), state_res_mod :: (r -> Maybe Char -> r) }
data Transition = StateNum Int | Reject | Accept

data CurState = Begin | CurStateNum Int

run_dfa :: DFA r -> Text.Text -> r -> Maybe (Int, r)
run_dfa dfa = run_dfa' dfa Begin 0

run_dfa' :: DFA r -> CurState -> Int -> Text.Text -> r -> Maybe (Int, r)
run_dfa' dfa@(DFA states begin_transition) cur_state cur_len_consumed input res =
    let cur_transition = case cur_state of
            Begin -> begin_transition
            CurStateNum i -> state_transition $ states !! i

        res_mod =  case cur_state of
            Begin -> const
            CurStateNum i -> state_res_mod $ states !! i

        cur_char = fst <$> Text.uncons input

        transition = cur_transition cur_char

    in case transition of
        Reject -> Nothing
        Accept -> Just (cur_len_consumed, res)

        StateNum next_state ->
            let (next_len_consumed, next_input) =
                    case Text.uncons input of
                        Just (_, more) -> (cur_len_consumed + 1, more)
                        Nothing -> (cur_len_consumed, input)

                next_res = res_mod res cur_char
            in run_dfa' dfa (CurStateNum next_state) next_len_consumed next_input next_res

tests :: Test
tests = test
    [ "run_dfa" ~:
        let dfa = DFA
              [ State
                  (\case
                      Just 'a' -> StateNum 0
                      Just 'b' -> StateNum 1
                      _ -> Reject
                  )
                  (\ _ _ -> 2)
              , State
                  (\case
                      Just 'b' -> StateNum 1
                      Nothing -> Accept
                      _ -> Reject
                  )
                  (\ _ _ -> 3)
              ]
              (\ c -> if c == Just 'a' then StateNum 0 else Reject)
        in
            [ Just (6, 3 :: Int) ~=? run_dfa dfa "aaaabb" 0
            , Nothing ~=? run_dfa dfa "" 0
            , Nothing ~=? run_dfa dfa "aa" 0
            , Nothing ~=? run_dfa dfa "aba" 0
            ]
    ]
