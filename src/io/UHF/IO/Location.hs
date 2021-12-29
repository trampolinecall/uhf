{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UHF.IO.Location
    ( Location
    , Span
    , Located (..)

    , new_location
    , new_span

    , file, ind, row, col, line
    , start, before_end, end

    , join_span
    , fmt_location, fmt_span, fmt_located
    , is_single_line
    , seek

    , tests
    ) where

import Test.Tasty.HUnit ((@=?), Assertion, testCase)
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty (TestTree)

import qualified UHF.IO.File as File

import qualified Data.Text as Text
import Data.List (minimumBy, maximumBy)
import Data.Function (on)
import qualified Safe

-- TODO: test this entire module

data Location = Location { file :: File.File, ind :: Int, row :: Int, col :: Int } deriving Eq
-- TODO: make span constructor function that does not allow locations to be in different files
data Span = Span { start :: Location, before_end :: Location, end :: Location } deriving (Show, Eq)

data Located a = Located { just_span :: Span, unlocate :: a } deriving (Show, Eq)

line :: Location -> Int
line = row

instance Show Location where
    show (Location f i r c) =
        let snippet = Text.drop (i - 2) $ File.contents f

            -- TODO: this does not return the correct results when snippet is not 5 chars long
            before = Text.take 2 snippet
            after = Text.take 2 $ Text.drop 3 snippet
            ch = Text.take 1 $ Text.drop 2 snippet

            before' = Safe.initDef "" $ Safe.tailDef "" $ show before
            after' = Safe.initDef "" $ Safe.tailDef "" $ show after
            ch' = Safe.initDef "" $ Safe.tailDef "" $ show ch

        in File.path f ++ ":" ++ show r ++ ":" ++ show c ++ ": " ++ "\"" ++ before' ++ "'" ++ ch' ++ "'" ++ after' ++ "\""

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

new_location :: File.File -> Location
new_location f = Location f 0 1 1

new_span :: Location -> Int -> Int -> Span
new_span loc start_i len =
    let start_l = seek start_i loc
        before_end_l = seek (len - 1) start_l
        end_l = seek len start_l
    in Span start_l before_end_l end_l

join_span :: Span -> Span -> Span
join_span (Span s1 b1 e1) (Span s2 b2 e2) =
    if all ((file s1 ==) . file) [s1, b1, e1, s2, b2, e2]
        then
            let comparator = compare `on` ind
                minstart = minimumBy comparator [s1, s2]
                maxbefore =  maximumBy comparator [b1, b2]
                maxend = maximumBy comparator [e1, e2]
            in Span minstart maxbefore maxend

        else error "join two spans where some locations have different files"

fmt_location :: Location -> String
fmt_location (Location f _ r c) = File.path f ++ ":" ++ show r ++ ":" ++ show c

fmt_span :: Span -> String
fmt_span (Span (Location f1 _ r1 c1) _ (Location _ _ r2 c2)) = File.path f1 ++ ":" ++ show r1 ++ ":" ++ show c1 ++ ":" ++ show r2 ++ ":" ++ show c2

fmt_located :: Show a => Located a -> String
fmt_located (Located sp a) = "<at " ++ fmt_span sp ++ "> " ++ show a

is_single_line :: Span -> Bool
is_single_line (Span s be _) = row s == row be

seek :: Int -> Location -> Location
seek 0 loc = loc
seek n loc =
    let file_contents = File.contents $ file loc

        num_nl
            | n < 0 = Text.count "\n" $ Text.take (-n) (Text.drop (ind loc + n) file_contents)
            | otherwise = Text.count "\n" $ Text.take n (Text.drop (ind loc) file_contents)

        row'
            | n < 0 = row loc - num_nl
            | otherwise = row loc + num_nl

        col'
            | num_nl == 0 = col loc + n
            | otherwise = 1 + Text.length (Text.takeWhile (/='\n') $ Text.reverse $ Text.take (ind loc + n) file_contents)

    in Location (file loc) (ind loc + n) row' col'

{- TODO: adapt lexer tests
case_seek_same :: Assertion
case_seek_same =
    let l = new_lexer $ File.File "a" "abc"
    in l @=? seek l 0

case_seek_forward_same_line :: Assertion
case_seek_forward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 2 1 3 [] @=? seek (Lexer f 0 1 1 []) 2
case_seek_forward_up_to_newline :: Assertion
case_seek_forward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 3 1 4 [] @=? seek (Lexer f 0 1 1 []) 3
case_seek_forward_to_newline :: Assertion
case_seek_forward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 4 1 5 [] @=? seek (Lexer f 0 1 1 []) 4
case_seek_forward_past_newline :: Assertion
case_seek_forward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 5 2 1 [] @=? seek (Lexer f 0 1 1 []) 5

case_seek_backward_same_line :: Assertion
case_seek_backward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 6 2 2 [] @=? seek (Lexer f 8 2 4 []) (-2)
case_seek_backward_up_to_newline :: Assertion
case_seek_backward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 5 2 1 [] @=? seek (Lexer f 8 2 4 []) (-3)
case_seek_backward_to_newline :: Assertion
case_seek_backward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 4 1 5 [] @=? seek (Lexer f 8 2 4 []) (-4)
case_seek_backward_past_newline :: Assertion
case_seek_backward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Lexer f 3 1 4 [] @=? seek (Lexer f 8 2 4 []) (-5)
-}

tests :: TestTree
tests = $(testGroupGenerator)
