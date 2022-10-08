module UHF.IO.Location
    ( Location
    , Span
    , Located (..)

    , new_location
    , new_span
    , eof_span

    , file, ind, row, col, line
    , start, before_end, end

    , join_span
    , is_single_line
    , seek

    , tests
    ) where

import UHF.Util.Prelude


import qualified UHF.IO.File as File

import qualified Data.Text as Text
import Data.List (minimumBy, maximumBy)

-- TODO: test this entire module

data Location = Location { file :: File.File, ind :: Int, row :: Int, col :: Int } deriving (Show, Eq)
data Span = Span { start :: Location, before_end :: Location, end :: Location } deriving (Show, Eq)

data Located a = Located { just_span :: Span, unlocate :: a } deriving (Show, Eq)

line :: Location -> Int
line = row

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

eof_span :: File.File -> Span
eof_span f = new_span (seek (Text.length $ File.contents f) $ new_location f) 0 1

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

instance Format Location where
    format (Location f _ r c) = Text.pack (File.path f) <> ":" <> show r <> ":" <> show c

instance Format Span where
    format (Span (Location f1 _ r1 c1) _ (Location _ _ r2 c2)) = Text.pack (File.path f1) <> ":" <> show r1 <> ":" <> show c1 <> ":" <> show r2 <> ":" <> show c2

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

case_seek_same :: Assertion
case_seek_same =
    let l = new_location $ File.File "a" "abc"
    in l @=? seek 0 l

case_seek_forward_same_line :: Assertion
case_seek_forward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Location f 2 1 3 @=? seek 2 (Location f 0 1 1)
case_seek_forward_up_to_newline :: Assertion
case_seek_forward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 3 1 4 @=? seek 3 (Location f 0 1 1)
case_seek_forward_to_newline :: Assertion
case_seek_forward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 4 1 5 @=? seek 4 (Location f 0 1 1)
case_seek_forward_past_newline :: Assertion
case_seek_forward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 5 2 1 @=? seek 5 (Location f 0 1 1)

case_seek_backward_same_line :: Assertion
case_seek_backward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Location f 6 2 2 @=? seek (-2) (Location f 8 2 4)
case_seek_backward_up_to_newline :: Assertion
case_seek_backward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 5 2 1 @=? seek (-3) (Location f 8 2 4)
case_seek_backward_to_newline :: Assertion
case_seek_backward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 4 1 5 @=? seek (-4) (Location f 8 2 4)
case_seek_backward_past_newline :: Assertion
case_seek_backward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f 3 1 4 @=? seek (-5) (Location f 8 2 4)

tests :: TestTree
tests = $(testGroupGenerator)
