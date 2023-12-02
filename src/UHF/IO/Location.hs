module UHF.IO.Location
    ( LineCol
    , Location

    , loc_file, lc
    , lc_ind, lc_row, lc_col
    , loc_ind, loc_row, loc_col, loc_remaining_in_file

    , new

    , seek_1
    , seek

    , tests
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text

import UHF.IO.File (File)
import qualified UHF.IO.File as File

-- TODO: test this entire module

-- TODO: count column numbers with tabs correctly

data LineCol = LineCol { lc_ind :: !Int, lc_row :: !Int, lc_col :: !Int } deriving (Show, Eq)
data Location = Location { loc_file :: File, lc :: !LineCol, loc_remaining_in_file :: Text } deriving (Show, Eq)

loc_ind, loc_row, loc_col :: Location -> Int
loc_ind = lc_ind . lc
loc_row = lc_row . lc
loc_col = lc_col . lc

instance Format Location where
    format (Location f (LineCol _ r c) _) = format f <> ":" <> format r <> ":" <> format c

new :: File -> Location
new f = Location f (LineCol 0 1 1) (File.contents f)

seek_1 :: Location -> Location
seek_1 (Location f (LineCol i r c) remaining) =
    let is_going_over_nl = (fst <$> Text.uncons remaining) == Just '\n'

        r'
            | is_going_over_nl = r + 1
            | otherwise = r

        c'
            | is_going_over_nl = 1
            | otherwise = c + 1

    in Location f (LineCol (i + 1) r' c') (Text.drop 1 remaining)

seek :: Int -> Location -> Location
seek 0 loc = loc
seek 1 l = seek_1 l
seek n (Location f (LineCol i r c) remaining) =
    let file_contents = File.contents f

        num_nl
            | n < 0 = Text.count "\n" $ Text.take (-n) (Text.drop (i + n) file_contents)
            | otherwise = Text.count "\n" $ Text.take n remaining

        r'
            | n < 0 = r - num_nl
            | otherwise = r + num_nl

        c'
            | num_nl == 0 = c + n
            | otherwise = 1 + Text.length (Text.takeWhileEnd (/='\n') $ Text.take (i + n) file_contents)

        remaining'
            | n > 0 = Text.drop n remaining
            | otherwise = Text.drop (i + n) file_contents

    in Location f (LineCol (i + n) r' c') remaining'

-- tests {{{1
case_seek_1_same_line :: Assertion
case_seek_1_same_line =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 1 1 2) "bcd\nefgh" @=? seek_1 (Location f (LineCol 0 1 1) "abcd\nefgh")
case_seek_1_up_to_newline :: Assertion
case_seek_1_up_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 3 1 4) "d\nefgh" @=? seek_1 (Location f (LineCol 2 1 3) "cd\nefgh")
case_seek_1_to_newline :: Assertion
case_seek_1_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 4 1 5) "\nefgh" @=? seek_1 (Location f (LineCol 3 1 4) "d\nefgh")
case_seek_1_past_newline :: Assertion
case_seek_1_past_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 5 2 1) "efgh" @=? seek_1 (Location f (LineCol 4 1 5) "\nefgh")

case_seek_same :: Assertion
case_seek_same =
    File.new "a" "abc" >>= \ f ->
    let l = new f
    in l @=? seek 0 l

case_seek_forward_same_line :: Assertion
case_seek_forward_same_line =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 2 1 3) "cd\nefgh" @=? seek 2 (Location f (LineCol 0 1 1) "abcd\nefgh")
case_seek_forward_up_to_newline :: Assertion
case_seek_forward_up_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 3 1 4) "d\nefgh" @=? seek 3 (Location f (LineCol 0 1 1) "abcd\nefgh")
case_seek_forward_to_newline :: Assertion
case_seek_forward_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 4 1 5) "\nefgh" @=? seek 4 (Location f (LineCol 0 1 1) "abcd\nefgh")
case_seek_forward_past_newline :: Assertion
case_seek_forward_past_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 5 2 1) "efgh" @=? seek 5 (Location f (LineCol 0 1 1) "abcd\nefgh")

case_seek_backward_same_line :: Assertion
case_seek_backward_same_line =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 6 2 2) "fgh" @=? seek (-2) (Location f (LineCol 8 2 4) "abcd\nefgh")
case_seek_backward_up_to_newline :: Assertion
case_seek_backward_up_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 5 2 1) "efgh" @=? seek (-3) (Location f (LineCol 8 2 4) "abcd\nefgh")
case_seek_backward_to_newline :: Assertion
case_seek_backward_to_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 4 1 5) "\nefgh" @=? seek (-4) (Location f (LineCol 8 2 4) "abcd\nefgh")
case_seek_backward_past_newline :: Assertion
case_seek_backward_past_newline =
    File.new "a" "abcd\nefgh" >>= \ f ->
    Location f (LineCol 3 1 4) "d\nefgh" @=? seek (-5) (Location f (LineCol 8 2 4) "abcd\nefgh")

tests :: TestTree
tests = $(testGroupGenerator)
