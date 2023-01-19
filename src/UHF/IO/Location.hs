module UHF.IO.Location
    ( LineCol
    , Location
    , Span
    , Located (..)

    , new_location
    , new_span
    , eof_span
    , dummy_span -- TODO: use conditional compilation? to make sure this only compiles in tests
    , dummy_locate -- TODO: use conditional compilation? to make sure this only compiles in tests

    , ind, row, col
    , loc_file, lc
    , loc_ind, loc_row, loc_col
    , sp_file, sp_s, sp_be, sp_e
    , sp_s_ind, sp_be_ind, sp_e_ind
    , sp_s_row, sp_be_row, sp_e_row
    , sp_s_col, sp_be_col, sp_e_col

    , join_span
    , is_single_line
    , seek

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.File as File

import qualified UHF.Diagnostic.Colors as Colors

import qualified Data.Text as Text
import Data.List (minimumBy, maximumBy)

-- TODO: test this entire module

data LineCol = LineCol { ind :: Int, row :: Int, col :: Int } deriving (Show, Eq)
data Location = Location { loc_file :: File.File, lc :: LineCol } deriving (Show, Eq)
data Span = Span File.File LineCol LineCol LineCol deriving (Show, Eq)

data Located a = Located { just_span :: Span, unlocate :: a } deriving (Show, Eq)

loc_ind, loc_row, loc_col :: Location -> Int
loc_ind = ind . lc
loc_row = row . lc
loc_col = col . lc

sp_file :: Span -> File.File
sp_file (Span f _ _ _) = f

sp_s, sp_be, sp_e :: Span -> Location
sp_s (Span f s _ _) = Location f s
sp_be (Span f _ be _) = Location f be
sp_e (Span f _ _ e) = Location f e

sp_s_ind, sp_be_ind, sp_e_ind, sp_s_row, sp_be_row, sp_e_row, sp_s_col, sp_be_col, sp_e_col :: Span -> Int
sp_s_ind = loc_ind . sp_s
sp_be_ind = loc_ind . sp_be
sp_e_ind = loc_ind . sp_e
sp_s_row = loc_row . sp_s
sp_be_row = loc_row . sp_be
sp_e_row = loc_row . sp_e
sp_s_col = loc_col . sp_s
sp_be_col = loc_col . sp_be
sp_e_col = loc_col . sp_e

instance Format Location where
    format (Location f (LineCol _ r c)) = Colored Colors.file_path $ format f <> ":" <> show r <> ":" <> show c

instance Format Span where
    format (Span f (LineCol _ r1 c1) _ (LineCol _ r2 c2)) = format f <> ":" <> show r1 <> ":" <> show c1 <> ":" <> show r2 <> ":" <> show c2

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

new_location :: File.File -> Location
new_location f = Location f (LineCol 0 1 1)

new_span :: Location -> Int -> Int -> Span
new_span loc@(Location file _) start_i len =
    let s_l@(Location _ s_lc) = seek start_i loc
        Location _ b_lc = seek (len - 1) s_l
        Location _ e_lc = seek len s_l
    in Span file s_lc b_lc e_lc

dummy_span :: Span
dummy_span = Span (File.File "<dummy file for dummy span>" "") (LineCol 0 1 1) (LineCol 0 1 1) (LineCol 0 1 1)

dummy_locate :: a -> Located a
dummy_locate = Located dummy_span

eof_span :: File.File -> Span
eof_span f = new_span (seek (Text.length $ File.contents f) $ new_location f) 0 1

join_span :: Span -> Span -> Span
join_span (Span f1 s1 b1 e1) (Span f2 s2 b2 e2) =
    if f1 == f2
        then
            let comparator = compare `on` ind
                minstart = minimumBy comparator [s1, s2]
                maxbefore =  maximumBy comparator [b1, b2]
                maxend = maximumBy comparator [e1, e2]
            in Span f1 minstart maxbefore maxend

        else error "join two spans where some locations have different files"

is_single_line :: Span -> Bool
is_single_line (Span _ s be _) = row s == row be

seek :: Int -> Location -> Location
seek 0 loc = loc
seek n (Location f (LineCol i r c)) =
    let file_contents = File.contents f

        num_nl
            | n < 0 = Text.count "\n" $ Text.take (-n) (Text.drop (i + n) file_contents)
            | otherwise = Text.count "\n" $ Text.take n (Text.drop i file_contents)

        r'
            | n < 0 = r - num_nl
            | otherwise = r + num_nl

        c'
            | num_nl == 0 = c + n
            | otherwise = 1 + Text.length (Text.takeWhile (/='\n') $ Text.reverse $ Text.take (i + n) file_contents)

    in Location f (LineCol (i + n) r' c')

case_seek_same :: Assertion
case_seek_same =
    let l = new_location $ File.File "a" "abc"
    in l @=? seek 0 l

case_seek_forward_same_line :: Assertion
case_seek_forward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 2 1 3) @=? seek 2 (Location f (LineCol 0 1 1))
case_seek_forward_up_to_newline :: Assertion
case_seek_forward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 3 1 4) @=? seek 3 (Location f (LineCol 0 1 1))
case_seek_forward_to_newline :: Assertion
case_seek_forward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 4 1 5) @=? seek 4 (Location f (LineCol 0 1 1))
case_seek_forward_past_newline :: Assertion
case_seek_forward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 5 2 1) @=? seek 5 (Location f (LineCol 0 1 1))

case_seek_backward_same_line :: Assertion
case_seek_backward_same_line =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 6 2 2) @=? seek (-2) (Location f (LineCol 8 2 4))
case_seek_backward_up_to_newline :: Assertion
case_seek_backward_up_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 5 2 1) @=? seek (-3) (Location f (LineCol 8 2 4))
case_seek_backward_to_newline :: Assertion
case_seek_backward_to_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 4 1 5) @=? seek (-4) (Location f (LineCol 8 2 4))
case_seek_backward_past_newline :: Assertion
case_seek_backward_past_newline =
    let f = File.File "a" "abcd\nefgh"
    in Location f (LineCol 3 1 4) @=? seek (-5) (Location f (LineCol 8 2 4))

tests :: TestTree
tests = $(testGroupGenerator)
