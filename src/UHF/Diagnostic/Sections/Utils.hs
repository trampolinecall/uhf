{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Utils where

import UHF.Util.Prelude

import UHF.IO.Location.SpanHelper

import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import UHF.IO.File (File)
import UHF.IO.Location (Span)

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map

newtype FileCmpByPath = FileCmpByPath { un_fcbp :: File }
instance Ord FileCmpByPath where
    compare = compare `on` (File.path . un_fcbp)
instance Eq FileCmpByPath where
    (==) = (==) `on` (File.path . un_fcbp)

group_by_spans :: (a -> Span) -> [a] -> Map FileCmpByPath (Map Int [a])
group_by_spans get_span = Map.map (group_by (Location.sp_s_row . get_span)) . group_by (FileCmpByPath . Location.sp_file . get_span)
    where
        group_by get =
            foldl'
                (\ m thing -> Map.alter (\ old_entry -> Just $ thing : fromMaybe [] old_entry) (get thing) m)
                Map.empty

context_lines :: File -> Int -> [(File, Int)]
context_lines f n = filter (uncurry can_be_context_line) $ map (f,) [n-1..n+1]
    where
        can_be_context_line fl nr = exists fl nr && not_empty fl nr

        exists fl nr =
            nr > 0 && nr <= length (Text.lines $ File.contents fl)

        not_empty fl nr =
            not $ Text.null $ Text.lines (File.contents fl) List.!! (nr - 1)

flnr_comparator :: (File, Int) -> (File, Int) -> Ordering
flnr_comparator (f1, n1) (f2, n2)
    | f1 == f2 = n1 `compare` n2
    | otherwise = EQ

get_quote :: File -> Int -> Text
get_quote fl nr = headDef "" $ drop (nr - 1) $ Text.lines $ File.contents fl

file_and_elipsis_lines :: (File, Int) -> (File, Int) -> [Line.Line]
file_and_elipsis_lines (lastf, lastn) (curf, curn)
    | lastf /= curf = [Line.file_line curf]
    | lastn + 1 /= curn = [Line.elipsis_line]
    | otherwise = []

-- tests {{{1
case_file_and_elipsis_lines :: Assertion
case_file_and_elipsis_lines =
    let (f1, _) = make_spans ["ajfowiejf"]
        (f2, _) = make_spans ["aobjiwoiejfawoeijf"]

    in
    ([Line.file_line f2] @=? file_and_elipsis_lines (f1, 2) (f2, 12)) >>
    ([] @=? file_and_elipsis_lines (f2, 12) (f2, 13)) >>
    ([Line.elipsis_line] @=? file_and_elipsis_lines (f2, 13) (f2, 20))

tests :: TestTree
tests = $(testGroupGenerator)
