{-# LANGUAGE TemplateHaskell #-}

module UHF.Diagnostic.Styles.Default.Utils where

import UHF.Prelude

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text

import UHF.Source.File (File)
import UHF.Source.Location.SpanHelper
import UHF.Source.Span (Span)
import qualified UHF.Diagnostic.Styles.Default.Line as Line
import qualified UHF.Diagnostic.Styles.Default.Options as Options
import qualified UHF.Source.File as File
import qualified UHF.Source.Span as Span

newtype FileCmpByPath = FileCmpByPath { un_fcbp :: File } deriving (Show)
instance Ord FileCmpByPath where
    compare = comparing (File.path . un_fcbp)
instance Eq FileCmpByPath where
    (==) = (==) `on` (File.path . un_fcbp)

group_by_spans :: (a -> Span) -> [a] -> Map FileCmpByPath (Map Int [a])
group_by_spans get_span = Map.map (group_by (Span.start_row . get_span)) . group_by (FileCmpByPath . Span.file . get_span)
    where
        group_by get =
            foldl'
                (\ m thing -> Map.alter (\ old_entry -> Just $ thing : fromMaybe [] old_entry) (get thing) m)
                Map.empty

context_lines :: Map FileCmpByPath (Map Int [a]) -> Map FileCmpByPath (Map Int [a])
context_lines = Map.mapWithKey (\ (FileCmpByPath fl) -> Map.unionsWith (<>) . map Map.fromList . map (:[]) . concatMap (\ (ln, msgs) -> (ln, msgs) : map (,[]) (filter (can_be_context_line fl) [ln-1..ln+1])) . Map.toAscList)
    where
        can_be_context_line fl nr = exists fl nr && not_empty fl nr

        exists fl nr = nr > 0 && nr <= length (Text.lines $ File.contents fl)
        not_empty fl nr = not $ Text.null $ Text.lines (File.contents fl) List.!! (nr - 1)

flnr_comparator :: (File, Int) -> (File, Int) -> Ordering
flnr_comparator (f1, n1) (f2, n2)
    | f1 == f2 = n1 `compare` n2
    | otherwise = EQ

get_quote :: File -> Int -> Text
get_quote fl nr = headDef "" $ drop (nr - 1) $ Text.lines $ File.contents fl

file_and_elipsis_lines :: Options.Options -> Maybe (File, Int) -> (File, Int) -> [Line.Line]
file_and_elipsis_lines style (Just (lastf, lastn)) (curf, curn)
    | lastf /= curf = [Line.file_line style curf]
    | lastn + 1 /= curn = [Line.elipsis_line style]
    | otherwise = []
file_and_elipsis_lines style Nothing (curf, _) = [Line.file_line style curf]
-- tests {{{1
case_file_and_elipsis_lines :: Assertion
case_file_and_elipsis_lines =
    make_spans ["ajfowiejf"] >>= \ (f1, _) ->
    make_spans ["aobjiwoiejfawoeijf"] >>= \ (f2, _) ->
    ([Line.file_line Options.ascii_options f2] @=? file_and_elipsis_lines Options.ascii_options (Just (f1, 2)) (f2, 12)) >>
    ([Line.file_line Options.ascii_options f2] @=? file_and_elipsis_lines Options.ascii_options Nothing (f2, 12)) >>
    ([] @=? file_and_elipsis_lines Options.ascii_options (Just (f2, 12)) (f2, 13)) >>
    ([Line.elipsis_line Options.ascii_options] @=? file_and_elipsis_lines Options.ascii_options (Just (f2, 13)) (f2, 20))

tests :: TestTree
tests = $(testGroupGenerator)
