{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Underlines
    ( UnderlinesSection
    , Underline
    , Importance(..)
    , Type(..)
    , underlines
    ) where

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Line as Line
import qualified UHF.Diagnostic.Sections.Utils as Utils

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File

import qualified Data.Text as Text
import qualified Data.List as List

type UnderlinesSection = [Underline]
type Underline = (Location.Span, Importance, [(Type, Text.Text)])
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint

underlines :: UnderlinesSection -> Diagnostic.Section
underlines unds =
    let (singleline, multiline) = List.partition (Location.is_single_line . (\ (a, _, _) -> a)) unds

        singleline' = show_singleline singleline
        multiline' = concatMap show_multiline multiline
    in Diagnostic.to_section $ singleline' ++ multiline'

-- show_singleline {{{1
data Line = UnderlinesLine File.File Int | FileLine File.File | ElipsisLine

show_singleline :: [Underline] -> [Line.Line]
show_singleline underlines =
    concatMap (show_line underlines) $
    file_and_elipsis_lines $
    List.sortBy Utils.flnr_comparator $
    nub $
    concatMap Utils.context_lines $
    lines_shown underlines

lines_shown :: [Underline] -> [(File.File, Int)]
lines_shown = map (\ (Location.Span start _ _, _, _) -> (Location.file start, Location.row start))

file_and_elipsis_lines :: [(File.File, Int)] -> [Line]
file_and_elipsis_lines lines =
    let lasts = Nothing : map Just lines

        fel (Just (lastf, lastn), (curf, curn))
            | lastf /= curf = [FileLine curf, UnderlinesLine curf curn]
            | lastn + 1 /= curn = [ElipsisLine, UnderlinesLine curf curn]
            | otherwise = [UnderlinesLine curf curn]
        fel (Nothing, (curf, curn)) = [UnderlinesLine curf curn]

    in concatMap fel $ zip lasts lines

show_line :: [Underline] -> Line -> [Line.Line]
show_line _ (FileLine f) = [Line.file_line f]
show_line _ (ElipsisLine) = [Line.elipsis_line]

show_line unds (UnderlinesLine fl nr) = _
-- show_multiline {{{1
show_multiline :: Underline -> [Line.Line]
show_multiline und = _
