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
show_singleline underlines = concatMap (show_line underlines) $ file_and_elipsis_lines $ context_lines $ lines_shown underlines

lines_shown :: [Underline] -> [(File.File, Int)]
lines_shown =
    map (\ (sp, _, _) -> (Location.file $ Location.start sp, Location.row $ Location.start sp))

context_lines :: [(File.File, Int)] -> [(File.File, Int)]
context_lines =
    List.sortBy (\ (f1, n1) (f2, n2) -> if f1 == f2 then n1 `compare` n2 else EQ) .
    List.nub .
    concatMap (\ (fl, nr) -> map (fl,) [nr-2..nr+2])

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
show_line = _
-- show_multiline {{{1
show_multiline :: Underline -> [Line.Line]
show_multiline und = _
