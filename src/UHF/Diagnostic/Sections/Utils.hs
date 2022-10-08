{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Utils where

import UHF.Util.Prelude

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import UHF.IO.Location.SpanHelper

import qualified UHF.Diagnostic.Line as Line

import qualified UHF.IO.File as File

import qualified Data.Text as Text
import qualified Safe

context_lines :: File.File -> Int -> [(File.File, Int)]
context_lines f n = filter (uncurry line_exists) $ map (f,) [n-2..n+2]

line_exists :: File.File -> Int -> Bool
line_exists fl nr =
    nr > 0 && nr <= length (Text.lines $ File.contents fl)

flnr_comparator :: (File.File, Int) -> (File.File, Int) -> Ordering
flnr_comparator (f1, n1) (f2, n2)
    | f1 == f2 = n1 `compare` n2
    | otherwise = EQ

get_quote :: File.File -> Int -> Text
get_quote fl nr = Safe.headDef "" $ drop (nr - 1) $ Text.lines $ File.contents fl

file_and_elipsis_lines :: (a -> (File.File, Int)) -> [a] -> [([Line.Line], a)]
file_and_elipsis_lines convert things =
    let lns = map convert things
        lasts = Nothing : map Just lns

        fel (Just (lastf, lastn), (curf, curn), cur)
            | lastf /= curf = ([Line.file_line curf], cur)
            | lastn + 1 /= curn = ([Line.elipsis_line], cur)
            | otherwise = ([], cur)

        fel (Nothing, (curf, _), cur) = ([Line.file_line curf], cur)

    in map fel $ zip3 lasts lns things

-- tests {{{1
case_file_and_elipsis_lines :: Assertion
case_file_and_elipsis_lines =
    let (f1, _) = make_spans ["ajfowiejf"]
        (f2, _) = make_spans ["aobjiwoiejfawoeijf"]

        lns =
            [ (f1, 2)
            , (f2, 12)
            , (f2, 20)
            ]

    in [([Line.file_line f1], (f1, 2)), ([Line.file_line f2], (f2, 12)), ([Line.elipsis_line], (f2, 20))] @=? file_and_elipsis_lines identity lns

tests :: TestTree
tests = $(testGroupGenerator)
