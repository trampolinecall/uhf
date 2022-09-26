module UHF.Lexer.IndentCounter
    ( count_indents

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified Data.List as List
import qualified Data.Text as Text

count_indents :: File.File -> [(Int, Location.Location, Text.Text, Location.Location)]
count_indents = count_each_line . split_lines

split_lines :: File.File -> [(Location.Location, Text.Text, Location.Location)]
split_lines f = snd $
    List.mapAccumL
        (\ l line -> ((Text.length line + 1) `Location.seek` l, (l, line, (Text.length line) `Location.seek` l)))
        (Location.new_location f)
        (Text.lines $ File.contents f)

count_each_line :: [(Location.Location, Text.Text, Location.Location)] -> [(Int, Location.Location, Text.Text, Location.Location)]
count_each_line = map (\ (sloc, ln, nlloc) -> (Text.length $ Text.takeWhile (==' ') ln, sloc, ln, nlloc))

-- tests {{{1
test_count_indents :: [TestTree]
test_count_indents =
    [ testCase "simple" $
        let (f, [ln1, ln1nl, ln2, ln2nl, ln3]) = SpanHelper.make_spans' "a" "" ["line1", "\n", "line2", "\n", "line3"]
        in
            [(0, Location.start ln1, "line1", Location.start ln1nl),
             (0, Location.start ln2, "line2", Location.start ln2nl),
             (0, Location.start ln3, "line3", Location.start $ Location.eof_span f)] @=? count_indents f

    , testCase "indent" $
        let (f, [ln1, ln1nl, ln2, ln2nl, ln3]) = SpanHelper.make_spans' "a" "" ["line1", "\n", "    line2", "\n", "line3"]
        in
            [(0, Location.start ln1, "line1", Location.start ln1nl),
             (4, Location.start ln2, "    line2", Location.start ln2nl),
             (0, Location.start ln3, "line3", Location.start $ Location.eof_span f)] @=? count_indents f

    , testCase "trailing newline" $
        let (f, [ln1, ln1nl, ln2, ln2nl, ln3, ln3nl]) = SpanHelper.make_spans' "a" "" ["line1", "\n", "line2", "\n", "line3", "\n"]
        in
            [(0, Location.start ln1, "line1", Location.start ln1nl),
             (0, Location.start ln2, "line2", Location.start ln2nl),
             (0, Location.start ln3, "line3", Location.start ln3nl)] @=? count_indents f
    ]

tests :: TestTree
tests = $(testGroupGenerator)
