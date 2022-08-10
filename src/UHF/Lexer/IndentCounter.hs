module UHF.Lexer.IndentCounter
    ( count_indents

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified Data.Text as Text

count_indents :: Text.Text -> [(Int, [Text.Text])]
count_indents = count_each_line . split_lines

split_lines :: Text.Text -> [[Text.Text]]
split_lines t =
    let lines = Text.lines t
        has_backslash = map (maybe False (('\\' ==) . snd) . Text.unsnoc . Text.stripEnd) lines

        -- the first line always starts a new line
        -- every line from then only starts a new line if the previous line doesnt end in a backslash
        start_new_line = True : map not has_backslash

        make_groups ((first_line, _):more_lines) =
            -- the second element is a bool saying whether or not that line is a new line, so we want to take the ones that arent starting a new line
            let (continuations, after) = break snd more_lines
            in (first_line : map fst continuations) : make_groups after

        make_groups [] = []

    in make_groups $ zip lines start_new_line

count_each_line :: [[Text.Text]] -> [(Int, [Text.Text])]
count_each_line lines =
    zip
        (map (Text.length . Text.takeWhile (==' ') . head) lines) -- a line should never be empty
        lines

-- tests {{{1
test_count_indents :: [TestTree]
test_count_indents =
    [ testCase "simple" $
        [(0, ["line1"]), (0, ["line2"]), (0, ["line3"])] @=? count_indents "line1\nline2\nline3"

    , testCase "indent" $
        [(0, ["line1"]), (4, ["    line2"]), (0, ["line3"])] @=? count_indents "line1\n    line2\nline3"

    , testCase "backslash" $
        [(0, ["line1\\", "line2"]), (0, ["line3"])] @=? count_indents "line1\\\nline2\nline3"

    , testCase "backslash then indent" $
        [(0, ["line1\\", "line2"]), (4, ["    line3"])] @=? count_indents "line1\\\nline2\n    line3"

    , testCase "indent on backslash" $
        [(4, ["    line1\\", "line2"]), (2, ["  line3"])] @=? count_indents "    line1\\\nline2\n  line3"

    , testCase "insignificant indent" $
        [(0, ["line1\\", "    line2"]), (0, ["line3"])] @=? count_indents "line1\\\n    line2\nline3"
    ]

test_split_lines :: [TestTree]
test_split_lines =
    [ testCase "no backslashes" $
        [["line1"], ["line2"], ["line3"]] @=? split_lines "line1\nline2\nline3"

    , testCase "backslash" $
        [["line1\\", "line2"], ["line3"]] @=? split_lines "line1\\\nline2\nline3"

    , testCase "backslash with wh" $
        [["line1 \\ ", "line2"], ["line3"]] @=? split_lines "line1 \\ \nline2\nline3"

    , testCase "backslashes" $
        [["line1 \\ ", "line2 \\", "line3"], ["line4"]] @=? split_lines "line1 \\ \nline2 \\\nline3\nline4"
    ]

test_count_each_line :: [TestTree]
test_count_each_line =
    [ testCase "simple" $
        [(0, ["line1"]), (4, ["    line2"]), (6, ["      line3"]), (0, ["line4"])] @=? count_each_line [["line1"], ["    line2"], ["      line3"], ["line4"]]

    , testCase "long line" $
        [(0, ["line1  \\", "line2 \\", "      line3"]), (4, ["    line2"]), (0, ["line3"])] @=? count_each_line [["line1  \\", "line2 \\", "      line3"], ["    line2"], ["line3"]]
    ]

tests :: TestTree
tests = $(testGroupGenerator)
