{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module UHF.Diagnostic.Sections.Underlines
    ( UnderlinesSection
    , Underline
    , Importance(..)
    , Type(..)
    , underlines

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import UHF.Test.SpanHelper

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Line as Line
import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Sections.Utils as Utils
import qualified UHF.Diagnostic.Colors as Colors

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File

import qualified Data.Text as Text
import qualified Data.Function as Function
import qualified Data.List as List
import qualified System.Console.ANSI as ANSI

type UnderlinesSection = [Underline]
type Underline = (Location.Span, Importance, [(Type, Text.Text)])
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint deriving (Show, Eq)

type_color :: Type -> [ANSI.SGR]
type_color Error = Colors.error
type_color Warning = Colors.warning
type_color Note = Colors.note
type_color Hint = Colors.hint

imp_char :: Importance -> Char
imp_char Primary = '^'
imp_char Secondary = '-'
imp_char Tertiary = '.'

underlines :: UnderlinesSection -> Diagnostic.Section
underlines unds =
    let (singleline, multiline) = List.partition (Location.is_single_line . (\ (a, _, _) -> a)) unds

        singleline' = show_singleline singleline
        -- multiline' = concatMap show_multiline multiline
    in Diagnostic.to_section $ singleline'
    -- in Diagnostic.to_section $ singleline' ++ multiline'

-- show_singleline {{{1
-- Message helpers {{{2
type CompleteMessage = (Bool, Location.Span, Type, Text.Text)
type AssignedCompleteMessage = (Int, Bool, Location.Span, Type, Text.Text)

cm_start_col :: CompleteMessage -> Int
cm_start_col (_, Location.Span _ before _, _, _) = Location.col before
acm_start_col :: AssignedCompleteMessage -> Int
acm_start_col (_, _, Location.Span _ before _, _, _) = Location.col before

-- `-- message
-- 4 ('`-- ') + length of message
cm_end_col :: CompleteMessage -> Int
cm_end_col (_, Location.Span _ before _, _, text) = Location.col before + Text.length text + 4
acm_end_col :: AssignedCompleteMessage -> Int
acm_end_col (_, _, Location.Span _ before _, _, text) = Location.col before + Text.length text + 4
-- show_singleline {{{2
show_singleline :: [Underline] -> [Line.Line]
show_singleline unds =
    concatMap (show_line unds) $
    Utils.file_and_elipsis_lines id $
    List.sortBy Utils.flnr_comparator $
    List.nub $
    concatMap (uncurry Utils.context_lines) $
    lines_shown unds

lines_shown :: [Underline] -> [(File.File, Int)]
lines_shown = map (\ (Location.Span start _ _, _, _) -> (Location.file start, Location.row start))
-- show_line {{{2
get_complete_messages :: [Underline] -> [CompleteMessage]
get_complete_messages =
    List.sortBy (flip compare `Function.on` (\ (_, Location.Span _ before _, _, _) -> Location.col before)) .
    concatMap
        (\ (sp, _, msgs) -> map (\ (i, (ty, tx)) -> (i == length msgs - 1, sp, ty, tx)) $ zip [0..] msgs)

get_colored_quote_and_underline_line :: File.File -> Int -> [Underline] -> (FormattedString.FormattedString, FormattedString.FormattedString)
get_colored_quote_and_underline_line fl nr unds =
    let col_in_underline c (Location.Span start before _, _, _) = Location.col start <= c && c <= Location.col before

        underline_importance_and_color (_, imp, (first_msg_ty, _):_) = (imp, Just first_msg_ty)
        underline_importance_and_color (_, imp, []) = (imp, Nothing)

        underline_for_cols = map (\ c -> underline_importance_and_color <$> List.find (col_in_underline c) unds) [1 .. length quote+1]

        quote = Text.unpack $ Utils.get_quote fl nr
        colored_quote =
            map
                (\ (ch, m_und) ->
                    case m_und of
                        Nothing -> ([], Text.pack [ch])
                        Just (_, Nothing) -> ([Colors.bold], Text.pack [ch])
                        Just (_, Just ty) -> (type_color ty, Text.pack [ch])
                )
                (zip quote underline_for_cols)

        underline_line =
            map
                (\case
                    Nothing -> ([], " ")
                    Just (imp, Nothing) -> ([Colors.bold], Text.pack [imp_char imp])
                    Just (imp, Just ty) -> (type_color ty, Text.pack [imp_char imp])
                )
                underline_for_cols

    in (FormattedString.make_formatted_string colored_quote, FormattedString.make_formatted_string underline_line)

show_row :: [AssignedCompleteMessage] -> [AssignedCompleteMessage] -> Line.Line
show_row below msgs =
    let below_pipes = map acm_start_col below
        sorted_msgs = List.sortBy (compare `Function.on` (\ (_, _, Location.Span _ before _, _, _) -> Location.col before)) msgs

        render_msg last_col msg@(_, is_last, _, ty, text) =
            let start_col = acm_start_col msg
                end_col = acm_end_col msg
            in (end_col, [([], Text.pack $ map (\ c -> if c `elem` below_pipes then '|' else ' ') [last_col..start_col - 1]), (type_color ty, Text.concat [if is_last then "`" else "|", "-- ", text])])

    in ("", '|', FormattedString.make_formatted_string $ concat $ snd $ List.mapAccumL render_msg 1 sorted_msgs)

get_rows :: [AssignedCompleteMessage] -> [([AssignedCompleteMessage], [AssignedCompleteMessage])]
get_rows assigned =
    let row_has_messages = not . null . snd

        get_row row =
            let below = filter (\ (r, _, _, _, _) -> r > row) assigned
                msgs = filter (\ (r, _, _, _, _) -> r == row) assigned
            in (below, msgs)

    in takeWhile row_has_messages $ map get_row [0..]

show_line :: [Underline] -> ([Line.Line], (File.File, Int)) -> [Line.Line]
show_line unds (other_lines, (fl, nr)) =
    let cur_line_unds = filter (\ (Location.Span start _ _, _, _) -> Location.file start == fl && Location.row start == nr) unds

        complete_messages = get_complete_messages cur_line_unds
        assigned = List.foldl' assign_message [] complete_messages

        (quote, underline_line) = get_colored_quote_and_underline_line fl nr cur_line_unds

        rows = get_rows assigned

    in other_lines ++
        [(Text.pack $ show nr, '|', quote)] ++
        (if not $ null cur_line_unds
            then [("", '|', underline_line)]
            else []) ++

        map (uncurry show_row) rows
-- assigning {{{3
assign_message :: [AssignedCompleteMessage] -> CompleteMessage -> [AssignedCompleteMessage]
assign_message assigned msg@(is_last, msg_sp, msg_ty, msg_text) =
    let (Just working_row) = List.find (not . overlapping msg assigned) [0..]
        assigned_message = (working_row, is_last, msg_sp, msg_ty, msg_text)
    in assigned_message : assigned

overlapping :: CompleteMessage -> [AssignedCompleteMessage] -> Int -> Bool
overlapping to_assign assigned row =
    let to_assign_start_col = cm_start_col to_assign
        to_assign_end_col = cm_end_col to_assign

        msgs_on_row = filter (\ (r, _, _, _, _) -> r == row) assigned

    in any
        (\ already ->
            let already_start = acm_start_col already
                already_end = acm_end_col already
            in not $
                (to_assign_start_col < already_start && to_assign_end_col < already_start) ||
                (to_assign_start_col > already_end && to_assign_end_col < already_end)
        )
        msgs_on_row
-- show_multiline {{{1
-- show_multiline :: Underline -> [Line.Line]
-- show_multiline und = _
-- tests {{{1
case_underlines :: Assertion
case_underlines =
    let (_, [single_sp, multi_sp]) = make_spans ["abc", "def\nghi\njklm\n"]

        section = underlines
            [ (single_sp, Primary, [(Error, "message 1"), (Hint, "message 2")])
            , (multi_sp, Primary, [(Warning, "message 3")])
            ]
    in Line.compare_many_lines'
        [('f', Colors.file_path), ('e', Colors.error), ('w', Colors.warning), ('h', Colors.hint)]
        [ ("", '>',  "<generated span file>",
                     "f--------------------")

        , ("1", '|', "abc def",
                     "eee    ")
        , ( "", '|', "^^^     ",
                     "eee     ")
        , ( "", '|', "  |-- message 1",
                     "  e------------")
        , ( "", '|', "  `-- message 2",
                     "  h------------")
        , ("2", '|', "ghi",
                     "   ")
        , ("3", '|', "jklm",
                     "    ")

        , ("", '>',  "<generated span file>",
                     "f--------------------")

        , ( "", '|', "     ^^^^^^^",
                     "     w------")
        , ("1", '|', "abc  ^ def ^ ",
                     "    w--   w--")
        , ( "", '|', " ^^^^^ ^^^^^",
                     " w---- w----")
        , ("2", '|', " ^ ghi ^ ",
                     "w--   w--")
        , (" ", '|', " ^     ^^",
                     " w     w-")
        , ("3", '|', " ^ jklm ^ ",
                     "w--    w--")
        , (" ", '|', " ^^^^^^^^",
                     " w-------")
        , (" ", '|', "        `-- message 3",
                     "        w------------")
        ]
        (Diagnostic.section_contents section)

case_show_singleline :: Assertion
case_show_singleline =
    let (_, [abc1, abc2, _, _, _, _, _, _, _, abc3, _, _]) = make_spans' "abc" "" ["abc1", "abc2", "\n", "\n", "\n", "\n", "\n", "context1\n", "context2\n", "abc3\n", "context3\n", "context4\n"]
        (_, [zyx1]) = make_spans' "zyx" "" ["zyx1"]

        unds =
            [ (zyx1, Primary, [(Error, "primary error")])
            , (abc3, Secondary, [(Hint, "secondary hint")])
            , (abc1, Secondary, [(Warning, "secondary warning")])
            , (abc2, Tertiary, [(Note, "tertiary note")])
            ]

    in Line.compare_many_lines'
        [('f', Colors.file_path), ('e', Colors.error), ('w', Colors.warning), ('n', Colors.note), ('h', Colors.hint)]
        [ (   "", '>', "zyx",
                       "f--")
        , (  "1", '|', "zyx1",
                       "eeee")
        , (   "", '|', "^^^^ ",
                       "eeee ")
        , (   "", '|', "   `-- primary error",
                       " --e----------------")
        , (   "", '>', "abc",
                       "f--")
        , (  "1", '|', "abc1abc2",
                       "wwwwnnnn")
        , (   "", '|', "----.... ",
                       "wwwwnnnn ")
        , (   "", '|', "   |   `-- tertiary note",
                       " ------n----------------")
        , (   "", '|', "   `-- secondary warning",
                       " --w--------------------")
        , (  "2", '|', "",
                       "")
        , (  "3", '|', "",
                       "")
        , ("...", '|', "...",
                       " --")
        , (  "6", '|', "context1",
                       "        ")
        , (  "7", '|', "context2",
                       "        ")
        , (  "8", '|', "abc3",
                       "hhhh")
        , (   "", '|', "-----",
                       "hhhhh")
        , (   "", '|', "    `-- secondary hint",
                       " ---h-----------------")
        , (  "9", '|', "context3",
                       "        ")
        , ( "10", '|', "context4",
                       "        ")
        ]
        (show_singleline unds)

case_lines_shown :: Assertion
case_lines_shown =
    let (f1, [sp1, sp2, _, sp3]) = make_spans' "f1" "" ["sp1", "sp2", "\n", "sp3"]
        (f2, [sp4]) = make_spans' "f2" "" ["sp4"]

        unds = [(sp1, undefined, undefined), (sp2, undefined, undefined), (sp3, undefined, undefined), (sp4, undefined, undefined)]
    in [(f1, 1), (f1, 1), (f1, 2), (f2, 1)] @=? lines_shown unds

case_show_line_other_lines :: Assertion -- check concatenates other lines
case_show_line_other_lines =
    let (f, [_]) = make_spans ["thing"]
        unds = []

        other = ("abcdef", '?', FormattedString.make_formatted_string [([], "abcdefghijklmnop")])

    in [ other
       , ("1", '|', FormattedString.make_formatted_string [([], "t"), ([], "h"), ([], "i"), ([], "n"), ([], "g")])
       ] @=? show_line unds ([other], (f, 1))

case_show_line_single :: Assertion
case_show_line_single =
    let (f, [sp]) = make_spans ["sp"]
        unds = [(sp, Primary, [(Error, "message")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp",
                     "ee")
        , ( "", '|', "^^ ",
                     "ee ")
        , ( "", '|', " `-- message",
                     " e----------")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple :: Assertion
case_show_line_multiple =
    let (f, [sp1, _, sp2]) = make_spans ["sp1", "abcdefghijklmnop", "sp2"]
        unds = [(sp1, Primary, [(Error, "a")]), (sp2, Primary, [(Error, "b")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp1 abcdefghijklmnop sp2",
                     "eee                  eee")
        , ( "", '|', "^^^                  ^^^ ",
                     "eee                  eee ")
        , ( "", '|', "  `-- a                `-- b",
                     " -e---- ---------------e----")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple_overlapping :: Assertion
case_show_line_multiple_overlapping =
    let (f, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        unds = [(sp1, Primary, [(Error, "message1"), (Error, "message2")]), (sp2, Primary, [(Error, "message3")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp1 sp2",
                     "eee eee")
        , ( "", '|', "^^^ ^^^ ",
                     "eee eee ")
        , ( "", '|', "  |   `-- message3",
                     " -----e-----------")
        , ( "", '|', "  |-- message1",
                     " -e-----------")
        , ( "", '|', "  `-- message2",
                     " -e-----------")
        ]
        (show_line unds ([], (f, 1)))

case_show_row :: Assertion
case_show_row =
    let (_, [sp1, _]) = make_spans ["sp1", "sp2"]
        messages = [(0, True, sp1, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  `-- message",
                   " -e----------")]

        [show_row [] messages]

case_show_row_message_below :: Assertion
case_show_row_message_below =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        messages = [(0, True, sp2, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  |   `-- message",
                   " -----e----------")]

        [show_row [(1, True, sp1, Error, "message")] messages]

case_show_row_not_last :: Assertion
case_show_row_not_last =
    let (_, [sp1, _]) = make_spans ["sp1", "sp2"]
        messages = [(0, False, sp1, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  |-- message",
                   " -e----------")]

        [show_row [] messages]

case_show_row_multiple :: Assertion
case_show_row_multiple =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "abcdefghijklmnop", "sp2"]
        messages = [(0, True, sp1, Error, "message1"), (0, True, sp2, Error, "message2")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 abcdefghijklmnop sp2
        [("", '|', "  `-- message1         `-- message2",
                   " -e----------- --------e-----------")]

        [show_row [] messages]

case_assign_message_non_overlapping :: Assertion
case_assign_message_non_overlapping =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "                ", "sp2"]

        msg2 = (0, False, sp2, Error, "message 2")
        msg1 = (False, sp1, Error, "message 1")

    in [(0, False, sp1, Error, "message 1"), msg2] @=? assign_message [msg2] msg1

case_assign_message_overlapping :: Assertion
case_assign_message_overlapping =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]

        msg2 = (0, False, sp2, Error, "message 2")
        msg1 = (False, sp1, Error, "message 1")

    in [(1, False, sp1, Error, "message 1"), msg2] @=? assign_message [msg2] msg1

case_assign_message_with_no_space_between :: Assertion
case_assign_message_with_no_space_between =
    {-
    this is not allowed:

    sp1  sp2
    ^^^  ^^^
      `-- a`-- b

    instead, one of the message must be placed a row below:
    sp1  sp2
    ^^^  ^^^
      |    `-- b
      `-- a
    -}
    let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", "  ", "sp2"]

        msg2 = (0, False, sp2, Error, "b")
        msg1 = (False, sp1, Error, "a")

    in [(1, False, sp1, Error, "a"), msg2] @=? assign_message [msg2] msg1

test_overlapping :: [TestTree]
test_overlapping =
    let make_test_case name spacing expected =
            let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", spacing, "sp2"]

                msg2 = (0, True, sp2, Error, "b")
                msg1 = (True, sp1, Error, "message 1")
            in testCase name $ expected @=? overlapping msg1 [msg2] 0

    in
        [ make_test_case "end left" "abcdefghijk" False
            -- sp1abcdefghijksp2
            --                 `-- b
            --   `-- message 1

        , make_test_case "end inside" "abcdefgh" True
            -- sp1abcdefghsp2
            --              `-- b
            --   `-- message 1

        , make_test_case "end right" "" True
            -- sp1sp2
            --    `-- b
            --   `-- message 1

        , make_test_case "no space between" "abcdefghij" True
            -- sp1abcdefghijsp2
            --                `-- b
            --   `-- message 1
        ]

-- case_multiline_flat_box :: Assertion
-- case_multiline_flat_box = _
-- case_multiline_top_change :: Assertion
-- case_multiline_top_change = _
-- case_multiline_bottom_change :: Assertion
-- case_multiline_bottom_change = _

tests :: TestTree
tests = $(testGroupGenerator)
