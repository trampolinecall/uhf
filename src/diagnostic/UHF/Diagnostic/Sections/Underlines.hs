{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

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

show_line :: [Underline] -> ([Line.Line], (File.File, Int)) -> [Line.Line]
show_line unds (other_lines, (fl, nr)) =
    let cur_line_unds = filter (\ (Location.Span start _ _, _, _) -> Location.file start == fl && Location.row start == nr) unds

        messages_with_span = concatMap (\ (sp, _, msgs) -> map (\ (ty, tx) -> (sp, ty, tx)) msgs) cur_line_unds
        assigned = List.foldl' assign_message [] messages_with_span

        col_in_underline c (Location.Span start before _, _, _) = Location.col start <= c && c <= Location.col before
        underline_importance_and_color (_, imp, (first_msg_ty, _):_) = (imp, Just first_msg_ty)
        underline_importance_and_color (_, imp, []) = (imp, Nothing)

        quote = Text.unpack $ Utils.get_quote fl nr
        quote_underlines =
            map (\ c -> underline_importance_and_color <$> List.find (col_in_underline c) cur_line_unds) [1 .. length quote+1]

        colored_quote =
            map (\ (ch, m_und) -> (maybe [] (maybe [] type_color . snd) m_und, Text.pack [ch])) (zip quote quote_underlines)
        underline_line =
            map (maybe ([], " ") (\ (imp, ty) -> (maybe [Colors.bold] type_color ty, Text.pack [imp_char imp])) . snd) (zip quote quote_underlines)

    in other_lines ++
        [ (Text.pack $ show nr, '|', FormattedString.make_formatted_string colored_quote)
        , ("", '|', FormattedString.make_formatted_string underline_line)
        ]
       -- TODO: put underlines

assign_message :: [(Int, Location.Span, Type, Text.Text)] -> (Location.Span, Type, Text.Text) -> [(Int, Location.Span, Type, Text.Text)]
assign_message assigned msg@(msg_sp, msg_ty, msg_text) =
    let (Just working_row) = List.find (not . overlapping msg assigned) [0..]
        assigned_message = (working_row, msg_sp, msg_ty, msg_text)
    in assigned_message : assigned

overlapping :: (Location.Span, Type, Text.Text) -> [(Int, Location.Span, Type, Text.Text)] -> Int -> Bool
overlapping (msg_sp@(Location.Span _ msg_sp_start _), _, msg_text) assigned row =
    let assigning_msg_start_col = Location.col msg_sp_start
        assigning_msg_end_col = message_end_column msg_sp msg_text

        msgs_on_row = filter (\ (r, _, _, _) -> r == row) assigned
    in any (\ (_, sp@(Location.Span _ b _), _, txt) ->
        let msg_start = Location.col b
            msg_end = message_end_column sp txt
        in not $
            (assigning_msg_start_col < msg_start && assigning_msg_end_col < msg_start) ||
            (assigning_msg_start_col > msg_end && assigning_msg_end_col < msg_end)
        ) msgs_on_row

message_end_column :: Location.Span -> Text.Text -> Int
message_end_column sp t = Location.col (Location.before_end sp) + Text.length t + 4
    -- `-- message
    -- 4 ('`-- ') + length of message
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
                     "e--    ")
        , ( "", '|', "^^^    ",
                     "e--    ")
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
                       "e---")
        , (   "", '|', "^^^^",
                       "e---")
        , (   "", '|', "   `-- primary error",
                       "   e----------------")
        , (   "", '>', "abc",
                       "f--")
        , (  "1", '|', "abc1abc2",
                       "w---n---")
        , (   "", '|', "----....",
                       "w---n---")
        , (   "", '|', "   |   `-- tertiary note",
                       "       n----------------")
        , (   "", '|', "   `-- secondary warning",
                       "   w--------------------")
        , (  "2", '|', "",
                       "")
        , (  "3", '|', "",
                       "")
        , ("...", '|', "...",
                       "   ")
        , (  "6", '|', "context1",
                       "        ")
        , (  "7", '|', "context2",
                       "        ")
        , (  "8", '|', "abc3",
                       "h---")
        , (   "", '|', "-----",
                       "h----")
        , (   "", '|', "    `-- secondary hint",
                       "    h-----------------")
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
       , ("1", '|', FormattedString.make_formatted_string [([], "thing")])
       ] @=? show_line unds ([other], (f, 1))

case_show_line_single :: Assertion
case_show_line_single =
    let (f, [sp]) = make_spans ["sp"]
        unds = [(sp, Primary, [(Error, "message")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp",
                     "e-")
        , ( "", '|', "^^",
                     "e-")
        , ( "", '|', " `-- message",
                     " e----------")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple :: Assertion
case_show_line_multiple =
    let (f, [sp1, _, sp2]) = make_spans ["sp1", "               ", "sp2"]
        unds = [(sp1, Primary, [(Error, "a")]), (sp2, Primary, [(Error, "b")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp1                 sp2",
                     "e--                 e--")
        , ( "", '|', "^^^                 ^^^",
                     "e--                 e--")
        , ( "", '|', "  `-- a               `-- b",
                     "  e----               e----")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple_overlapping :: Assertion
case_show_line_multiple_overlapping =
    let (f, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        unds = [(sp1, Primary, [(Error, "message1"), (Error, "message2")]), (sp2, Primary, [(Error, "message3")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp1 sp2",
                     "e-- e--")
        , ( "", '|', "^^^ ^^^",
                     "e-- e--")
        , ( "", '|', "  |   `-- message3",
                     "      e-----------")
        , ( "", '|', "  |-- message1",
                     "   e----------")
        , ( "", '|', "  `-- message2",
                     "  e-----------")
        ]
        (show_line unds ([], (f, 1)))

case_assign_message_non_overlapping :: Assertion
case_assign_message_non_overlapping =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "                ", "sp2"]

        msg2 = (0, sp2, Error, "message 2")
        msg1 = (sp1, Error, "message 1")

    in [(0, sp1, Error, "message 1"), msg2] @=? assign_message [msg2] msg1

case_assign_message_overlapping :: Assertion
case_assign_message_overlapping =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]

        msg2 = (0, sp2, Error, "message 2")
        msg1 = (sp1, Error, "message 1")

    in [(1, sp1, Error, "message 1"), msg2] @=? assign_message [msg2] msg1

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

        msg2 = (0, sp2, Error, "b")
        msg1 = (sp1, Error, "a")

    in [(1, sp1, Error, "a"), msg2] @=? assign_message [msg2] msg1

test_overlapping :: [TestTree]
test_overlapping =
    let make_test_case name spacing expected =
            let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", spacing, "sp2"]

                msg2 = (0, sp2, Error, "b")
                msg1 = (sp1, Error, "message 1")
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

case_message_end_column :: Assertion
case_message_end_column =
    {-
    sp
     `-- abc
    12345678
    -}
    let (_, [sp]) = make_spans ["sp"]
    in 9 @=? message_end_column sp "abc"

-- case_multiline_flat_box :: Assertion
-- case_multiline_flat_box = _
-- case_multiline_top_change :: Assertion
-- case_multiline_top_change = _
-- case_multiline_bottom_change :: Assertion
-- case_multiline_bottom_change = _

tests :: TestTree
tests = $(testGroupGenerator)
