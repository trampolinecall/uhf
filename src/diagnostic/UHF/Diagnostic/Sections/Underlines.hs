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
data Type = Error | Warning | Note | Hint

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
        multiline' = concatMap show_multiline multiline
    in Diagnostic.to_section $ singleline' ++ multiline'

-- show_singleline {{{1
data Line = UnderlinesLine File.File Int | FileLine File.File | ElipsisLine

show_singleline :: [Underline] -> [Line.Line]
show_singleline underlines =
    concatMap (show_line underlines) $
    file_and_elipsis_lines $
    List.sortBy Utils.flnr_comparator $
    List.nub $
    concatMap (uncurry Utils.context_lines) $
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

show_line unds (UnderlinesLine fl nr) =
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
        underlines =
            map (maybe ([], " ") (\ (imp, ty) -> (maybe [Colors.bold] type_color ty, Text.pack [imp_char imp])) . snd) (zip quote quote_underlines)

    in [ (Text.pack $ show nr, '|', FormattedString.make_formatted_string colored_quote)
       , ("", '|', FormattedString.make_formatted_string underlines)
       ]
       -- TODO: put underlines

assign_message :: [(Int, Location.Span, Type, Text.Text)] -> (Location.Span, Type, Text.Text) -> [(Int, Location.Span, Type, Text.Text)]
assign_message assigned msg@(msg_sp, msg_ty, msg_text) =
    let (Just working_row) = List.find (not . overlapping msg assigned) [0..]
        assigned_message = (working_row, msg_sp, msg_ty, msg_text)
    in assigned_message : assigned

overlapping :: (Location.Span, Type, Text.Text) -> [(Int, Location.Span, Type, Text.Text)] -> Int -> Bool
overlapping msg@(msg_sp@(Location.Span _ msg_start _), _, msg_text) assigned row =
    let msg_start_col = Location.col msg_start
        msg_end_col = message_end_column msg_sp msg_text

        msgs_on_row = filter (\ (r, _, _, _) -> r == row) assigned
    in any (\ (_, sp@(Location.Span _ b _), _, txt) -> msg_start_col <= message_end_column sp txt || msg_end_col >= Location.col b) msgs_on_row

message_end_column :: Location.Span -> Text.Text -> Int
message_end_column sp t = Location.col (Location.before_end sp) + Text.length t
-- show_multiline {{{1
show_multiline :: Underline -> [Line.Line]
show_multiline und = _
-- tests {{{1
case_underlines :: Assertion -- pass singlelinee and multiline to see if it concatenates corectly
case_underlines =
    let (f, [single_sp, multi_sp]) = make_spans ["abc", "def\nghi\njklm\n"]

        section = underlines
            [ (single_sp, Primary, [(Error, "message 1"), (Hint, "message 2")])
            , (multi_sp, Primary, [(Warning, "message 3")])
            ]
    in assertBool "underlines failed to render correctly" $
        Line.compare_many_lines
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
    let (abc, [abc1, abc2, _, _, _, _, _, _, _, abc3, _, _]) = make_spans' "" "abc" ["abc1", "abc2", "\n", "\n", "\n", "\n", "\n", "context1\n", "context2\n", "abc3\n", "context3\n", "context4\n"]
        (zyx, [zyx1]) = make_spans' "" "zyx" ["zyx1"]

        unds =
            [ (zyx1, Primary, [(Error, "primary error")])
            , (abc3, Secondary, [(Hint, "secondary hint")])
            , (abc1, Secondary, [(Warning, "secondary warning")])
            , (abc2, Tertiary, [(Note, "tertiary note")])
            ]

    in assertBool "underlines failed to render correctly" $
        Line.compare_many_lines
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
                           "        ")
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
    let (f1, [sp1, sp2, _, sp3]) = make_spans' "" "f1" ["sp1", "sp2", "\n", "sp3"]
        (f2, [sp4]) = make_spans' "" "f2" ["sp4"]

        unds = [(sp1, undefined, undefined), (sp2, undefined, undefined), (sp3, undefined, undefined), (sp4, undefined, undefined)]
    in [(f1, 1), (f1, 1), (f1, 2), (f2, 1)] @=? lines_shown unds

case_file_and_elipsis_lines :: Assertion -- pass multiple spans from differnet files, f1:2 f2:12, f2:20
case_file_and_elipsis_lines = _

case_show_line_single :: Assertion -- single underline
case_show_line_single = _
case_show_line_multiple :: Assertion -- multiple undelrines, nothing overlapping
case_show_line_multiple = _
case_show_line_multiple_overlapping :: Assertion -- multiple undliners, with overlap, make sure right to left
case_show_line_multiple_overlapping = _

case_assign_message_non_overlapping :: Assertion -- 2 messages
case_assign_message_non_overlapping = _
case_assign_message_overlapping :: Assertion -- 2 messages
case_assign_message_overlapping = _
case_assign_message_with_no_space_between :: Assertion -- 2 messages, should put on different lines
case_assign_message_with_no_space_between = _

case_overlapping_overlapping :: Assertion
case_overlapping_overlapping = _
case_overlapping_not_overlapping :: Assertion
case_overlapping_not_overlapping = _

case_message_end_column :: Assertion
case_message_end_column = _

case_multiline_flat_box :: Assertion
case_multiline_flat_box = _
case_multiline_top_change :: Assertion
case_multiline_top_change = _
case_multiline_bottom_change :: Assertion
case_multiline_bottom_change = _

tests :: TestTree
tests = $(testGroupGenerator)
