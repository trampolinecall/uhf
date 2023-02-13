{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic.Sections.Messages
    ( MessagesSection
    , Underline
    , Type(..)

    , error, warning, note, hint

    , tests
    ) where

import UHF.Util.Prelude hiding (error)

import UHF.IO.Location.SpanHelper

import qualified UHF.Diagnostic.Line as Line
import qualified UHF.FormattedString as FormattedString
import qualified UHF.Diagnostic.Sections.Utils as Utils
import qualified UHF.Diagnostic.Colors as Colors
import qualified UHF.Diagnostic.Section as Section

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Function as Function
import qualified Data.List as List
import qualified System.Console.ANSI as ANSI

-- TODO: fix errors on eof where messages arent shown

type MessagesSection = [Underline]
type Underline = (Location.Span, Type, Maybe FormattedString)
data Type = Error | Warning | Note | Hint deriving (Show, Eq)

type_color :: Type -> [ANSI.SGR]
type_color Error = Colors.error
type_color Warning = Colors.warning
type_color Note = Colors.note
type_color Hint = Colors.hint

type_char, top_type_char :: Type -> Char
type_char Error = '^'
type_char Warning = '^'
type_char Note = '~'
type_char Hint = '~'

top_type_char Error = 'v'
top_type_char Warning = 'v'
top_type_char Note = '~'
top_type_char Hint = '~'

error, warning, note, hint :: Location.Span -> FormattedString -> Underline
error s m = (s, Error, Just m)
warning s m = (s, Warning, Just m)
note s m = (s, Note, Just m)
hint s m = (s, Hint, Just m)

instance Section.Section MessagesSection where
    render unds =
        let (singleline, multiline) = List.partition (Location.is_single_line . (\ (a, _, _) -> a)) unds

            singleline' = show_singleline singleline
            multiline' = concatMap show_multiline multiline
        in singleline' ++ multiline'

-- show_singleline {{{1
-- Message helpers {{{2
newtype CompleteMessage = CompleteMessage (Bool, Location.Location, Type, FormattedString) deriving (Eq, Show)
instance Format CompleteMessage where
    format (CompleteMessage (is_last, _, ty, text)) = FormattedString.Colored (type_color ty) ((if is_last then "`" else "|") <> "-- " <> text)

cm_start_col :: CompleteMessage -> Int
cm_start_col (CompleteMessage (_, loc, _, _)) = Location.col $ Location.lc loc
-- `-- message
-- 4 ('`-- ') + length of message
cm_end_col :: CompleteMessage -> Int
cm_end_col (CompleteMessage (_, loc, _, text)) = Location.col (Location.lc loc) + FormattedString.length text + 4
-- show_singleline {{{2
show_singleline :: [Underline] -> [Line.Line]
show_singleline unds =
    concatMap (show_line unds) $ -- TODO: rewrite by grouping
    Utils.file_and_elipsis_lines identity $
    List.sortBy Utils.flnr_comparator $
    List.nub $
    concatMap (uncurry Utils.context_lines) $
    lines_shown unds

lines_shown :: [Underline] -> [(File.File, Int)]
lines_shown = map (\ (sp, _, _) -> (Location.sp_file sp, Location.row $ Location.lc $ Location.sp_s sp))
-- show_line {{{2
get_complete_messages :: [Underline] -> [CompleteMessage]
get_complete_messages =
    List.sortBy (flip compare `Function.on` cm_start_col) .
    Maybe.mapMaybe (\ (sp, ty, msg) -> CompleteMessage <$> ((False, Location.sp_be sp, ty,) <$> msg)) -- TODO: figure out what to do with False (probably overhaul / refactor the rendering code)

get_colored_quote_and_underline_line :: File.File -> Int -> [Underline] -> (FormattedString.FormattedString, FormattedString.FormattedString)
get_colored_quote_and_underline_line fl nr unds =
    let col_in_underline c (sp, _, _) = Location.sp_s_col sp <= c && c <= Location.sp_be_col sp

        message_type (_, ty, _) = ty

        underline_for_cols = map (\ c -> message_type <$> find (col_in_underline c) unds) [1 .. length quote+1]

        quote = Text.unpack $ Utils.get_quote fl nr
        colored_quote =
            foldl' (<>) "" $
            zipWith
                (\ ch m_und ->
                    case m_und of
                        Nothing -> FormattedString.Literal $ Text.pack [ch]
                        Just ty -> FormattedString.color_text (type_color ty) (Text.pack [ch])
                )
                quote underline_for_cols

        underline_line =
            foldl' (<>) "" $
            map
                (\case
                    Nothing -> " "
                    Just ty -> FormattedString.color_text (type_color ty) (Text.pack [type_char ty])
                )
                underline_for_cols

    in (colored_quote, underline_line)

show_row :: [CompleteMessage] -> [CompleteMessage] -> Line.Line
show_row below msgs =
    let below_pipes = map cm_start_col below
        sorted_msgs = List.sortBy (compare `Function.on` cm_start_col) msgs

        render_msg last_col msg =
            let start_col = cm_start_col msg
                end_col = cm_end_col msg
            in ( end_col
               , FormattedString.Literal (Text.pack $ map (\ c -> if c `elem` below_pipes then '|' else ' ') [last_col..start_col - 1]) <> format msg
               )

    in Line.other_line $ foldl' FormattedString.Join "" $ snd $ List.mapAccumL render_msg 1 sorted_msgs

show_line :: [Underline] -> ([Line.Line], (File.File, Int)) -> [Line.Line]
show_line unds (other_lines, (fl, nr)) =
    let cur_line_unds = filter (\ (sp, _, _) -> Location.sp_file sp == fl && Location.sp_s_row sp == nr) unds

        complete_messages = get_complete_messages cur_line_unds
        msg_rows = assign_messages complete_messages

        (quote, underline_line) = get_colored_quote_and_underline_line fl nr cur_line_unds

    in other_lines ++
        [Line.numbered_line nr quote] ++
        (if not $ null cur_line_unds
            then [Line.other_line underline_line]
            else []) ++

        map (uncurry show_row) msg_rows
-- assigning rows {{{3
assign_messages :: [CompleteMessage] -> [([CompleteMessage], [CompleteMessage])]
assign_messages msgs =
    let assignments =
            zipWith
                (\ cur_msg prev_assignments ->
                    -- head should never fail because there will always be a next row number that doesn't have any previously assigned messages to overlap with the current one
                    let row = head $ filter (\ rown -> not $ overlapping cur_msg $ map fst $ filter ((==rown) . snd) prev_assignments) ([0..] :: [Int])
                    in (cur_msg, row))
                msgs
                (List.inits assignments)

        rows = map (map fst) $ List.groupBy ((==) `on` snd) assignments
        belows = map concat $ drop 1 $ List.tails rows

    in zip belows rows

overlapping :: CompleteMessage -> [CompleteMessage] -> Bool
overlapping to_assign assigned =
    let to_assign_start_col = cm_start_col to_assign
        to_assign_end_col = cm_end_col to_assign
    in any
        (\ already ->
            let already_start = cm_start_col already
                already_end = cm_end_col already
            in not $
                (to_assign_start_col < already_start && to_assign_end_col < already_start) || -- completely to the left of the already assigned message
                (to_assign_start_col > already_end && to_assign_end_col < already_end) -- completely to the right of the already assigned message
        )
        assigned
-- show_multiline {{{1
show_multiline :: Underline -> [Line.Line]
show_multiline (und_sp, und_type, und_msg) = -- TODO: merge messages with the same span
    let
        start_line = Location.sp_s_row und_sp
        end_line = Location.sp_be_row und_sp

        n_lines = (end_line + 1) - start_line
        n_vertical_lines = n_lines `div` 2
        mid_line =
            if odd n_lines
                then Just (Location.sp_file und_sp, (start_line + end_line) `div` 2)
                else Nothing

        und_char = type_char und_type
        rev_und_char = top_type_char und_type
        sgr = type_color und_type

    in
        [Line.file_line (Location.sp_file und_sp)] ++
        show_top_lines (Location.sp_s und_sp) n_vertical_lines rev_und_char sgr ++
        show_middle_line mid_line sgr ++
        show_bottom_lines (Location.sp_be und_sp) n_vertical_lines und_char und_type und_msg -- TODO: see todo at the beginning of this function

show_top_lines :: Location.Location -> Int -> Char -> [ANSI.SGR] -> [Line.Line]
show_top_lines loc n ch sgr =
    let start_col = Location.loc_col loc
        start_line = Location.loc_row loc
        file = Location.loc_file loc

        start_quote = Utils.get_quote file start_line

        top_lines = [Location.loc_row loc + 1 .. Location.loc_row loc + n - 1]
        max_col = maximum (map (Text.length . Utils.get_quote file) (start_line : top_lines)) + 2

        col_diff = max_col - start_col + 1
        n_ch = min 3 col_diff
        n_dash = col_diff - n_ch

    in [ Line.other_line $
            FormattedString.Literal (Text.replicate (start_col + 2 - 1) " ") <> FormattedString.color_text sgr (Text.replicate n_ch (Text.pack [ch])) <> FormattedString.color_text [Colors.bold] (Text.replicate n_dash "-")] ++
        [ Line.numbered_line start_line $
            "  " <> FormattedString.Literal (Text.take (start_col - 1) start_quote) <> FormattedString.color_text sgr (Text.drop (start_col - 1) start_quote) <> FormattedString.Literal (Text.replicate (max_col - Text.length start_quote - 1) " ") <> FormattedString.color_text [Colors.bold] "|"] ++
        map (\ l ->
            let quote = Utils.get_quote file l
                pad = max_col - Text.length quote - 1
            in Line.numbered_line l $ "  " <> FormattedString.color_text sgr quote <> FormattedString.Literal (Text.replicate pad " ") <> FormattedString.color_text [Colors.bold] "|"
        ) top_lines

show_bottom_lines :: Location.Location -> Int -> Char -> Type -> Maybe FormattedString -> [Line.Line]
show_bottom_lines loc n ch ty msg =
    let end_col = Location.loc_col loc + 2
        end_line = Location.loc_row loc
        file = Location.loc_file loc

        end_quote = Utils.get_quote file end_line

        n_ch = min 3 end_col
        n_dash = end_col - n_ch

        bottom_lines = [Location.loc_row loc - n + 1 .. Location.loc_row loc - 1]

        sgr = type_color ty

        -- TODO: refactor this because this is badly adapted to the message overhaul
    in map (\ l ->
            let quote = Utils.get_quote file l
            in Line.numbered_line l $ FormattedString.color_text [Colors.bold] "| " <> FormattedString.color_text sgr quote
        ) bottom_lines ++
        [ Line.numbered_line end_line $
            FormattedString.color_text [Colors.bold] "| " <> FormattedString.color_text sgr (Text.take (end_col - 2 ) end_quote) <> FormattedString.Literal (Text.drop (end_col - 2) end_quote)] ++
        [ Line.other_line $
            FormattedString.color_text [Colors.bold] (Text.replicate n_dash "-") <> FormattedString.color_text sgr (Text.replicate n_ch (Text.pack [ch]))] ++
        zipWith (\ i msg ->
            Line.other_line $
                FormattedString.Literal (Text.replicate (end_col - 1) " ") <> format (CompleteMessage (i == (0 :: Int) {- length msgs - 1 -}, loc, ty, msg))
        ) [0..] (Maybe.maybeToList msg)

show_middle_line :: Maybe (File.File, Int) -> [ANSI.SGR] -> [Line.Line]
show_middle_line (Just (file, nr)) sgr = [Line.numbered_line nr $ "  " <> FormattedString.color_text sgr (Utils.get_quote file nr)]
show_middle_line Nothing _ = []
-- tests {{{1
case_empty :: Assertion
case_empty =
    let lines = Section.render ([] :: MessagesSection)
    in Line.compare_lines [] lines

case_messages :: Assertion
case_messages =
    let (_, [single_sp, multi_sp]) = make_spans ["abc", "def\nghi\njklm\n"]

        lines = Section.render
            (
                [ error single_sp "message 1", hint single_sp "message 2"
                , warning multi_sp "message 3"
                ] :: MessagesSection
            )
    in Line.compare_lines
        [ ("", '>',  "")
        , ("1", '|', "abc def")
        , ( "", '|', "^^^     ")
        , ( "", '|', "  |-- message 1")
        , ( "", '|', "  `-- message 2")
        , ("2", '|', "ghi")
        , ("3", '|', "jklm")

        , ("", '>',  "")

        , ( "", '|', "      vvv--")
        , ("1", '|', "  abc def |")
        , ("2", '|', "  ghi")
        , ("3", '|', "| jklm")
        , ( "", '|', "----^^^")
        , ( "", '|', "      `-- message 3")
        ]
        lines

case_show_singleline :: Assertion
case_show_singleline =
    let (_, [abc1, abc2, _, _, _, _, _, _, _, abc3, _, _]) = make_spans' "abc" "" ["abc1", "abc2", "\n", "\n", "\n", "\n", "\n", "context1\n", "context2\n", "abc3\n", "context3\n", "context4\n"]
        (_, [zyx1]) = make_spans' "zyx" "" ["zyx1"]

        unds =
            [ error zyx1 "error"
            , hint abc3 "hint"
            , warning abc1 "warning"
            , note abc2 "note"
            ]

    in Line.compare_lines
        [ (   "", '>', "zyx")
        , (  "1", '|', "zyx1")
        , (   "", '|', "^^^^ ")
        , (   "", '|', "   `-- error")
        , (   "", '>', "abc")
        , (  "1", '|', "abc1abc2")
        , (   "", '|', "^^^^~~~~ ")
        , (   "", '|', "   |   `-- note")
        , (   "", '|', "   `-- warning")
        , (  "2", '|', "")
        , (  "3", '|', "")
        , ("...", '|', "...")
        , (  "6", '|', "context1")
        , (  "7", '|', "context2")
        , (  "8", '|', "abc3")
        , (   "", '|', "~~~~~")
        , (   "", '|', "    `-- hint")
        , (  "9", '|', "context3")
        , ( "10", '|', "context4")
        ]
        (show_singleline unds)

case_lines_shown :: Assertion
case_lines_shown =
    let (f1, [sp1, sp2, _, sp3]) = make_spans' "f1" "" ["sp1", "sp2", "\n", "sp3"]
        (f2, [sp4]) = make_spans' "f2" "" ["sp4"]

        unds = [(sp1, Error, Nothing), (sp2, Note, Nothing), (sp3, Note, Nothing), (sp4, Note, Nothing)]
    in [(f1, 1), (f1, 1), (f1, 2), (f2, 1)] @=? lines_shown unds

case_show_line_other_lines :: Assertion -- check concatenates other lines
case_show_line_other_lines =
    let (f, [_]) = make_spans ["thing"]
        unds = []

        other = Line.numbered_line 2 "abcdefghijklmnop"

    in Line.compare_lines
        [ ("2", '|', "abcdefghijklmnop")
        , ("1", '|', "thing")
        ]
        (show_line unds ([other], (f, 1)))

case_show_line_single :: Assertion
case_show_line_single =
    let (f, [sp]) = make_spans ["sp"]
        unds = [error sp "message"]

    in Line.compare_lines
        [ ("1", '|', "sp")
        , ( "", '|', "^^ ")
        , ( "", '|', " `-- message")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple :: Assertion
case_show_line_multiple =
    let (f, [sp1, _, sp2]) = make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"]
        unds = [error sp1 "a", error sp2 "b"]

    in Line.compare_lines
        [ ("1", '|', "sp1 ABCDEFGHIJKLMNOP sp2")
        , ( "", '|', "^^^                  ^^^ ")
        , ( "", '|', "  `-- a                `-- b")
        ]
        (show_line unds ([], (f, 1)))

case_show_line_multiple_overlapping :: Assertion
case_show_line_multiple_overlapping =
    let (f, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        unds = [error sp1 "message1", note sp1 "message2", hint sp2 "message3"]

    in Line.compare_lines
        [ ("1", '|', "sp1 sp2")
        , ( "", '|', "^^^ --- ")
        , ( "", '|', "  |   `-- message3")
        , ( "", '|', "  |-- message1")
        , ( "", '|', "  `-- message2")
        ]
        (show_line unds ([], (f, 1)))

case_show_row :: Assertion
case_show_row =
    let (_, [sp1, _]) = make_spans ["sp1", "sp2"]
        messages = [CompleteMessage (True, Location.sp_be sp1, Error, "message")]
    in Line.compare_lines
                 -- sp1 sp2
        [("", '|', "  `-- message")]

        [show_row [] messages]

case_show_row_message_below :: Assertion
case_show_row_message_below =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        messages = [CompleteMessage (True, Location.sp_be sp2, Error, "message")]
    in Line.compare_lines
                 -- sp1 sp2
        [("", '|', "  |   `-- message")]

        [show_row [CompleteMessage (True, Location.sp_be sp1, Error, "message")] messages]

case_show_row_not_last :: Assertion
case_show_row_not_last =
    let (_, [sp1, _]) = make_spans ["sp1", "sp2"]
        messages = [CompleteMessage (False, Location.sp_be sp1, Error, "message")]
    in Line.compare_lines
                 -- sp1 sp2
        [("", '|', "  |-- message")]

        [show_row [] messages]

case_show_row_multiple :: Assertion
case_show_row_multiple =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"]
        messages = [CompleteMessage (True, Location.sp_be sp1, Error, "message1"), CompleteMessage (True, Location.sp_be sp2, Error, "message2")]
    in Line.compare_lines
                 -- sp1 ABCDEFGHIJKLMNOP sp2
        [("", '|', "  `-- message1         `-- message2")]

        [show_row [] messages]

case_assign_message_non_overlapping :: Assertion
case_assign_message_non_overlapping =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "                ", "sp2"]

        msg1 = CompleteMessage (False, Location.sp_be sp1, Error, "message 1")
        msg2 = CompleteMessage (False, Location.sp_be sp2, Error, "message 2")

    in [([], [msg2, msg1])] @=? assign_messages [msg2, msg1]

case_assign_message_overlapping :: Assertion
case_assign_message_overlapping =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]

        msg1 = CompleteMessage (False, Location.sp_be sp1, Error, "message 1")
        msg2 = CompleteMessage (False, Location.sp_be sp2, Error, "message 2")

    in [([msg1], [msg2]), ([], [msg1])] @=? assign_messages [msg2, msg1]

case_assign_message_with_no_space_between :: Assertion
case_assign_message_with_no_space_between =
    {-
    this is not allowed:

    sp1ABsp2
    ^^^  ^^^
      `-- a`-- b

    instead, one of the message must be placed a row below:
    sp1ABsp2
    ^^^  ^^^
      |    `-- b
      `-- a
    -}
    let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", "AB", "sp2"]

        msg1 = CompleteMessage (False, Location.sp_be sp1, Error, "a")
        msg2 = CompleteMessage (False, Location.sp_be sp2, Error, "b")

    in [([msg1], [msg2]), ([], [msg1])] @=? assign_messages [msg2, msg1]

test_overlapping :: [TestTree]
test_overlapping =
    let make_test_case name spacing expected =
            let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", spacing, "sp2"]

                msg2 = CompleteMessage (True, Location.sp_be sp2, Error, "b")
                msg1 = CompleteMessage (True, Location.sp_be sp1, Error, "message 1")
            in testCase name $ expected @=? overlapping msg1 [msg2]

    in
        [ make_test_case "end left" "ABCDEFGHIJK" False
            -- sp1ABCDEFGHIJKsp2
            --                 `-- b
            --   `-- message 1

        , make_test_case "end inside" "ABCDEFGH" True
            -- sp1ABCDEFGHsp2
            --              `-- b
            --   `-- message 1

        , make_test_case "end right" "" True
            -- sp1sp2
            --    `-- b
            --   `-- message 1

        , make_test_case "no space between" "ABCDEFGHIJ" True
            -- sp1ABCDEFGHIJsp2
            --                `-- b
            --   `-- message 1
        ]

case_multiline_lines_even :: Assertion
case_multiline_lines_even =
    let (_, [_, _, sp, _]) = make_spans' "file" "" ["\n", "th", "ing\nthingthing\nthing\nab", "c"]
    in Line.compare_lines
        [ ( "", '>', "file")
        , ( "", '|', "    vvv-------")
        , ("2", '|', "  thing      |")
        , ("3", '|', "  thingthing |")
        , ("4", '|', "| thing")
        , ("5", '|', "| abc")
        , ( "", '|', "-^^^")
        , ( "", '|', "   |-- message 1")
        -- , ( "", '|', "   `-- message 2")
        ]
        (show_multiline (error sp "message 1") {-, (Warning, "message 2") -})

case_multiline_lines_odd :: Assertion
case_multiline_lines_odd =
    let (_, [_, _, sp, _]) = make_spans' "file" "" ["\n", "th", "ing\nthingthing\nzyx\nthing\nab", "c"]
    in Line.compare_lines
        [ ( "", '>', "file")
        , ( "", '|', "    vvv-------")
        , ("2", '|', "  thing      |")
        , ("3", '|', "  thingthing |")
        , ("4", '|', "  zyx")
        , ("5", '|', "| thing")
        , ("6", '|', "| abc")
        , ( "", '|', "-^^^")
        , ( "", '|', "   |-- message 1")
        -- , ( "", '|', "   `-- message 2")
        ]
        (show_multiline (error sp "message 1") {- warning sp "message 2" -}) -- TODO: grouping multiple together

tests :: TestTree
tests = $(testGroupGenerator)
