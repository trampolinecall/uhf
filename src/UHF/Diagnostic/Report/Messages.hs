{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic.Report.Messages
    ( render
    , tests
    ) where

import UHF.Util.Prelude hiding (error)

import UHF.IO.Location.SpanHelper

import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report.Line as Line
import qualified UHF.Diagnostic.Report.Utils as Utils
import qualified UHF.Diagnostic.Report.Style as Style

import qualified UHF.IO.FormattedString as FormattedString
import UHF.IO.File (File)
import qualified UHF.IO.Location as Location
import UHF.IO.Span (Span)
import qualified UHF.IO.Span as Span

import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map as Map
import qualified System.Console.ANSI as ANSI

-- TODO: fix errors on eof where messages arent shown
-- TODO: fix column counting that doesnt work with unicode

type MessageWithSpan = (Span, Diagnostic.MessageType, Maybe Text)

type_color :: Diagnostic.MessageType -> Style.Style -> [ANSI.SGR]
type_color Diagnostic.MsgError = Style.error_color
type_color Diagnostic.MsgWarning = Style.warning_color
type_color Diagnostic.MsgNote = Style.note_color
type_color Diagnostic.MsgHint = Style.hint_color

type_char, top_type_char :: Diagnostic.MessageType -> Style.Style -> Char
type_char Diagnostic.MsgError = Style.msg_error_char
type_char Diagnostic.MsgWarning = Style.msg_warning_char
type_char Diagnostic.MsgNote = Style.msg_note_char
type_char Diagnostic.MsgHint = Style.msg_hint_char

top_type_char Diagnostic.MsgError = Style.msg_error_char_top
top_type_char Diagnostic.MsgWarning = Style.msg_warning_char_top
top_type_char Diagnostic.MsgNote = Style.msg_note_char_top
top_type_char Diagnostic.MsgHint = Style.msg_hint_char_top

render :: Style.Style -> Diagnostic.MessagesSection -> [Line.Line]
render style msgs =
    let (nospan, withspan) =
            Either.partitionEithers $
            map (\case
                    (Just s, ty, msg) -> Right (s, ty, msg)
                    (Nothing, ty, msg) -> Left (ty, msg)
                )
                msgs
        (singleline, multiline) = List.partition (Span.is_single_line . (\ (a, _, _) -> a)) withspan

        singleline' = (show_singleline style) singleline
        multiline' = concatMap (show_multiline style) multiline
        nospan' =
            concatMap (\case
                    (ty, Just msg) -> [Line.heading_line style $ FormattedString.color_text (type_color ty style) msg]
                    (_, Nothing) -> []
                )
                nospan -- TODO: refactor this as well as the whole module and all the reporting code
    in singleline' ++ multiline' ++ nospan'

-- show_singleline {{{1
-- Message helpers {{{2
type RenderMessage = (Location.Location, Diagnostic.MessageType, Text)

format_render_message :: Style.Style -> Bool -> RenderMessage -> FormattedString.FormattedString
format_render_message style is_last (_, ty, text) = FormattedString.color_text (type_color ty style) (Text.pack [if is_last then Style.message_prefix_last style else Style.message_prefix_line style] <> Style.message_prefix style <> text)

rm_start_col :: RenderMessage -> Int
rm_start_col (loc, _, _) = Location.loc_col loc
rm_end_col :: Style.Style -> RenderMessage -> Int
-- [char][prefix] message
-- for example: in '`-- message' the char is '`' and the prefix is '--'
-- +1 for the char, +1 for the prefix
rm_end_col style (loc, _, text) = Location.loc_col loc + Text.length text + 1 + 1 + Text.length (Style.message_prefix style)
-- show_singleline {{{2
show_singleline :: Style.Style -> [MessageWithSpan] -> [Line.Line]
show_singleline style unds =
    let grouped = Utils.group_by_spans (\ (sp, _, _) -> sp) unds
        flattened =
            concatMap
                (\ (file, lines) ->
                    map
                        (\ (line, unds) -> (Utils.un_fcbp file, line, unds))
                        (Map.toAscList lines))
                (Map.toAscList grouped)

    in concatMap (show_line style) $ zip (Nothing : map Just flattened) flattened
    -- concatMap (concatMap show_line . Map.toAscList) $
    -- Utils.file_and_elipsis_lines identity $ TODO: context lines
    -- List.sortBy Utils.flnr_comparator $
    -- List.nub $
    -- concatMap (uncurry Utils.context_lines) $
-- show_line {{{2
show_line :: Style.Style -> (Maybe (File, Int, [MessageWithSpan]), (File, Int, [MessageWithSpan])) -> [Line.Line]
show_line style (last, (fl, nr, messages)) =
    let renderable_messages = get_renderable_messages messages
        msg_rows = assign_messages style renderable_messages

        (quote, underline_line) = get_colored_quote_and_underline_line style fl nr messages

    in Utils.file_and_elipsis_lines style ((\ (lastfl, lastnr, _) -> (lastfl, lastnr)) <$> last) (fl, nr) ++
        [Line.numbered_line style nr quote] ++
        (if not $ null messages
            then [Line.other_line style underline_line]
            else []) ++

        map (uncurry $ show_msg_row style) msg_rows

get_renderable_messages :: [MessageWithSpan] -> [RenderMessage]
get_renderable_messages =
    List.sortBy (flip compare `on` rm_start_col) .
    Maybe.mapMaybe (\ (sp, ty, msg) -> (Span.before_end sp, ty,) <$> msg)

get_colored_quote_and_underline_line :: Style.Style -> File -> Int -> [MessageWithSpan] -> (FormattedString.FormattedString, FormattedString.FormattedString)
get_colored_quote_and_underline_line style fl nr unds =
    let col_in_underline c (sp, _, _) = Span.start_col sp <= c && c <= Span.before_end_col sp

        message_type (_, ty, _) = ty

        underline_for_cols = map (\ c -> message_type <$> find (col_in_underline c) unds) [1 .. length quote + 1]

        quote = Text.unpack $ Utils.get_quote fl nr
        colored_quote =
            foldl' (<>) "" $
            zipWith
                (\ ch m_und ->
                    case m_und of
                        Nothing -> FormattedString.Literal $ Text.pack [ch]
                        Just ty -> FormattedString.color_text (type_color ty style) (Text.pack [ch])
                )
                quote underline_for_cols

        underline_line =
            foldl' (<>) "" $
            map
                (\case
                    Nothing -> " "
                    Just ty -> FormattedString.color_text (type_color ty style) (Text.pack [type_char ty style])
                )
                underline_for_cols

    in (colored_quote, underline_line)

show_msg_row :: Style.Style -> [RenderMessage] -> [RenderMessage] -> Line.Line
show_msg_row style below msgs =
    let below_pipes = map rm_start_col below
        sorted_msgs = List.sortBy (compare `Function.on` rm_start_col) msgs

        render_msg last_col msg =
            let start_col = rm_start_col msg
                end_col = rm_end_col style msg
            in ( end_col
               , FormattedString.Literal (Text.pack $ map (\ c -> if c `elem` below_pipes then '|' else ' ') [last_col..start_col - 1])
                   <> format_render_message style (start_col `notElem` below_pipes) msg
               )

    in Line.other_line style $ foldl' (<>) "" $ snd $ List.mapAccumL render_msg 1 sorted_msgs
-- assigning rows {{{3
assign_messages :: Style.Style -> [RenderMessage] -> [([RenderMessage], [RenderMessage])]
assign_messages style msgs =
    let assignments =
            zipWith
                (\ cur_msg prev_assignments ->
                    -- head should never fail because there will always be a next row number that doesn't have any previously assigned messages to overlap with the current one
                    -- TODO: rewrite this with recursive function because i think that was clearer
                    let row = head $ filter (\ test_row -> not $ overlapping style cur_msg $ map fst $ filter ((==test_row) . snd) prev_assignments) ([0..] :: [Int])
                    in (cur_msg, row))
                msgs
                (List.inits assignments)

        rows = map (map fst) $ List.groupBy ((==) `on` snd) assignments
        belows = map concat $ drop 1 $ List.tails rows

    in zip belows rows

overlapping :: Style.Style -> RenderMessage -> [RenderMessage] -> Bool
overlapping style to_assign assigned =
    let to_assign_start_col = rm_start_col to_assign
        to_assign_end_col = rm_end_col style to_assign
    in any
        (\ already ->
            let already_start = rm_start_col already
                already_end = rm_end_col style already
            in not $
                (to_assign_start_col < already_start && to_assign_end_col < already_start) || -- completely to the left of the already assigned message
                (to_assign_start_col > already_end && to_assign_end_col < already_end) -- completely to the right of the already assigned message
        )
        assigned
-- show_multiline {{{1
show_multiline :: Style.Style -> MessageWithSpan -> [Line.Line]
show_multiline style (und_sp, und_type, und_msg) = -- TODO: merge messages with the same span
    let
        start_line = Span.start_row und_sp
        end_line = Span.before_end_row und_sp

        n_lines = (end_line + 1) - start_line
        n_vertical_lines = n_lines `div` 2
        mid_line =
            if odd n_lines
                then Just (Span.file und_sp, (start_line + end_line) `div` 2)
                else Nothing

        und_char = type_char und_type style
        rev_und_char = top_type_char und_type style
        sgr = type_color und_type style

    in
        [Line.file_line style (Span.file und_sp)] ++
        show_top_lines style (Span.start und_sp) n_vertical_lines rev_und_char sgr ++
        show_middle_line style mid_line sgr ++
        show_bottom_lines style (Span.before_end und_sp) n_vertical_lines und_char und_type und_msg -- TODO: see todo at the beginning of this function

show_top_lines :: Style.Style -> Location.Location -> Int -> Char -> [ANSI.SGR] -> [Line.Line]
show_top_lines style loc n ch sgr =
    let start_col = Location.loc_col loc
        start_line = Location.loc_row loc
        file = Location.loc_file loc

        start_quote = Utils.get_quote file start_line

        top_lines = [Location.loc_row loc + 1 .. Location.loc_row loc + n - 1]
        max_col = maximum (map (Text.length . Utils.get_quote file) (start_line : top_lines)) + 2

        col_diff = max_col - start_col + 1
        n_ch = min 3 col_diff
        n_dash = col_diff - n_ch

    in [ Line.other_line style $
            FormattedString.Literal (Text.replicate (start_col + 2 - 1) " ") <> FormattedString.color_text sgr (Text.replicate n_ch (Text.pack [ch])) <> FormattedString.color_text (Style.und_other_color style) (Text.replicate n_dash "-")] ++
        [ Line.numbered_line style start_line $
            "  " <> FormattedString.Literal (Text.take (start_col - 1) start_quote) <> FormattedString.color_text sgr (Text.drop (start_col - 1) start_quote) <> FormattedString.Literal (Text.replicate (max_col - Text.length start_quote - 1) " ") <> FormattedString.color_text (Style.und_other_color style) "|"] ++
        map (\ l ->
            let quote = Utils.get_quote file l
                pad = max_col - Text.length quote - 1
            in Line.numbered_line style l $ "  " <> FormattedString.color_text sgr quote <> FormattedString.Literal (Text.replicate pad " ") <> FormattedString.color_text (Style.und_other_color style) "|"
        ) top_lines

show_bottom_lines :: Style.Style -> Location.Location -> Int -> Char -> Diagnostic.MessageType -> Maybe Text -> [Line.Line]
show_bottom_lines style loc n ch ty msg =
    let end_col = Location.loc_col loc + 2
        end_line = Location.loc_row loc
        file = Location.loc_file loc

        end_quote = Utils.get_quote file end_line

        n_ch = min 3 end_col
        n_dash = end_col - n_ch

        bottom_lines = [Location.loc_row loc - n + 1 .. Location.loc_row loc - 1]

        sgr = type_color ty style

        -- TODO: refactor this because this is badly adapted to the message overhaul
    in map (\ l ->
            let quote = Utils.get_quote file l
            in Line.numbered_line style l $ FormattedString.color_text (Style.und_other_color style) "| " <> FormattedString.color_text sgr quote
        ) bottom_lines ++
        [ Line.numbered_line style end_line $
            FormattedString.color_text (Style.und_other_color style) "| " <> FormattedString.color_text sgr (Text.take (end_col - 2) end_quote) <> FormattedString.Literal (Text.drop (end_col - 2) end_quote)] ++
        [ Line.other_line style $
            FormattedString.color_text (Style.und_other_color style) (Text.replicate n_dash "-") <> FormattedString.color_text sgr (Text.replicate n_ch (Text.pack [ch]))] ++
        zipWith (\ i msg ->
            Line.other_line style $
                FormattedString.Literal (Text.replicate (end_col - 1) " ") <> format_render_message style (i == (0 :: Int) {- length msgs -}) (loc, ty, msg)
        ) [0..] (Maybe.maybeToList msg)

show_middle_line :: Style.Style -> Maybe (File, Int) -> [ANSI.SGR] -> [Line.Line]
show_middle_line style (Just (file, nr)) sgr = [Line.numbered_line style nr $ "  " <> FormattedString.color_text sgr (Utils.get_quote file nr)]
show_middle_line _ Nothing _ = []
-- tests {{{1
case_empty :: Assertion
case_empty =
    let lines = render Style.default_style []
    in Line.compare_lines [] lines

case_messages :: Assertion
case_messages =
    make_spans ["abc", "def\nghi\njklm\n"] >>= \ (_, [single_sp, multi_sp]) ->

    let lines = render Style.default_style
            (
                [ (Just single_sp, Diagnostic.MsgError, Just "message 1"), (Just single_sp, Diagnostic.MsgHint, Just "message 2")
                , (Just multi_sp, Diagnostic.MsgWarning, Just "message 3")
                ] :: Diagnostic.MessagesSection -- TODO: test no span messages and no message messages
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
    make_spans' "abc" "" ["abc1", "abc2", "\n", "\n", "\n", "\n", "\n", "context1\n", "context2\n", "abc3\n", "context3\n", "context4\n"] >>= \ (_, [abc1, abc2, _, _, _, _, _, _, _, abc3, _, _]) ->
    make_spans' "zyx" "" ["zyx1"] >>= \ (_, [zyx1]) ->

    let unds =
            [ (zyx1, Diagnostic.MsgError, Just "error")
            , (abc3, Diagnostic.MsgHint, Just "hint")
            , (abc1, Diagnostic.MsgWarning, Just "warning")
            , (abc2, Diagnostic.MsgNote, Just "note")
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
        (show_singleline Style.default_style unds)

case_show_line_single :: Assertion
case_show_line_single =
     make_spans ["sp"] >>= \ (f, [sp]) ->

    Line.compare_lines
        [ ( "", '>', "")
        , ("1", '|', "sp")
        , ( "", '|', "^^ ")
        , ( "", '|', " `-- message")
        ]
        (show_line Style.default_style (Nothing, (f, 1, [(sp, Diagnostic.MsgError, Just "message")])))

case_show_line_multiple :: Assertion
case_show_line_multiple =
     make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"] >>= \ (f, [sp1, _, sp2]) ->

    Line.compare_lines
        [ ( "", '>', "")
        , ("1", '|', "sp1 ABCDEFGHIJKLMNOP sp2")
        , ( "", '|', "^^^                  ^^^ ")
        , ( "", '|', "  `-- a                `-- b")
        ]
        (show_line Style.default_style (Nothing, (f, 1, [(sp1, Diagnostic.MsgError, Just "a"), (sp2, Diagnostic.MsgError, Just "b")])))

case_show_line_multiple_overlapping :: Assertion
case_show_line_multiple_overlapping =
     make_spans ["sp1", "sp2"] >>= \ (f, [sp1, sp2]) ->

    Line.compare_lines
        [ ("1", '|', "sp1 sp2")
        , ( "", '|', "^^^ --- ")
        , ( "", '|', "  |   `-- message3")
        , ( "", '|', "  |-- message1")
        , ( "", '|', "  `-- message2")
        ]
        (show_line Style.default_style (Nothing, (f, 1, [(sp1, Diagnostic.MsgError, Just "message1"), (sp1, Diagnostic.MsgNote, Just "message2"), (sp2, Diagnostic.MsgHint, Just "message3")])))

case_show_msg_row :: Assertion
case_show_msg_row =
    make_spans ["sp1", "sp2"] >>= \ (_, [sp1, _]) ->
    let messages = [(Span.before_end sp1, Diagnostic.MsgError, "message")]
    in Line.compare_lines
                 -- sp1 sp2
        [("", '|', "  `-- message")]

        [show_msg_row Style.default_style [] messages]

case_show_msg_row_message_below :: Assertion
case_show_msg_row_message_below =
    make_spans ["sp1", "sp2"] >>= \ (_, [sp1, sp2]) ->
    let messages = [(Span.before_end sp2, Diagnostic.MsgError, "message")]
    in Line.compare_lines
                 -- sp1 sp2
        [("", '|', "  |   `-- message")]

        [show_msg_row Style.default_style [(Span.before_end sp1, Diagnostic.MsgError, "message")] messages]

case_show_msg_row_multiple :: Assertion
case_show_msg_row_multiple =
    make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"] >>= \ (_, [sp1, _, sp2]) ->
    let messages = [(Span.before_end sp1, Diagnostic.MsgError, "message1"), (Span.before_end sp2, Diagnostic.MsgError, "message2")]
    in Line.compare_lines
                 -- sp1 ABCDEFGHIJKLMNOP sp2
        [("", '|', "  `-- message1         `-- message2")]

        [show_msg_row Style.default_style [] messages]

case_assign_message_non_overlapping :: Assertion
case_assign_message_non_overlapping =
    make_spans ["sp1", "                ", "sp2"] >>= \ (_, [sp1, _, sp2]) ->

    let msg1 = (Span.before_end sp1, Diagnostic.MsgError, "message 1")
        msg2 = (Span.before_end sp2, Diagnostic.MsgError, "message 2")

    in [([], [msg2, msg1])] @=? assign_messages Style.default_style [msg2, msg1]

case_assign_message_overlapping :: Assertion
case_assign_message_overlapping =
    make_spans ["sp1", "sp2"] >>= \ (_, [sp1, sp2]) ->

    let msg1 = (Span.before_end sp1, Diagnostic.MsgError, "message 1")
        msg2 = (Span.before_end sp2, Diagnostic.MsgError, "message 2")

    in [([msg1], [msg2]), ([], [msg1])] @=? assign_messages Style.default_style [msg2, msg1]

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
    make_spans' "f" "" ["sp1", "AB", "sp2"] >>= \ (_, [sp1, _, sp2]) ->

    let msg1 = (Span.before_end sp1, Diagnostic.MsgError, "a")
        msg2 = (Span.before_end sp2, Diagnostic.MsgError, "b")

    in [([msg1], [msg2]), ([], [msg1])] @=? assign_messages Style.default_style [msg2, msg1]

test_overlapping :: [TestTree]
test_overlapping =
    let make_test_case name spacing expected =
            testCase name $
                make_spans' "f" "" ["sp1", spacing, "sp2"] >>= \ (_, [sp1, _, sp2]) ->
                let msg2 = (Span.before_end sp2, Diagnostic.MsgError, "b")
                    msg1 = (Span.before_end sp1, Diagnostic.MsgError, "message 1")
                in expected @=? overlapping Style.default_style msg1 [msg2]

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
    make_spans' "file" "" ["\n", "th", "ing\nthingthing\nthing\nab", "c"] >>= \ (_, [_, _, sp, _]) ->
    Line.compare_lines
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
        (show_multiline Style.default_style ((sp, Diagnostic.MsgError, Just "message 1")) {-, (Warning, Just "message 2") -})

case_multiline_lines_odd :: Assertion
case_multiline_lines_odd =
     make_spans' "file" "" ["\n", "th", "ing\nthingthing\nzyx\nthing\nab", "c"] >>= \ (_, [_, _, sp, _]) ->
    Line.compare_lines
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
        (show_multiline Style.default_style ((sp, Diagnostic.MsgError, Just "message 1")) {- (sp, Diagnostic.MsgWarning, "message 2") -}) -- TODO: grouping multiple together

tests :: TestTree
tests = $(testGroupGenerator)
