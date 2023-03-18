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

        multiline_grouped = map (\ msgs@((sp, _, _):_) -> (sp, map (\ (_, ty, text) -> (ty, text)) msgs)) $ List.groupBy ((==) `on` (\ (sp, _, _) -> sp)) multiline

        singleline' = show_singleline style singleline
        multiline' = concatMap (uncurry $ show_multiline style) multiline_grouped
        nospan' = concatMap (show_nospan style) nospan
    in singleline' ++ multiline' ++ nospan'

-- show_singleline {{{1
-- Message helpers {{{2
type RenderMessage = (Location.Location, Diagnostic.MessageType, Text)

format_render_message :: Style.Style -> Bool -> RenderMessage -> FormattedString.FormattedString
format_render_message style is_last (_, ty, text) = FormattedString.color_text (type_color ty style) (Text.pack [if is_last then Style.message_prefix_last style else Style.message_prefix_line style] <> Style.message_prefix style <> " " <> text)

rm_start_col :: RenderMessage -> Int
rm_start_col (loc, _, _) = Location.loc_col loc
rm_end_col :: Style.Style -> RenderMessage -> Int
-- [char][prefix] message
-- for example: in '`-- message' the char is '`' and the prefix is '--'
-- +1 for the char, +1 for the space after the prefix
rm_end_col style (loc, _, text) = Location.loc_col loc + 1 + Text.length (Style.message_prefix style) + 1 + Text.length text
-- show_singleline {{{2
show_singleline :: Style.Style -> [MessageWithSpan] -> [Line.Line]
show_singleline style unds =
    let grouped = Utils.context_lines $ Utils.group_by_spans (\ (sp, _, _) -> sp) unds
        flattened =
            concatMap
                (\ (file, lines) ->
                    map
                        (\ (line, unds) -> (Utils.un_fcbp file, line, unds))
                        (Map.toAscList lines))
                (Map.toAscList grouped)

    in concatMap (show_line style) $ zip (Nothing : map Just flattened) flattened
-- show_line {{{2
show_line :: Style.Style -> (Maybe (File, Int, [MessageWithSpan]), (File, Int, [MessageWithSpan])) -> [Line.Line]
show_line style (last, (fl, nr, messages)) =
    let render_messages = get_render_messages messages
        msg_rows = assign_messages style render_messages

        (quote, underline_line) = get_colored_quote_and_underline_line style fl nr messages

    in Utils.file_and_elipsis_lines style ((\ (lastfl, lastnr, _) -> (lastfl, lastnr)) <$> last) (fl, nr) ++
        [Line.numbered_line style nr quote] ++
        (if not $ null messages
            then [Line.other_line style underline_line]
            else []) ++
        map (uncurry $ show_msg_row style) msg_rows

get_render_messages :: [MessageWithSpan] -> [RenderMessage]
get_render_messages =
    List.sortBy (flip $ comparing rm_start_col) .
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
               , FormattedString.Literal (Text.pack $ map (\ c -> if c `elem` below_pipes then Style.message_below_char style else ' ') [last_col..start_col - 1])
                   <> format_render_message style (start_col `notElem` below_pipes) msg
               )

    in Line.other_line style $ foldl' (<>) "" $ snd $ List.mapAccumL render_msg 1 sorted_msgs
-- assigning rows {{{3
assign_messages :: Style.Style -> [RenderMessage] -> [([RenderMessage], [RenderMessage])]
assign_messages style msgs =
    let assignments = reverse $ foldl' assign [] msgs
        rows = map (map fst) $ List.groupBy ((==) `on` snd) $ List.sortBy (comparing snd) assignments
        belows = map concat $ drop 1 $ List.tails rows
    in zip belows rows
    where
        assign assigned msg =
            let working_row = Maybe.fromJust $ List.find (try_row assigned msg) [0..]
            in (msg, working_row :: Int) : assigned

        try_row already_assigned msg row =
            let messages_on_row = map fst $ filter ((row==) . snd) already_assigned
            in not $ overlapping style msg messages_on_row

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
show_multiline :: Style.Style -> Span -> [(Diagnostic.MessageType, Maybe Text)] -> [Line.Line]
show_multiline style sp msgs =
    let
        start_line = Span.start_row sp
        end_line = Span.before_end_row sp

        (main_ty, _) = head msgs

        n_lines = (end_line + 1) - start_line
        n_vertical_lines = n_lines `div` 2
        mid_line =
            if odd n_lines
                then Just (Span.file sp, (start_line + end_line) `div` 2)
                else Nothing

        und_char = type_char main_ty style
        rev_und_char = top_type_char main_ty style
        sgr = type_color main_ty style

    in
        [Line.file_line style (Span.file sp)] ++
        show_top_lines style (Span.start sp) n_vertical_lines rev_und_char sgr ++
        show_middle_line style mid_line sgr ++
        show_bottom_lines style (Span.before_end sp) n_vertical_lines und_char sgr msgs

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
            FormattedString.Literal (Text.replicate (start_col + 2 - 1) " ") <> FormattedString.color_text sgr (Text.replicate n_ch (Text.pack [ch])) <> FormattedString.color_text (Style.multiline_other_color style) (Text.replicate (n_dash - 1) (Text.singleton $ Style.multiline_other_char style) <> Text.singleton (Style.multiline_tr_char style))] ++
        [ Line.numbered_line style start_line $
            "  " <> FormattedString.Literal (Text.take (start_col - 1) start_quote) <> FormattedString.color_text sgr (Text.drop (start_col - 1) start_quote) <> FormattedString.Literal (Text.replicate (max_col - Text.length start_quote - 1) " ") <> FormattedString.color_text (Style.multiline_other_color style) (Text.singleton $ Style.multiline_vertical_char style)] ++
        map (\ l ->
            let quote = Utils.get_quote file l
                pad = max_col - Text.length quote - 1
            in Line.numbered_line style l $ "  " <> FormattedString.color_text sgr quote <> FormattedString.Literal (Text.replicate pad " ") <> FormattedString.color_text (Style.multiline_other_color style) (Text.singleton $ Style.multiline_vertical_char style)
        ) top_lines

show_bottom_lines :: Style.Style -> Location.Location -> Int -> Char -> [ANSI.SGR] -> [(Diagnostic.MessageType, Maybe Text)] -> [Line.Line]
show_bottom_lines style loc n ch sgr msgs =
    let end_col = Location.loc_col loc + 2
        end_line = Location.loc_row loc
        file = Location.loc_file loc

        end_quote = Utils.get_quote file end_line

        n_ch = min 3 end_col
        n_dash = end_col - n_ch

        bottom_lines = [Location.loc_row loc - n + 1 .. Location.loc_row loc - 1]

        msgs_with_text = mapMaybe (\ (ty, msg) -> (ty,) <$> msg) msgs

    in map (\ l ->
            let quote = Utils.get_quote file l
            in Line.numbered_line style l $ FormattedString.color_text (Style.multiline_other_color style) (Text.singleton $ Style.multiline_vertical_char style) <> " " <> FormattedString.color_text sgr quote
        ) bottom_lines ++
        [ Line.numbered_line style end_line $
            FormattedString.color_text (Style.multiline_other_color style) (Text.singleton $ Style.multiline_vertical_char style) <> " " <> FormattedString.color_text sgr (Text.take (end_col - 2) end_quote) <> FormattedString.Literal (Text.drop (end_col - 2) end_quote)] ++
        [ Line.other_line style $
            FormattedString.color_text (Style.multiline_other_color style) (Text.singleton (Style.multiline_bl_char style) <> Text.replicate (n_dash - 1) (Text.singleton $ Style.multiline_other_char style)) <> FormattedString.color_text sgr (Text.replicate n_ch (Text.singleton ch))] ++
        zipWith (\ i (msg_ty, msg_text) ->
            Line.other_line style $
                FormattedString.Literal (Text.replicate (end_col - 1) " ") <> format_render_message style (i == length msgs - 1) (loc, msg_ty, msg_text)
        ) [0..] msgs_with_text

show_middle_line :: Style.Style -> Maybe (File, Int) -> [ANSI.SGR] -> [Line.Line]
show_middle_line style (Just (file, nr)) sgr = [Line.numbered_line style nr $ "  " <> FormattedString.color_text sgr (Utils.get_quote file nr)]
show_middle_line _ Nothing _ = []
-- show_nospan {{{1
show_nospan :: Style.Style -> (Diagnostic.MessageType, Maybe Text) -> [Line.Line]
show_nospan style (ty, Just msg) = [Line.heading_line style $ FormattedString.color_text (type_color ty style) msg]
show_nospan _ (_, Nothing) = []
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
                ] -- TODO: test no span messages and no message messages
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
        [ (   "", '>', "abc")
        , (  "1", '|', "abc1abc2")
        , (   "", '|', "^^^^---- ")
        , (   "", '|', "   |   `-- note")
        , (   "", '|', "   `-- warning")
        , ("...", '|', "...")
        , (  "6", '|', "context1")
        , (  "7", '|', "context2")
        , (  "8", '|', "abc3")
        , (   "", '|', "-----")
        , (   "", '|', "    `-- hint")
        , (  "9", '|', "context3")
        , ( "10", '|', "context4")
        , (   "", '>', "zyx")
        , (  "1", '|', "zyx1")
        , (   "", '|', "^^^^ ")
        , (   "", '|', "   `-- error")
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

case_show_line_assign_out_of_order :: Assertion
case_show_line_assign_out_of_order =
     make_spans ["sp1", "abcdefghijklmnop", "sp2", "sp3"] >>= \ (f, [sp1, _, sp2, sp3]) ->

    Line.compare_lines
        [ ("1", '|', "sp1 abcdefghijklmnop sp2 sp3")
        , ( "", '|', "^^^                  --- --- ")
        , ( "", '|', "  `-- message1         |   `-- message3")
        , ( "", '|', "                       `-- message2")
        ]
        (show_line Style.default_style (Nothing, (f, 1, [(sp1, Diagnostic.MsgError, Just "message1"), (sp2, Diagnostic.MsgNote, Just "message2"), (sp3, Diagnostic.MsgHint, Just "message3")])))

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

-- similar to case_show_line_assign_out_of_order above
case_assign_messages_out_of_order :: Assertion
case_assign_messages_out_of_order =
    make_spans ["sp1", "abcdefghijklmnop", "sp2", "sp3"] >>= \ (_, [sp1, _, sp2, sp3]) ->
    let msg1 = (Span.before_end sp1, Diagnostic.MsgError, "message1")
        msg2 = (Span.before_end sp2, Diagnostic.MsgError, "message2")
        msg3 = (Span.before_end sp3, Diagnostic.MsgError, "message3")
    in [([msg2], [msg3, msg1]), ([], [msg2])] @=? assign_messages Style.default_style [msg3, msg2, msg1]

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
        , ( "", '|', "   `-- message 2")
        ]
        (show_multiline Style.default_style sp [(Diagnostic.MsgError, Just "message 1"), (Diagnostic.MsgWarning, Just "message 2")])

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
        , ( "", '|', "   `-- message 2")
        ]
        (show_multiline Style.default_style sp [(Diagnostic.MsgError, Just "message 1"), (Diagnostic.MsgWarning, Just "message 2")])

tests :: TestTree
tests = $(testGroupGenerator)
