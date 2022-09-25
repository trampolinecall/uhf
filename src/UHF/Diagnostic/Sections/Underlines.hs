{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Underlines
    ( UnderlinesSection
    , Underline
    , Importance(..)
    , Type(..)
    , underlines

    , primary, secondary, tertiary
    , UHF.Diagnostic.Sections.Underlines.error, warning, note, hint

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import UHF.IO.Location.SpanHelper

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

import qualified Safe

type UnderlinesSection = [Underline]
type Underline = (Location.Span, Importance, [(Type, Text.Text)])
data Importance = Primary | Secondary | Tertiary
data Type = Error | Warning | Note | Hint deriving (Show, Eq)

type_color :: Type -> [ANSI.SGR]
type_color Error = Colors.error
type_color Warning = Colors.warning
type_color Note = Colors.note
type_color Hint = Colors.hint

imp_char, top_imp_char :: Importance -> Char
imp_char Primary = '^'
imp_char Secondary = '~'
imp_char Tertiary = '-'

top_imp_char Primary = 'v'
top_imp_char Secondary = '~'
top_imp_char Tertiary = '-'

primary, secondary, tertiary :: Location.Span -> [(Type, Text.Text)] -> Underline
primary s m = (s, Primary, m)
secondary s m = (s, Secondary, m)
tertiary s m = (s, Tertiary, m)

error, warning, note, hint :: Text.Text -> (Type, Text.Text)
error = (Error,)
warning = (Warning,)
note = (Note,)
hint = (Hint,)

underlines :: UnderlinesSection -> Diagnostic.Section
underlines unds =
    let (singleline, multiline) = List.partition (Location.is_single_line . (\ (a, _, _) -> a)) unds

        singleline' = show_singleline singleline
        multiline' = concatMap show_multiline multiline
    in Diagnostic.to_section $ singleline' ++ multiline'

-- show_singleline {{{1
-- Message helpers {{{2
type CompleteMessage = (Bool, Location.Location, Type, Text.Text)

cm_start_col :: CompleteMessage -> Int
cm_start_col (_, loc, _, _) = Location.col loc
-- `-- message
-- 4 ('`-- ') + length of message
cm_end_col :: CompleteMessage -> Int
cm_end_col (_, loc, _, text) = Location.col loc + Text.length text + 4

str_message :: Bool -> Type -> Text.Text -> ([ANSI.SGR], Text.Text)
str_message is_last ty text = (type_color ty, Text.concat [if is_last then "`" else "|", "-- ", text])
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
lines_shown = map (\ (sp, _, _) -> (Location.file $ Location.start sp, Location.row $ Location.start sp))
-- show_line {{{2
get_complete_messages :: [Underline] -> [CompleteMessage]
get_complete_messages =
    List.sortBy (flip compare `Function.on` cm_start_col) .
    concatMap (\ (sp, _, msgs) -> map (\ (i, (ty, tx)) -> (i == length msgs - 1, Location.before_end sp, ty, tx)) $ zip [0..] msgs)

get_colored_quote_and_underline_line :: File.File -> Int -> [Underline] -> (FormattedString.FormattedString, FormattedString.FormattedString)
get_colored_quote_and_underline_line fl nr unds =
    let col_in_underline c (sp, _, _) = Location.col (Location.start sp) <= c && c <= Location.col (Location.before_end sp)

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

show_row :: [CompleteMessage] -> [CompleteMessage] -> Line.Line
show_row below msgs =
    let below_pipes = map cm_start_col below
        sorted_msgs = List.sortBy (compare `Function.on` cm_start_col) msgs

        render_msg last_col msg@(is_last, _, ty, text) =
            let start_col = cm_start_col msg
                end_col = cm_end_col msg
            in ( end_col
               , [ ([], Text.pack $ map (\ c -> if c `elem` below_pipes then '|' else ' ') [last_col..start_col - 1])
                 , str_message is_last ty text
                 ]
               )

    in Line.other_line $ FormattedString.make_formatted_string $ concat $ snd $ List.mapAccumL render_msg 1 sorted_msgs

show_line :: [Underline] -> ([Line.Line], (File.File, Int)) -> [Line.Line]
show_line unds (other_lines, (fl, nr)) =
    let cur_line_unds = filter (\ (sp, _, _) -> Location.file (Location.start sp) == fl && Location.row (Location.start sp) == nr) unds

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

        rows = map (map fst) $ List.groupBy ((==) `Function.on` snd) assignments
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
show_multiline (und_sp, und_importance, und_msgs) =
    let
        start_line = Location.line $ Location.start und_sp
        end_line = Location.line $ Location.before_end und_sp

        n_lines = (end_line + 1) - start_line
        n_vertical_lines = n_lines `div` 2
        mid_line =
            if odd n_lines
                then Just $ (Location.file $ Location.start und_sp, (start_line + end_line) `div` 2)
                else Nothing

        und_imp_char = imp_char und_importance
        rev_und_imp_char = top_imp_char und_importance

        und_sgr = maybe ([Colors.bold]) type_color (fst <$> Safe.headMay und_msgs)

    in
        [Line.file_line (Location.file $ Location.start und_sp)] ++
        show_top_lines (Location.start und_sp) n_vertical_lines rev_und_imp_char und_sgr ++
        show_middle_line mid_line und_sgr ++
        show_bottom_lines (Location.before_end und_sp) n_vertical_lines und_imp_char und_sgr und_msgs

show_top_lines :: Location.Location -> Int -> Char -> [ANSI.SGR] -> [Line.Line]
show_top_lines loc n ch sgr =
    let start_col = Location.col loc
        start_line = Location.line loc
        file = Location.file loc

        start_quote = Utils.get_quote file start_line

        top_lines = [Location.line loc + 1 .. Location.line loc + n - 1]
        max_col = maximum (map (Text.length . Utils.get_quote (file)) (start_line : top_lines)) + 2

        col_diff = max_col - start_col + 1
        n_ch = min 3 col_diff
        n_dash = col_diff - n_ch

    in [ Line.other_line $ FormattedString.make_formatted_string
            [([], Text.replicate (start_col + 2 - 1) " "), (sgr, Text.replicate n_ch (Text.pack [ch])), ([Colors.bold], Text.replicate n_dash "-")]] ++
        [ Line.numbered_line start_line $ FormattedString.make_formatted_string
            [([], "  "), ([], Text.take (start_col - 1) start_quote), (sgr, Text.drop (start_col - 1) start_quote), ([], Text.replicate (max_col - Text.length start_quote - 1) " "), ([Colors.bold], "|")]] ++
        map (\ l ->
            let quote = Utils.get_quote (file) l
                pad = max_col - Text.length quote - 1
            in Line.numbered_line l $ FormattedString.make_formatted_string [([], "  "), (sgr, quote), ([], Text.replicate pad " "), ([Colors.bold], "|")]
        ) top_lines

show_bottom_lines :: Location.Location -> Int -> Char -> [ANSI.SGR] -> [(Type, Text.Text)] -> [Line.Line]
show_bottom_lines loc n ch sgr msgs =
    let end_col = Location.col loc + 2
        end_line = Location.line loc
        file = Location.file loc

        end_quote = Utils.get_quote file end_line

        n_ch = min 3 end_col
        n_dash = end_col - n_ch

        bottom_lines = [Location.line loc - n + 1 .. Location.line loc - 1]

    in map (\ l ->
            let quote = Utils.get_quote file l
            in Line.numbered_line l $ FormattedString.make_formatted_string [([Colors.bold], "| "), (sgr, quote)]
        ) bottom_lines ++
        [ Line.numbered_line end_line $ FormattedString.make_formatted_string
            [([Colors.bold], "| "), (sgr, Text.take (end_col - 2 ) end_quote), ([], Text.drop (end_col - 2) end_quote)]] ++
        [ Line.other_line $ FormattedString.make_formatted_string
            [([Colors.bold], Text.replicate n_dash "-"), (sgr, Text.replicate n_ch (Text.pack [ch]))]] ++
        zipWith (\ i (ty, msg) ->
            Line.other_line $ FormattedString.make_formatted_string
                [([], Text.replicate (end_col - 1) " "), str_message (i == length msgs - 1) ty msg]
        ) [0..] msgs

show_middle_line :: Maybe (File.File, Int) -> [ANSI.SGR] -> [Line.Line]
show_middle_line (Just (file, nr)) sgr = [Line.numbered_line nr $ FormattedString.make_formatted_string [([], "  "), (sgr, Utils.get_quote file nr)]]
show_middle_line Nothing _ = []
-- tests {{{1
case_underlines :: Assertion
case_underlines =
    let (_, [single_sp, multi_sp]) = make_spans ["abc", "def\nghi\njklm\n"]

        section = underlines
            [ (single_sp, Primary, [(Error, "message 1"), (Hint, "message 2")])
            , (multi_sp, Primary, [(Warning, "message 3")])
            ]
    in Line.compare_many_lines'
        [('f', Colors.file_path), ('e', Colors.error), ('w', Colors.warning), ('h', Colors.hint), ('b', [Colors.bold])]
        [ ("", '>',  "<generated span file>",
                     "f--------------------")

        , ("1", '|', "abc def",
                     "eee    ")
        , ( "", '|', "^^^     ",
                     "eee     ")
        , ( "", '|', "  |-- message 1",
                     " -e------------")
        , ( "", '|', "  `-- message 2",
                     " -h------------")
        , ("2", '|', "ghi",
                     "   ")
        , ("3", '|', "jklm",
                     "    ")

        , ("", '>',  "<generated span file>",
                     "f--------------------")

        , ( "", '|', "      vvv--",
                     " -----w--b-")
        , ("1", '|', "  abc def |",
                     " - ---w-- b")
        , ("2", '|', "  ghi",
                     " -w--")
        , ("3", '|', "| jklm",
                     "b-w---")
        , ( "", '|', "----^^^",
                     "b---w--")
        , ( "", '|', "      `-- message 3",
                     " -----w------------")
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
        , (   "", '|', "~~~~---- ",
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
        , (   "", '|', "~~~~~",
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

        other = Line.numbered_line 2 $ FormattedString.make_formatted_string [([], "abcdefghijklmnop")]

    in [ other
       , Line.numbered_line 1 $ FormattedString.make_formatted_string [([], "t"), ([], "h"), ([], "i"), ([], "n"), ([], "g")]
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
    let (f, [sp1, _, sp2]) = make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"]
        unds = [(sp1, Primary, [(Error, "a")]), (sp2, Primary, [(Error, "b")])]

    in Line.compare_many_lines'
        [('e', Colors.error)]
        [ ("1", '|', "sp1 ABCDEFGHIJKLMNOP sp2",
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
        messages = [(True, Location.before_end sp1, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  `-- message",
                   " -e----------")]

        [show_row [] messages]

case_show_row_message_below :: Assertion
case_show_row_message_below =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]
        messages = [(True, Location.before_end sp2, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  |   `-- message",
                   " -----e----------")]

        [show_row [(True, Location.before_end sp1, Error, "message")] messages]

case_show_row_not_last :: Assertion
case_show_row_not_last =
    let (_, [sp1, _]) = make_spans ["sp1", "sp2"]
        messages = [(False, Location.before_end sp1, Error, "message")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 sp2
        [("", '|', "  |-- message",
                   " -e----------")]

        [show_row [] messages]

case_show_row_multiple :: Assertion
case_show_row_multiple =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "ABCDEFGHIJKLMNOP", "sp2"]
        messages = [(True, Location.before_end sp1, Error, "message1"), (True, Location.before_end sp2, Error, "message2")]
    in Line.compare_many_lines'
        [('e', Colors.error)]
                 -- sp1 ABCDEFGHIJKLMNOP sp2
        [("", '|', "  `-- message1         `-- message2",
                   " -e----------- --------e-----------")]

        [show_row [] messages]

case_assign_message_non_overlapping :: Assertion
case_assign_message_non_overlapping =
    let (_, [sp1, _, sp2]) = make_spans ["sp1", "                ", "sp2"]

        msg1 = (False, Location.before_end sp1, Error, "message 1")
        msg2 = (False, Location.before_end sp2, Error, "message 2")

    in [([], [msg2, msg1])] @=? assign_messages [msg2, msg1]

case_assign_message_overlapping :: Assertion
case_assign_message_overlapping =
    let (_, [sp1, sp2]) = make_spans ["sp1", "sp2"]

        msg1 = (False, Location.before_end sp1, Error, "message 1")
        msg2 = (False, Location.before_end sp2, Error, "message 2")

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

        msg1 = (False, Location.before_end sp1, Error, "a")
        msg2 = (False, Location.before_end sp2, Error, "b")

    in [([msg1], [msg2]), ([], [msg1])] @=? assign_messages [msg2, msg1]

test_overlapping :: [TestTree]
test_overlapping =
    let make_test_case name spacing expected =
            let (_, [sp1, _, sp2]) = make_spans' "f" "" ["sp1", spacing, "sp2"]

                msg2 = (True, Location.before_end sp2, Error, "b")
                msg1 = (True, Location.before_end sp1, Error, "message 1")
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
    in Line.compare_many_lines'
        [ ('e', Colors.error)
        , ('w', Colors.warning)
        , ('b', [Colors.bold])
        , ('f', Colors.file_path)
        ]
        [ ( "", '>', "file",
                     "f---")
        , ( "", '|', "    vvv-------",
                     " ---e--b------")
        , ("2", '|', "  thing      |",
                     " - -e-- -----b")
        , ("3", '|', "  thingthing |",
                     " -e--------- b")
        , ("4", '|', "| thing",
                     "b-e----")
        , ("5", '|', "| abc",
                     "b-e- ")
        , ( "", '|', "-^^^",
                     "be--")
        , ( "", '|', "   |-- message 1",
                     " --e------------")
        , ( "", '|', "   `-- message 2",
                     " --w------------")
        ]
        (show_multiline (sp, Primary, [(Error, "message 1"), (Warning, "message 2")]))

case_multiline_lines_odd :: Assertion
case_multiline_lines_odd =
    let (_, [_, _, sp, _]) = make_spans' "file" "" ["\n", "th", "ing\nthingthing\nzyx\nthing\nab", "c"]
    in Line.compare_many_lines'
        [ ('e', Colors.error)
        , ('w', Colors.warning)
        , ('b', [Colors.bold])
        , ('f', Colors.file_path)
        ]
        [ ( "", '>', "file",
                     "f---")
        , ( "", '|', "    vvv-------",
                     " ---e--b------")
        , ("2", '|', "  thing      |",
                     " - -e-- -----b")
        , ("3", '|', "  thingthing |",
                     " -e--------- b")
        , ("4", '|', "  zyx",
                     " -e--")
        , ("5", '|', "| thing",
                     "b-e----")
        , ("6", '|', "| abc",
                     "b-e- ")
        , ( "", '|', "-^^^",
                     "be--")
        , ( "", '|', "   |-- message 1",
                     " --e------------")
        , ( "", '|', "   `-- message 2",
                     " --w------------")
        ]
        (show_multiline (sp, Primary, [(Error, "message 1"), (Warning, "message 2")]))

tests :: TestTree
tests = $(testGroupGenerator)
