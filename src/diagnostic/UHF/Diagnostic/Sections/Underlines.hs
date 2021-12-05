{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Underlines
    ( UnderlinesSection
    , Underline
    , Importance(..)
    , Type(..)
    , underlines
    ) where

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
