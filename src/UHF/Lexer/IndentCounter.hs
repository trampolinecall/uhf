{-# LANGUAGE TupleSections #-}

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

import qualified UHF.Token as Token

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Void as Void
import qualified Data.Maybe as Maybe

import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Control.Monad.Trans.Class

data IndentationFrame = IndentationSensitive Int | IndentationInsensitive

count_indents :: ([Token.LUnprocessedToken], Token.LNormalToken) -> [Token.LTokenWithIndentation]
count_indents = insert_indentation_tokens . count_indent_numbers . join_logical_lines . split_lines

split_lines :: ([Token.LUnprocessedToken], Token.LNormalToken) -> [([Token.LUnprocessedToken], Location.Span)]
split_lines (toks, eof) = (one_line, next_nl_span) : split_lines (drop 1 more, eof) -- drop the newline, but if there is no newline then it will just be an empty list
    where
        nl_ind = List.findIndex (\ (Location.Located _ t) -> t == Token.Newline Token.NLPhysical) toks
        next_nl_span = Maybe.fromMaybe (Location.just_span eof) (Location.just_span <$> (toks !!) <$> nl_ind)
        (one_line, more) = maybe (,[]) List.splitAt nl_ind toks

join_logical_lines :: [([Token.LUnprocessedToken], Location.Span)] -> [([Token.LUnprocessedToken], Location.Span)]
join_logical_lines lines = -- TODO: refactor this
    let (until_end, last_line) = (init lines, last lines)
        (has_backslash, without_backslash) = span ((==Token.Backslash ()) . Location.unlocate . last . fst) until_end
        one_without_backslash:more = without_backslash ++ [last_line]

        joining = has_backslash ++ [one_without_backslash] -- TODO: remove backslash from this line
    in (concat $ map fst joining, snd $ last joining) : join_logical_lines more

count_indent_numbers :: [([Token.LUnprocessedToken], Location.Span)] -> [(Int, [Token.LUnprocessedToken], Location.Span)]
count_indent_numbers = Maybe.mapMaybe count_indent
    where
        -- TODO: count tab characters properly
        count_indent ([], _) = Nothing
        count_indent (toks@((Location.Located sp _):_), nl) = Just (Text.length $ Text.takeWhileEnd (/='\n') $ Text.take (Location.ind sp_start) (File.contents $ Location.file sp_start), toks, nl)
            where
                sp_start = Location.start sp

insert_indentation_tokens :: [(Int, [Token.LUnprocessedToken], Location.Span)] -> [Token.LTokenWithIndentation]
insert_indentation_tokens lns = Writer.execWriter $ State.execStateT (mapM do_line lns >> put_final_dedents) [IndentationSensitive 0]
    where
        do_line (indent_amt, toks, nl) =
            do_indentation indent_amt (Location.start $ Location.just_span $ head toks) >>
            go_through_tokens toks >>
            put_newline nl

        do_indentation cur_indent start_loc =
            let indent_token_sp = Location.new_span start_loc 0 1
            in head <$> State.get >>= \case
                IndentationSensitive last_indent
                    | cur_indent > last_indent ->
                        lift (Writer.tell [Location.Located indent_token_sp (Token.Indent ())]) >>
                        State.modify (IndentationSensitive cur_indent:)

                    | cur_indent < last_indent ->
                        let pop_if_needed =
                                head <$> State.get >>= \case
                                    IndentationSensitive il
                                        | cur_indent < il ->
                                            lift (Writer.tell [Location.Located indent_token_sp (Token.Dedent ())]) >>
                                            State.modify tail >>
                                            pop_if_needed

                                    _ -> return ()

                        in pop_if_needed >>

                        head <$> State.get >>= \case
                            IndentationSensitive il
                                | il < cur_indent -> error "invalid dedent" -- TODO: make an actual error for this

                            _ -> return ()

                _ -> return ()

        put_newline :: Location.Span -> State.StateT [IndentationFrame] (Writer.Writer [Token.LTokenWithIndentation]) ()
        put_newline nl_sp =
            head <$> State.get >>= \case
                IndentationSensitive _ -> lift $ Writer.tell [Location.Located nl_sp (Token.Newline Token.NLLogical)]
                _ -> return ()

        go_through_tokens :: [Token.LUnprocessedToken] -> State.StateT [IndentationFrame] (Writer.Writer [Token.LTokenWithIndentation]) ()
        go_through_tokens = mapM_ wtok -- TODO: braces
            where
                wtok :: Token.LUnprocessedToken -> State.StateT [IndentationFrame] (Writer.Writer [Token.LTokenWithIndentation]) ()
                wtok (Location.Located sp t) = lift $ Writer.tell [Location.Located sp (do_tok t)]

                do_tok :: Token.UnprocessedToken -> Token.TokenWithIndentation
                do_tok (Token.SingleTypeToken t) = Token.SingleTypeToken t

                do_tok (Token.DoubleColon dc) = Token.DoubleColon dc

                do_tok (Token.SymbolIdentifier i) = Token.SymbolIdentifier i
                do_tok (Token.AlphaIdentifier i) = Token.AlphaIdentifier i

                do_tok (Token.OBrace) = Token.OBrace
                do_tok (Token.CBrace) = Token.CBrace
                do_tok (Token.Semicolon) = Token.Semicolon
                do_tok (Token.Backslash _) = error "unreachable"
                do_tok (Token.Indent i) = Void.absurd i
                do_tok (Token.Dedent i) = Void.absurd i
                do_tok (Token.Newline _) = error "unreachable"
                do_tok (Token.EOF e) = Void.absurd e

        put_final_dedents =
            init <$> State.get >>= \ indentation_frames ->
            lift (Writer.tell $
                concatMap
                    (\case
                        IndentationSensitive _ -> [Location.Located undefined (Token.Dedent ())]
                        IndentationInsensitive -> [])
                    indentation_frames)

-- tests {{{1
-- TODO: tests

-- case_split_lines :: Assertion
-- case_split_lines = "abcde\nfghij"
-- case_split_lines_trailing :: Assertion
-- case_split_lines_trailing = "abcde\nfghij"

-- case_join_logical_lines :: Assertion
-- case_join_logical_lines = "line1\\\nline2\nline3"
-- case_join_logical_lines_none :: Assertion
-- case_join_logical_lines_none = "line1\nline2\nline3"
-- case_join_logical_lines_multiple :: Assertion
-- case_join_logical_lines_multiple = "line1\\\nline2\\\nline3"
-- case_join_logical_lines_backslash_last = "line1\\\n"

-- case_count_indent_numbers :: Assertion
-- case_count_indent_numbers = "line1\n    line2\n        line3\n  line4\n\tline5\n  \tline6"

case_insert_indentation_tokens_indented_block :: Assertion
case_insert_indentation_tokens_indented_block = undefined

{-
line1
    line2
line3
-}

case_insert_indentation_tokens_ending_dedents = undefined

{-
line1
    line2
-}

case_insert_indentation_tokens_braced_block :: Assertion
case_insert_indentation_tokens_braced_block = undefined
{-
line1 {
    line2
}
-}

case_insert_indentation_tokens_nested_indented_blocks :: Assertion
case_insert_indentation_tokens_nested_indented_blocks = undefined
{-

line1
    line2
        line3
            line4
        line5
line6
    line7
        line8
-}

case_insert_indentation_tokens_nl_after_semi :: Assertion
case_insert_indentation_tokens_nl_after_semi = undefined

{-
"line1;\n"
-}

case_insert_indentation_tokens_semi_before_dedent :: Assertion
case_insert_indentation_tokens_semi_before_dedent = undefined

{-
line1
    line2;
line3
-}

tests :: TestTree
tests = $(testGroupGenerator)
