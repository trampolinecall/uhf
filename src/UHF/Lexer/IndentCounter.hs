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
import qualified Data.List.Split as Split
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
split_lines ([], _) = []
split_lines (toks, eof) = (one_line, next_nl_span) : split_lines (drop 1 more, eof) -- drop the newline, but if there is no newline then it will just be an empty list
    where
        nl_ind = List.findIndex (\ (Location.Located _ t) -> t == Token.Newline Token.NLPhysical) toks
        next_nl_span = Maybe.fromMaybe (Location.just_span eof) (Location.just_span <$> (toks !!) <$> nl_ind)
        (one_line, more) = maybe (,[]) List.splitAt nl_ind toks

join_logical_lines :: [([Token.LUnprocessedToken], Location.Span)] -> [([Token.LUnprocessedToken], Location.Span)]
join_logical_lines ((cur_line_toks, _) : (next_line_toks, next_line_nl) : more)
    | Location.unlocate (last cur_line_toks) == Token.Backslash () = join_logical_lines $ (init cur_line_toks ++ next_line_toks, next_line_nl) : more
join_logical_lines (cur_line : more) = cur_line : join_logical_lines more
join_logical_lines [] = []

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
generate_lines :: [Int] -> (File.File, [(Int, Location.Located (Token.BaseToken dc String eof ind nl bs), Location.Span)])
generate_lines indents =
    let (f, sps) = SpanHelper.make_spans' "test" "" (concatMap (\ (ln, i) -> [replicate i ' ', "line" ++ show ln, "\n"]) $ zip ([1..] :: [Int]) indents)
    in (f, map
        (\ (ind, ln, [_, line_sp, nl_sp]) -> (ind, Location.Located line_sp $ Token.AlphaIdentifier $ "line" ++ show ln, nl_sp))
            (zip3
                indents
                ([1..] :: [Int])
                (Split.chunksOf 3 sps)))

case_split_lines :: Assertion
case_split_lines =
    let (f, toks@[line1, nl, line2]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2"]
        eof_sp = Location.eof_span f
        eof = Location.Located eof_sp (Token.EOF ())

    in [([line1], Location.just_span nl), ([line2], eof_sp)] @=? split_lines (toks, eof)

case_split_lines_trailing :: Assertion
case_split_lines_trailing =
    let (f, toks@[line1, nl1, line2, nl2]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2", Token.Newline Token.NLPhysical]
        eof_sp = Location.eof_span f
        eof = Location.Located eof_sp (Token.EOF ())

    in [([line1], Location.just_span nl1), ([line2], Location.just_span nl2)] @=? split_lines (toks, eof)

case_join_logical_lines :: Assertion
case_join_logical_lines =
    let (f, [line1, bs, nl1, line2, nl2, line3]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Backslash (), Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line3"]
        eof_sp = Location.eof_span f
    in [([line1, line2], Location.just_span nl2), ([line3], eof_sp)] @=? join_logical_lines [([line1, bs], Location.just_span nl1), ([line2], Location.just_span nl2), ([line3], eof_sp)]

case_join_logical_lines_none :: Assertion
case_join_logical_lines_none =
    let (f, [line1, nl1, line2, nl2, line3]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line3"]
        eof_sp = Location.eof_span f
        a = [([line1], Location.just_span nl1), ([line2], Location.just_span nl2), ([line3], eof_sp)]
    in a @=? join_logical_lines a

case_join_logical_lines_multiple :: Assertion
case_join_logical_lines_multiple =
    let (f, [line1, bs1, nl1, line2, bs2, nl2, line3]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Backslash (), Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2", Token.Backslash (), Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line3"]
        eof_sp = Location.eof_span f
    in [([line1, line2, line3], eof_sp)] @=? join_logical_lines [([line1, bs1], Location.just_span nl1), ([line2, bs2], Location.just_span nl2), ([line3], eof_sp)]

case_join_logical_lines_backslash_last :: Assertion
case_join_logical_lines_backslash_last =
    let (_, [line1, bs1, nl1]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Backslash (), Token.Newline Token.NLPhysical]
    in [([line1, bs1], Location.just_span nl1)] @=? join_logical_lines [([line1, bs1], Location.just_span nl1)]

case_count_indent_numbers :: Assertion
case_count_indent_numbers =
    let (_, res) = generate_lines [0, 4, 8, 2]
    in map (\ (i, t, nl) -> (i, [t], nl)) res @=? count_indent_numbers (map (\ (_, t, nl) -> ([t], nl)) res)

case_count_indent_numbers_tabs :: Assertion
case_count_indent_numbers_tabs =
    let (_, [_, ln1, nl1, _, ln2, nl2]) =
            SpanHelper.make_spans_with_items' "test" ""
                [ ("\t", undefined), ("line1", Token.AlphaIdentifier "line1"), ("\n", Token.Newline Token.NLPhysical)
                , ("  \t", undefined), ("line2", Token.AlphaIdentifier "line2"), ("\n", Token.Newline Token.NLPhysical)
                ]
    in
        [ (8, [ln1], Location.just_span nl1)
        , (8, [ln2], Location.just_span nl2)
        ] @=?
        (count_indent_numbers
            [ ([ln1], Location.just_span nl1)
            , ([ln2], Location.just_span nl2)
            ])

case_insert_indentation_tokens_indented_block :: Assertion
case_insert_indentation_tokens_indented_block =
    let (_, res@[(_, ln1, _), (_, ln2, nl2), (_, ln3, nl3)]) = generate_lines [0, 4, 0]
        indent = Location.Located (Location.new_span (Location.start $ Location.just_span ln2) 0 1) (Token.Indent ())
        dedent = Location.Located (Location.new_span (Location.start $ Location.just_span ln3) 0 1) (Token.Dedent ())
    in [ln1, indent, ln2, Location.Located nl2 $ Token.Newline Token.NLLogical, dedent, ln3, Location.Located nl3 $ Token.Newline Token.NLLogical] @=? insert_indentation_tokens (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_ending_dedents =
    let (_, res@[(_, ln1, _), (_, ln2, nl2)]) = generate_lines [0, 4]
        indent = Location.Located (Location.new_span (Location.start $ Location.just_span ln2) 0 1) (Token.Indent ())
        dedent = Location.Located (Location.new_span (Location.start nl2) 0 1) (Token.Dedent ())
    in [ln1, indent, ln2, Location.Located nl2 $ Token.Newline Token.NLLogical, dedent] @=? insert_indentation_tokens (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_braced_block :: Assertion
case_insert_indentation_tokens_braced_block =
    let (_, [ln1, obrace, nl1, ln2, nl2, cbrace, nl3]) =
            SpanHelper.make_spans_with_show_items' "test" ""
                [ (Token.AlphaIdentifier "line1", Token.AlphaIdentifier "line1"), (Token.OBrace, Token.OBrace), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.AlphaIdentifier "line2", Token.AlphaIdentifier "line2"), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.CBrace, Token.CBrace), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                ]
    in
        insert_indentation_tokens
            [(0, [fst <$> ln1, fst <$> obrace], Location.just_span nl1), (4, [fst <$> ln2], Location.just_span nl2), (3, [fst <$> cbrace], Location.just_span nl3)] @?=
        map (snd <$>) [ln1, obrace, ln2, nl2, cbrace, nl3]

-- TODO: these tests

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
