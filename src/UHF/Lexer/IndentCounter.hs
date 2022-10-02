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

import qualified UHF.Lexer.LexError as LexError

import qualified UHF.Token as Token

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Text as Text
import qualified Data.Void as Void
import qualified Data.Maybe as Maybe

import qualified Control.Arrow as Arrow
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Writer as Writer
import Control.Monad.Trans.Class

data IFrame = ISensitive Int | IInsensitive

count_indents :: [Token.LUnprocessedToken] -> Location.Span -> ([LexError.LexError], [Token.LTokenWithIndentation])
count_indents toks eof_span =
    insert_indentation_tokens eof_span $ count_indent_numbers $ join_logical_lines $ split_lines toks eof_span

split_lines :: [Token.LUnprocessedToken] -> Location.Span -> [([Token.LUnprocessedToken], Location.Span)]
split_lines [] _ = []
split_lines toks eof_span = (one_line, next_nl_span) : split_lines (drop 1 more) eof_span -- drop the newline, but if there is no newline then it will just be an empty list
    where
        nl_ind = List.findIndex (\ (Location.Located _ t) -> t == Token.Newline Token.NLPhysical) toks
        next_nl_span = Maybe.fromMaybe eof_span (Location.just_span <$> (toks !!) <$> nl_ind)
        (one_line, more) = maybe (,[]) List.splitAt nl_ind toks

join_logical_lines :: [([Token.LUnprocessedToken], Location.Span)] -> [([Token.LUnprocessedToken], Location.Span)]
join_logical_lines (([], _) : more) = join_logical_lines more
join_logical_lines ((cur_line_toks, _) : (next_line_toks, next_line_nl) : more)
    | Location.unlocate (last cur_line_toks) == Token.Backslash () = join_logical_lines $ (init cur_line_toks ++ next_line_toks, next_line_nl) : more
join_logical_lines (cur_line : more) = cur_line : join_logical_lines more
join_logical_lines [] = []

count_indent_numbers :: [([Token.LUnprocessedToken], Location.Span)] -> [(Int, [Token.LUnprocessedToken], Location.Span)]
count_indent_numbers = Maybe.mapMaybe count_indent
    where
        count_indent ([], _) = error "unreachable"
        count_indent (toks@((Location.Located sp _):_), nl) = Just (count_spaces $ Text.takeWhileEnd (/='\n') $ Text.take (Location.ind sp_start) (File.contents $ Location.file sp_start), toks, nl)
            where
                sp_start = Location.start sp
                count_spaces = Text.foldl'
                    (\ i -> \case
                        '\t' -> (i `div` 8 + 1) * 8
                        _ -> i + 1)
                    0

insert_indentation_tokens :: Location.Span -> [(Int, [Token.LUnprocessedToken], Location.Span)] -> ([LexError.LexError], [Token.LTokenWithIndentation])
insert_indentation_tokens eof_sp lns =
    let remove_nls ((Location.Located _ (Token.Newline Token.NLLogical)) : indent@(Location.Located _ (Token.Indent ())) : more) = indent : remove_nls more
        remove_nls (semi@(Location.Located _ Token.Semicolon) : (Location.Located _ (Token.Newline Token.NLLogical)) : more) = semi : remove_nls more
        remove_nls (x:more) = x : remove_nls more
        remove_nls [] = []
    in Arrow.second remove_nls $ Writer.runWriter $ Writer.execWriterT $ State.execStateT
        (mapM do_line lns >> put_final_dedents)
        [ISensitive 0]
    where
        do_line (indent_amt, toks, nl) =
            do_indentation indent_amt (Location.start $ Location.just_span $ head toks) >>
            go_through_tokens toks >>
            put_newline nl

        do_indentation cur_indent start_loc =
            let indent_token_sp = Location.new_span start_loc 0 1
            in head <$> State.get >>= \case
                ISensitive last_indent
                    | cur_indent > last_indent ->
                        lift (lift (Writer.tell [Location.Located indent_token_sp (Token.Indent ())])) >>
                        State.modify (ISensitive cur_indent:)

                    | cur_indent < last_indent ->
                        let pop_if_needed =
                                head <$> State.get >>= \case
                                    ISensitive il
                                        | cur_indent < il ->
                                            lift (lift (Writer.tell [Location.Located indent_token_sp (Token.Dedent ())])) >>
                                            State.modify tail >>
                                            pop_if_needed

                                        | il < cur_indent -> lift (Writer.tell [LexError.BadDedent indent_token_sp])

                                        | il == cur_indent -> return ()

                                    _ -> error "unreachable: indentation block inside braced block"

                        in pop_if_needed

                _ -> return ()

        put_newline nl_sp =
            head <$> State.get >>= \case
                ISensitive _ -> lift $ lift $ Writer.tell [Location.Located nl_sp (Token.Newline Token.NLLogical)]
                _ -> return ()

        go_through_tokens = mapM_ $ \ (Location.Located sp t) ->
            case t of
                Token.SingleTypeToken stt -> lift $ lift $ Writer.tell [Location.Located sp (Token.SingleTypeToken stt)]

                Token.DoubleColon dc -> lift $ lift $ Writer.tell [Location.Located sp (Token.DoubleColon dc)]

                Token.SymbolIdentifier i -> lift $ lift $ Writer.tell [Location.Located sp (Token.SymbolIdentifier i)]
                Token.AlphaIdentifier i -> lift $ lift $ Writer.tell [Location.Located sp (Token.AlphaIdentifier i)]

                Token.OBrace -> lift (lift $ Writer.tell [Location.Located sp (Token.OBrace)]) >> State.modify (IInsensitive:)
                Token.CBrace ->
                    lift (lift $ Writer.tell [Location.Located sp (Token.CBrace)]) >>
                    State.modify (\case
                        IInsensitive : more -> more
                        x -> x) -- the parser can handle the stray '}'
                Token.Semicolon -> lift $ lift $ Writer.tell [Location.Located sp (Token.Semicolon)]
                Token.Backslash _ -> error "unreachable"
                Token.Indent i -> Void.absurd i
                Token.Dedent i -> Void.absurd i
                Token.Newline _ -> error "unreachable"
                Token.EOF e -> Void.absurd e

        put_final_dedents =
            init <$> State.get >>= \ indentation_frames ->
            lift (lift $ Writer.tell $
                concatMap
                    (\case
                        ISensitive _ -> [Location.Located eof_sp (Token.Dedent ())]
                        IInsensitive -> [])
                    indentation_frames)

-- tests {{{1
generate_lines :: [Int] -> (File.File, [(Int, Location.Located (Token.BaseToken dc String eof ind nl bs), Location.Span)])
generate_lines indents =
    let (f, sps) = SpanHelper.make_spans' "test" "" (concatMap (\ (ln, i) -> [replicate i ' ', "line" ++ show ln, "\n"]) $ zip ([1..] :: [Int]) indents)
    in (f, map
        (\ (ind, ln, [_, line_sp, nl_sp]) -> (ind, Location.Located line_sp $ Token.AlphaIdentifier $ "line" ++ show ln, nl_sp))
            (zip3 indents ([1..] :: [Int]) (Split.chunksOf 3 sps)))

nl_at :: Location.Span -> Location.Located (Token.BaseToken dc iden eof ind Token.NLLogical bs)
nl_at sp = Location.Located sp (Token.Newline Token.NLLogical)

indent_at :: Location.Located (Token.BaseToken dc iden eof ind nl bs) -> Location.Located (Token.BaseToken dc' iden' eof' () nl' bs')
indent_at (Location.Located sp _) = Location.Located (Location.new_span (Location.start sp) 0 1) (Token.Indent ())
dedent_at :: Location.Located (Token.BaseToken dc iden eof ind nl bs) -> Location.Located (Token.BaseToken dc' iden' eof' () nl' bs')
dedent_at (Location.Located sp _) = Location.Located (Location.new_span (Location.start sp) 0 1) (Token.Dedent ())
dedent_e :: Location.Span -> Location.Located (Token.BaseToken dc iden eof () nl bs)
dedent_e sp = Location.Located sp (Token.Dedent ())

case_split_lines :: Assertion
case_split_lines =
    let (f, toks@[line1, nl, line2]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2"]
        eof_sp = Location.eof_span f
    in [([line1], Location.just_span nl), ([line2], eof_sp)] @=? split_lines toks eof_sp

case_split_lines_trailing :: Assertion
case_split_lines_trailing =
    let (f, toks@[line1, nl1, line2, nl2]) = SpanHelper.make_spans_with_show_items [Token.AlphaIdentifier "line1", Token.Newline Token.NLPhysical, Token.AlphaIdentifier "line2", Token.Newline Token.NLPhysical]
        eof_sp = Location.eof_span f
    in [([line1], Location.just_span nl1), ([line2], Location.just_span nl2)] @=? split_lines toks eof_sp

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
    let (f, res@[(_, ln1, _), (_, ln2, nl2), (_, ln3, nl3)]) = generate_lines [0, 4, 0]
        eof_sp = Location.eof_span f
    in ([], [ln1, indent_at ln2, ln2, nl_at nl2, dedent_at ln3, ln3, nl_at nl3]) @=? insert_indentation_tokens eof_sp (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_ending_dedents :: Assertion
case_insert_indentation_tokens_ending_dedents =
    let (f, res@[(_, ln1, _), (_, ln2, nl2)]) = generate_lines [0, 4]
        eof_sp = Location.eof_span f
    in ([], [ln1, indent_at ln2, ln2, nl_at nl2, dedent_e eof_sp]) @=? insert_indentation_tokens eof_sp (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_braced_block :: Assertion
case_insert_indentation_tokens_braced_block =
    let (f, [ln1, obrace, nl1, ln2, nl2, cbrace, nl3]) =
            SpanHelper.make_spans_with_show_items' "test" ""
                [ (Token.AlphaIdentifier "line1", Token.AlphaIdentifier "line1"), (Token.OBrace, Token.OBrace), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.AlphaIdentifier "line2", Token.AlphaIdentifier "line2"), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.CBrace, Token.CBrace), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                ]
        eof_sp = Location.eof_span f
    in
        insert_indentation_tokens eof_sp
            [(0, [fst <$> ln1, fst <$> obrace], Location.just_span nl1), (4, [fst <$> ln2], Location.just_span nl2), (3, [fst <$> cbrace], Location.just_span nl3)] @?=
        ([], map (snd <$>) [ln1, obrace, ln2, cbrace, nl3])

case_insert_indentation_tokens_indented_begin :: Assertion
case_insert_indentation_tokens_indented_begin =
    let (f, res@[(_, ln1, nl1), (_, ln2, nl2)]) = generate_lines [4, 0]
        eof_sp = Location.eof_span f
    in ([], [indent_at ln1, ln1, nl_at nl1, dedent_at ln2, ln2, nl_at nl2]) @=? insert_indentation_tokens eof_sp (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_nested_indented_blocks :: Assertion
case_insert_indentation_tokens_nested_indented_blocks =
    let (f, res@[(_, ln1, _), (_, ln2, _), (_, ln3, _), (_, ln4, nl4), (_, ln5, nl5), (_, ln6, _), (_, ln7, _), (_, ln8, nl8)]) = generate_lines [0, 4, 8, 12, 8, 0, 4, 8]
        eof_sp = Location.eof_span f
    in
        ( [],
          [ ln1
          , indent_at ln2, ln2
          , indent_at ln3, ln3
          , indent_at ln4, ln4, nl_at nl4
          , dedent_at ln5, ln5, nl_at nl5
          , dedent_at ln6, dedent_at ln6, ln6
          , indent_at ln7, ln7
          , indent_at ln8, ln8, nl_at nl8
          , dedent_e eof_sp, dedent_e eof_sp
          ]
        ) @=?
        insert_indentation_tokens eof_sp (map (\ (i, t, nl) -> (i, [t], nl)) res)

case_insert_indentation_tokens_nl_after_semi :: Assertion
case_insert_indentation_tokens_nl_after_semi =
    let (f, [ln, semi, nl]) = SpanHelper.make_spans_with_show_items' "test" ""
                [(Token.AlphaIdentifier "line1", Token.AlphaIdentifier "line1"), (Token.Semicolon, Token.Semicolon), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)]
        eof_sp = Location.eof_span f
    in ([], [snd <$> ln, snd <$> semi]) @=? insert_indentation_tokens eof_sp [(0, [fst <$> ln, fst <$> semi], Location.just_span nl)]

case_insert_indentation_tokens_semi_before_dedent :: Assertion
case_insert_indentation_tokens_semi_before_dedent =
    let (f, [ln1, nl1, ln2, semi2, nl2, ln3, nl3]) = SpanHelper.make_spans_with_show_items' "test" ""
                [ (Token.AlphaIdentifier "line1", Token.AlphaIdentifier "line1"), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.AlphaIdentifier "line2", Token.AlphaIdentifier "line2"), (Token.Semicolon, Token.Semicolon), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)
                , (Token.AlphaIdentifier "line3", Token.AlphaIdentifier "line3"), (Token.Newline Token.NLPhysical, Token.Newline Token.NLLogical)]
        eof_sp = Location.eof_span f
    in ([], [snd <$> ln1, indent_at $ snd <$> ln2, snd <$> ln2, snd <$> semi2, dedent_at $ snd <$> ln3, snd <$> ln3, snd <$> nl3]) @=?
        insert_indentation_tokens eof_sp
            [ (0, [fst <$> ln1], Location.just_span nl1)
            , (4, [fst <$> ln2, fst <$> semi2], Location.just_span nl2)
            , (0, [fst <$> ln3], Location.just_span nl3)
            ]

tests :: TestTree
tests = $(testGroupGenerator)
