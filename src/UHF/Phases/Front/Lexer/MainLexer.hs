module UHF.Phases.Front.Lexer.MainLexer
    ( lex

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.Phases.Front.Lexer.LexError as LexError
import qualified UHF.IO.File as File
import UHF.IO.File (File)
import qualified UHF.IO.Location as Location
import UHF.IO.Location (Location)
import qualified UHF.IO.Span as Span
import UHF.IO.Span (Span)
import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located (Located))
import qualified UHF.Data.Token as Token

import qualified Data.Text as Text
import qualified Data.Sequence as Sequence

import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, digitToInt)

-- lexing {{{1
lex :: File -> Writer [LexError.Error] (Sequence.Seq Token.LInternalToken, Token.LToken)
lex f =
    let eof = Located (Span.end_of_file f) (Token.EOF ())
    in evalStateT (run Sequence.Empty) (LexLocation (Location.new f) (File.contents f)) >>= \ toks -> pure (toks, eof)
    where
        run toks =
            get >>= \ l ->
            if at_end l
                then pure toks
                else lex_one_token >>= \ more -> run (toks <> more)
-- lex_one_token {{{2
data LexLocation = LexLocation Location Text deriving Show
lex_one_token :: StateT LexLocation (Writer [LexError.Error]) (Sequence.Seq Token.LInternalToken)
lex_one_token =
    StateT $ \ loc ->
        writer $
            head $ mapMaybe (runWriterT . ($ loc) . runStateT)
                [ lex_comment

                , lex_alpha_identifier
                , lex_symbol_identifier

                , lex_str_or_char_lit
                , lex_number

                , lex_space
                , make_bad_char
                ]
-- Lexer {{{2
type Lexer = StateT LexLocation (WriterT [LexError.Error] Maybe)
-- lexing functions {{{2
lex_comment :: Lexer (Sequence.Seq Token.LInternalToken)
lex_comment =
    get_loc >>= \ start_loc ->
    let lex_singleline = consume (=='/') >> consume (=='/') >> lex_singleline_body
        lex_singleline_body =
            choice
                [ consume (/='\n') >> lex_singleline_body
                , consume (=='\n') >> pure Sequence.Empty
                , pure Sequence.Empty]

        lex_multiline = consume (=='/') >> consume (=='*') >> lex_multiline_body
        lex_multiline_body =
            choice
                [ consume (=='*') >> consume (=='/') >> pure Sequence.Empty
                , lex_multiline >> lex_multiline_body
                , consume (const True) >> lex_multiline_body
                , get_loc >>= \ loc -> put_error (LexError.UnclosedMComment $ new_span_start_and_end start_loc loc) >> pure Sequence.Empty
                ]
    in choice [lex_singleline, lex_multiline]

lex_id_or_kw :: (Char -> Bool) -> (Char -> Bool) -> [(Text, Token.InternalToken)] -> (Located Text -> Token.InternalToken) -> Lexer (Sequence.Seq Token.LInternalToken)
lex_id_or_kw is_valid_start is_valid_char kws def =
    get_loc >>= \ start_loc ->
    consume is_valid_start >>= \ first_char ->
    choice [one_or_more $ consume is_valid_char, pure []] >>= \ more ->
    let full =
            let chars = first_char : more
                sp = Located.just_span (head chars) <> Located.just_span (last chars)
            in Located sp (Text.pack $ Located.unlocate <$> chars)
        tok = fromMaybe (def full) (lookup (Located.unlocate full) kws)
    in get_loc >>= \ end_loc ->
    pure (Sequence.singleton $ Located (new_span_start_and_end start_loc end_loc) tok)

lex_alpha_identifier :: Lexer (Sequence.Seq Token.LInternalToken)
lex_alpha_identifier =
    lex_id_or_kw
        (\ ch -> isAlpha ch || ch == '_')
        (\ ch -> isAlpha ch || isDigit ch || ch == '_' || ch == '\'')
        [ ("_", Token.SingleTypeToken Token.Underscore)
        , ("root", Token.SingleTypeToken Token.Root)
        , ("let", Token.SingleTypeToken Token.Let)
        , ("letrec", Token.SingleTypeToken Token.LetRec)
        , ("data", Token.SingleTypeToken Token.Data)
        , ("impl", Token.SingleTypeToken Token.Impl)
        , ("if", Token.SingleTypeToken Token.If)
        , ("then", Token.SingleTypeToken Token.Then)
        , ("else", Token.SingleTypeToken Token.Else)
        , ("case", Token.SingleTypeToken Token.Case)
        , ("typesyn", Token.SingleTypeToken Token.TypeSyn)
        , ("true", Token.Bool True)
        , ("false", Token.Bool False)
        ]
        Token.AlphaIdentifier

lex_symbol_identifier :: Lexer (Sequence.Seq Token.LInternalToken)
lex_symbol_identifier =
    lex_id_or_kw
        -- TODO: fix bug: '(:' is not a symbol identifier but the continuation characters matches the ':' after '('
        (`elem` ("~!@#$%^&*+`-=|:./<>?()[]\\{};," :: [Char]))
        (`elem` ("~!@#$%^&*+`-=|:./<>?" :: [Char]))
        [ ("->", Token.SingleTypeToken Token.Arrow)
        , ("::", Token.SingleTypeToken Token.DoubleColon)
        , ("(", Token.SingleTypeToken Token.OParen)
        , (")", Token.SingleTypeToken Token.CParen)
        , ("[", Token.SingleTypeToken Token.OBrack)
        , ("]", Token.SingleTypeToken Token.CBrack)
        , ("{", Token.SingleTypeToken Token.OBrace)
        , ("}", Token.SingleTypeToken Token.CBrace)
        , (";", Token.SingleTypeToken Token.Semicolon)
        , (",", Token.SingleTypeToken Token.Comma)
        , ("=", Token.SingleTypeToken Token.Equal)
        , (":", Token.SingleTypeToken Token.Colon)
        , ("@", Token.SingleTypeToken Token.At)
        , ("\\", Token.SingleTypeToken Token.Backslash)
        ]
        Token.SymbolIdentifier

lex_str_or_char_lit :: Lexer (Sequence.Seq Token.LInternalToken)
lex_str_or_char_lit =
    get_loc >>= \ start_loc ->
    consume (\ c -> c == '\'' || c == '"') >>= \ (Located _ open) ->
    one_or_more (Located.unlocate <$> consume (\ ch -> ch /= open && ch /= '\n')) >>= \ contents ->
    choice
        [ consume (==open) >>
          get_loc >>= \ end_loc ->
          let sp = new_span_start_and_end start_loc end_loc
          in if open == '\''
              then case contents of
                    [c] -> pure $ Sequence.singleton $ Located sp $ Token.Char c
                    _ -> put_error (LexError.MulticharCharLit sp) >> pure Sequence.Empty
              else pure $ Sequence.singleton $ Located sp $ Token.String $ Text.pack contents

        , get_loc >>= \ end_loc ->
          let sp = new_span_start_and_end start_loc end_loc
          in if open == '\''
              then put_error (LexError.UnclosedCharLit sp) >> pure Sequence.Empty
              else put_error (LexError.UnclosedStrLit sp) >> pure Sequence.Empty
        ]

lex_number :: Lexer (Sequence.Seq Token.LInternalToken)
lex_number =
    get_loc >>= \ start_loc ->

    choice [Just <$> lex_base, pure Nothing] >>= \ m_base ->
    lex_digits >>= \ digits ->
    choice [lex_float, pure []] >>= \ floats ->

    get_loc >>= \ end_loc ->

    let num_span = new_span_start_and_end start_loc end_loc
        ei_tok_base = case m_base of
            Just (Located _ 'o') -> Right (Token.Oct, 8)
            Just (Located _ 'x') -> Right (Token.Hex, 16)
            Just (Located _ 'b') -> Right (Token.Bin, 2)
            Nothing -> Right (Token.Dec, 10)

            Just (Located base_sp c) -> Left $ LexError.InvalidIntBase c base_sp

        read_digits power num_base = sum . map (\ (place, value) -> num_base `power` place * fromIntegral (digitToInt value))

        check_digits verify = mapMaybe (\ (Located sp c) -> if verify c then Nothing else Just $ LexError.InvalidIntDigit c sp)

    in case (ei_tok_base, floats) of
        (Right (tok_base, base_num), []) ->
            let digit_legal = case tok_base of
                    Token.Oct -> isOctDigit
                    Token.Hex -> isHexDigit
                    Token.Bin -> \ c -> c == '0' || c == '1'
                    Token.Dec -> isDigit

                illegal_digits = check_digits digit_legal digits

            in if null illegal_digits
                then pure $ Sequence.singleton $ Located num_span (Token.Int tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (map Located.unlocate (reverse digits)))))
                else mapM_ put_error illegal_digits >> pure Sequence.Empty

        (Right (tok_base, _), _) ->
            let illegal_digits = check_digits isDigit (digits ++ floats)
                base_is_dec = if tok_base == Token.Dec then [] else [LexError.NonDecimalFloat num_span]

            in if null illegal_digits && null base_is_dec
                then pure $ Sequence.singleton $ Located num_span (Token.Float $ read_digits ((^^) :: Rational -> Int -> Rational) 10 (zip [0..] (map Located.unlocate $ reverse digits) ++ zip [-1, -2..] (map Located.unlocate floats)))
                else mapM_ put_error (illegal_digits ++ base_is_dec) >> pure Sequence.Empty

        (Left err, _) -> put_error err >> pure Sequence.Empty
    where
        lex_base = consume (=='0') >> consume isAlpha
        lex_digits = one_or_more (consume isHexDigit)
        lex_float = consume (=='.') >> lex_digits

lex_space :: Lexer (Sequence.Seq Token.LInternalToken)
lex_space = consume isSpace >> pure Sequence.Empty

make_bad_char :: Lexer (Sequence.Seq Token.LInternalToken)
make_bad_char = consume (const True) >>= \ (Located sp c) -> put_error (LexError.BadChar c sp) >> pure Sequence.Empty
-- helper functions {{{1
remaining :: LexLocation -> Text
remaining (LexLocation _ r) = r

at_end :: LexLocation -> Bool
at_end (LexLocation _ r) = Text.null r

new_span_start_and_end :: LexLocation -> LexLocation -> Span
-- start and end should be in the same file because the lex function never processes more than one file at a time
new_span_start_and_end (LexLocation start _) (LexLocation end _) = Span.new start 0 (Location.loc_ind end - Location.loc_ind start)

choice :: [Lexer a] -> Lexer a
choice [] = StateT $ \ _ -> WriterT Nothing
choice (fn:fns) = StateT $ \ loc -> WriterT $
    case runWriterT $ runStateT fn loc of
        Nothing -> runWriterT $ runStateT (choice fns) loc
        Just res -> Just res

consume :: (Char -> Bool) -> Lexer (Located Char)
consume p = StateT $ \ (LexLocation loc remaining) -> WriterT $
    case Text.uncons remaining of
        Just (c, more) | p c -> Just ((Located (Span.new loc 0 1) c, LexLocation (Location.seek 1 loc) more), [])
        _ -> Nothing

get_loc :: Lexer LexLocation
get_loc = get

put_error :: LexError.Error -> Lexer ()
put_error = lift . tell . (:[])

one_or_more :: Lexer a -> Lexer [a]
one_or_more a = a >>= \ res -> (res:) <$> choice [one_or_more a, pure []]
-- tests {{{1
-- TODO: update tests
case_lex :: Assertion
case_lex =
    let src = "abc *&* ( \"adji\n"
    in File.new "a" src >>= \ f ->
    case runWriter $ lex f of
        ((Located _ (Token.AlphaIdentifier (Located _ "abc")) Sequence.:<| Located _ (Token.SymbolIdentifier (Located _ "*&*")) Sequence.:<| Located _ (Token.SingleTypeToken Token.OParen) Sequence.:<| Sequence.Empty, _), [LexError.UnclosedStrLit _]) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

case_lex_empty :: Assertion
case_lex_empty =
    File.new "a" "" >>= \ f ->
    case runWriter $ lex f of
        ((Sequence.Empty, _), []) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

lex_test :: (LexLocation -> r) -> Text -> (r -> IO ()) -> IO ()
lex_test fn input check = File.new "a" input >>= \ f -> check $ fn $ (LexLocation (Location.new f) input)
lex_test' :: Lexer r -> Text -> (Maybe (LexLocation, [LexError.Error], r) -> IO ()) -> IO ()
lex_test' fn = lex_test (((\ ((r, loc), errs) -> (loc, errs, r)) <$>) . runWriterT . runStateT fn)
lex_test_fail :: Show r => [Char] -> r -> IO a
lex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"

convert_seq_to_list :: ((Sequence.Seq a, b), c) -> (([a], b), c)
convert_seq_to_list ((a, b), c) = ((toList a, b), c)

case_lex_one_token :: Assertion
case_lex_one_token =
    lex_test (convert_seq_to_list . runWriter . runStateT lex_one_token) "abc" $ \case
        (([Located _ (Token.AlphaIdentifier (Located _ "abc"))], l), [])
            | remaining l == "" -> pure ()

        x -> lex_test_fail "lex_one_token" x

case_lex_comment_single :: Assertion
case_lex_comment_single =
    lex_test' lex_comment "// asdf\nabcde\n" $ \case
        Just (l', [], Sequence.Empty)
            | remaining l' == "abcde\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_single2 :: Assertion
case_lex_comment_single2 =
    lex_test' lex_comment "// asdf" $ \case
        Just (l', [], Sequence.Empty)
            | remaining l' == "" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline :: Assertion
case_lex_comment_multiline =
    lex_test' lex_comment "/* asdf\nasdf */more\n" $ \case
        Just (l', [], Sequence.Empty)
            | remaining l' == "more\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline_nesting :: Assertion
case_lex_comment_multiline_nesting =
    lex_test' lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
        Just (l', [], Sequence.Empty)
            | remaining l' == "more\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_unclosed_multiline :: Assertion
case_lex_comment_unclosed_multiline =
    lex_test' lex_comment "/* asdf" $ \case
        Just (l', [LexError.UnclosedMComment _], Sequence.Empty)
            | remaining l' == "" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_not_comment :: Assertion
case_lex_comment_not_comment =
    lex_test' lex_comment "a" $ \case
        Nothing -> pure ()
        x -> lex_test_fail "lex_comment" x

case_lex_alpha_identifier :: Assertion
case_lex_alpha_identifier =
    lex_test' lex_alpha_identifier "a" $ \case
        Just (l, [], Located _ (Token.AlphaIdentifier (Located _ "a")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_with_numbers :: Assertion
case_lex_alpha_identifier_with_numbers =
    lex_test' lex_alpha_identifier "a12" $ \case
        Just (l, [], Located _ (Token.AlphaIdentifier (Located _ "a12")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_apostrophes :: Assertion
case_lex_alpha_identifier_apostrophes =
    lex_test' lex_alpha_identifier "a''" $ \case
        Just (l, [], Located _ (Token.AlphaIdentifier (Located _ "a''")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_underscore :: Assertion
case_lex_alpha_identifier_underscore =
    lex_test' lex_alpha_identifier "_a" $ \case
        Just (l, [], Located _ (Token.AlphaIdentifier (Located _ "_a")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_symbol_identifier :: Assertion
case_lex_symbol_identifier =
    lex_test' lex_symbol_identifier "*" $ \case
        Just (l, [], Located _ (Token.SymbolIdentifier (Located _ "*")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_multiple :: Assertion
case_lex_symbol_identifier_multiple =
    lex_test' lex_symbol_identifier "*^&*&" $ \case
        Just (l, [], Located _ (Token.SymbolIdentifier (Located _ "*^&*&")) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_kw :: Assertion
case_lex_symbol_identifier_kw =
    lex_test' lex_symbol_identifier ":" $ \case
        Just (l, [], Located _ (Token.SingleTypeToken Token.Colon) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_long_kw :: Assertion
case_lex_symbol_identifier_long_kw =
    lex_test' lex_symbol_identifier "->" $ \case
        Just (l, [], Located _ (Token.SingleTypeToken Token.Arrow) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_char_lit :: Assertion
case_lex_char_lit =
    lex_test' lex_str_or_char_lit "'c'" $ \case
        Just (l, [], Located _ (Token.Char 'c') Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_unclosed :: Assertion
case_lex_char_lit_unclosed =
    lex_test' lex_str_or_char_lit "'c" $ \case
        Just (l, [LexError.UnclosedCharLit _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_multiple :: Assertion
case_lex_char_lit_multiple =
    lex_test' lex_str_or_char_lit "'cab'" $ \case
        Just (l, [LexError.MulticharCharLit _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_str_lit :: Assertion
case_lex_str_lit =
    lex_test' lex_str_or_char_lit "\"abcde\"" $ \case
        Just (l, [], Located _ (Token.String "abcde") Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_str_lit_unclosed :: Assertion
case_lex_str_lit_unclosed =
    lex_test' lex_str_or_char_lit "\"cjfwoeifj" $ \case
        Just (l, [LexError.UnclosedStrLit _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_number_decimal :: Assertion
case_lex_number_decimal =
    lex_test' lex_number "1234" $ \case
        Just (l, [], Located _ (Token.Int Token.Dec 1234) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_leading_0 :: Assertion
case_lex_number_decimal_leading_0 =
    lex_test' lex_number "01234" $ \case
        Just (l, [], Located _ (Token.Int Token.Dec 1234) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_float :: Assertion
case_lex_number_float =
    lex_test' lex_number "1234.1234" $ \case
        Just (l, [], Located _ (Token.Float 1234.1234) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary :: Assertion
case_lex_number_binary =
    lex_test' lex_number "0b101" $ \case
        Just (l, [], Located _ (Token.Int Token.Bin 5) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_hex :: Assertion
case_lex_number_hex =
    lex_test' lex_number "0xf1abcABC" $ \case
        Just (l, [], Located _ (Token.Int Token.Hex 4054567612) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal :: Assertion
case_lex_number_octal =
    lex_test' lex_number "0o765" $ \case
        Just (l, [], Located _ (Token.Int Token.Oct 501) Sequence.:<| Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_leading_point :: Assertion
case_lex_number_leading_point =
    lex_test' lex_number ".123" $ \case
        Nothing -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_with_invalid_float :: Assertion
case_lex_number_with_invalid_float =
    lex_test' lex_number "123.x" $ \case
        Just (l, [], Located _ (Token.Int Token.Dec 123) Sequence.:<| Sequence.Empty)
            | remaining l == ".x" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_no_float_digits :: Assertion
case_lex_number_no_float_digits =
    lex_test' lex_number "123." $ \case
        Just (l, [], Located _ (Token.Int Token.Dec 123) Sequence.:<| Sequence.Empty)
            | remaining l == "." -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_decimal :: Assertion
case_lex_number_binary_decimal =
    lex_test' lex_number "0b101.1" $ \case
        Just (l, [LexError.NonDecimalFloat _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_invalid :: Assertion
case_lex_number_binary_invalid =
    lex_test' lex_number "0b29a" $ \case
        Just (l, [LexError.InvalidIntDigit '2' _, LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_invalid :: Assertion
case_lex_number_decimal_invalid =
    lex_test' lex_number "20ab3" $ \case
        Just (l, [LexError.InvalidIntDigit 'a' _, LexError.InvalidIntDigit 'b' _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal_invalid :: Assertion
case_lex_number_octal_invalid =
    lex_test' lex_number "0o79a" $ \case
        Just (l, [LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_invalid_base :: Assertion
case_lex_number_invalid_base =
    lex_test' lex_number "0a98a" $ \case
        Just (l, [LexError.InvalidIntBase 'a' _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_space_space :: Assertion
case_lex_space_space =
    lex_test' lex_space " a" $ \case
        Just (l, [], Sequence.Empty)
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_newline :: Assertion
case_lex_space_newline =
    lex_test' lex_space "\na" $ \case
        Just (l, [], Sequence.Empty)
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_tab :: Assertion
case_lex_space_tab =
    lex_test' lex_space "\ta" $ \case
        Just (l, [], Sequence.Empty)
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_vertical_tab :: Assertion
case_lex_space_vertical_tab =
    lex_test' lex_space "\va" $ \case
        Just (l, [], Sequence.Empty)
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x

case_lex_make_bad_char :: Assertion
case_lex_make_bad_char =
    lex_test' make_bad_char "a" $ \case
        Just (l, [LexError.BadChar 'a' _], Sequence.Empty)
            | remaining l == "" -> pure ()
        x -> lex_test_fail "make_bad_char" x
case_lex_make_bad_char_empty :: Assertion
case_lex_make_bad_char_empty =
    lex_test' make_bad_char "" $ \case
        Nothing -> pure ()
        x -> lex_test_fail "make_bad_char" x

tests :: TestTree
tests = $(testGroupGenerator)
