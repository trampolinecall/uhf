{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module UHF.Parts.Lexer
    ( lex

    , LexError.Error
    , tests
    ) where

import UHF.Prelude

import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, digitToInt)
import qualified Data.Map as Map
import qualified Data.Text as Text

import UHF.Source.File (File)
import UHF.Source.Located (Located (Located))
import UHF.Source.Location (Location)
import UHF.Source.Span (Span)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.Token as Token
import qualified UHF.Source.File as File
import qualified UHF.Source.Located as Located
import qualified UHF.Source.Location as Location
import qualified UHF.Source.Span as Span
import qualified UHF.Parts.Lexer.LexError as LexError

-- lexing {{{1
lex :: File -> Compiler.WithDiagnostics LexError.Error Void ([Token.LToken], Token.LToken)
lex f =
    let eof = Located (Span.end_of_file f) (Token.EOF ())
    in evalStateT (run []) (Location.new f) >>= \ toks ->
    pure (toList toks, eof)

    where
        run toks =
            get >>= \ l ->
            if at_end l
                then pure toks
                else lex_one_token >>= \ more -> run (toks <> more)
-- lex_one_token {{{2
lex_one_token :: StateT Location (Compiler.WithDiagnostics LexError.Error Void) (Seq Token.LToken)
lex_one_token =
    StateT $ \ loc ->
        writer $
            head $ mapMaybe (runWriterT . ($ loc) . runStateT)
                [ lex_comment

                , lex_delimiter

                , lex_alpha_identifier
                , lex_symbol_identifier

                , lex_str_or_char_lit
                , lex_number

                , lex_space
                , make_bad_char
                ]
-- Lexer {{{2
type Lexer = StateT Location (Compiler.WithDiagnosticsT LexError.Error Void Maybe)
-- lexing functions {{{2
lex_comment :: Lexer (Seq Token.LToken)
lex_comment =
    get_loc >>= \ start_loc ->
    let lex_singleline = consume (=='/') >> consume (=='/') >> lex_singleline_body
        lex_singleline_body =
            choice
                [ consume (/='\n') >> lex_singleline_body
                , consume (=='\n') >> pure []
                , pure []]

        lex_multiline = consume (=='/') >> consume (=='*') >> lex_multiline_body
        lex_multiline_body =
            choice
                [ consume (=='*') >> consume (=='/') >> pure []
                , lex_multiline >> lex_multiline_body
                , consume (const True) >> lex_multiline_body
                , get_loc >>= \ loc -> put_error (LexError.UnclosedMComment $ new_span_start_and_end start_loc loc) >> pure []
                ]
    in choice [lex_singleline, lex_multiline]

lex_id_or_kw :: (Char -> Bool) -> (Char -> Bool) -> Map Text Token.Token -> (Text -> Token.Token) -> Lexer (Seq Token.LToken)
lex_id_or_kw is_valid_start is_valid_char kws def =
    get_loc >>= \ start_loc ->
    consume is_valid_start >>= \ first_char ->
    choice [one_or_more $ consume is_valid_char, pure []] >>= \ more ->
    let full = Located.unlocate $
            let chars = first_char : more
                sp = Located.just_span (head chars) <> Located.just_span (last chars)
            in Located sp (Text.pack $ Located.unlocate <$> chars)
        tok = fromMaybe (def full) (Map.lookup full kws)
    in get_loc >>= \ end_loc ->
    pure [Located (new_span_start_and_end start_loc end_loc) tok]

lex_delimiter :: Lexer (Seq Token.LToken)
lex_delimiter =
    choice
        [ consume (==':') >>= \ c1 -> consume (==':') >>= \ c2 -> pure [Located (Located.just_span c1 <> Located.just_span c2) (Token.SingleTypeToken Token.DoubleColon)]
        , consume (=='(') >>= \ ch -> pure [Token.SingleTypeToken Token.OParen <$ ch]
        , consume (==')') >>= \ ch -> pure [Token.SingleTypeToken Token.CParen <$ ch]
        , consume (=='[') >>= \ ch -> pure [Token.SingleTypeToken Token.OBrack <$ ch]
        , consume (==']') >>= \ ch -> pure [Token.SingleTypeToken Token.CBrack <$ ch]
        , consume (=='{') >>= \ ch -> pure [Token.SingleTypeToken Token.OBrace <$ ch]
        , consume (=='}') >>= \ ch -> pure [Token.SingleTypeToken Token.CBrace <$ ch]
        , consume (==';') >>= \ ch -> pure [Token.SingleTypeToken Token.Semicolon <$ ch]
        , consume (==',') >>= \ ch -> pure [Token.SingleTypeToken Token.Comma <$ ch]
        ]

lex_alpha_identifier :: Lexer (Seq Token.LToken)
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
        , ("match", Token.SingleTypeToken Token.Match)
        , ("typesyn", Token.SingleTypeToken Token.TypeSyn)
        , ("true", Token.Bool True)
        , ("false", Token.Bool False)
        ]
        Token.AlphaIdentifier

lex_symbol_identifier :: Lexer (Seq Token.LToken)
lex_symbol_identifier =
    lex_id_or_kw
        (`elem` ("~!@#$%^&*+`-=|:./<>?\\" :: [Char]))
        (`elem` ("~!@#$%^&*+`-=|:./<>?\\" :: [Char]))
        [ ("->", Token.SingleTypeToken Token.Arrow)
        , ("#", Token.SingleTypeToken Token.Hash)
        , ("=", Token.SingleTypeToken Token.Equal)
        , (":", Token.SingleTypeToken Token.Colon)
        , ("@", Token.SingleTypeToken Token.At)
        , ("?", Token.SingleTypeToken Token.Question)
        , ("\\", Token.SingleTypeToken Token.Backslash)
        ]
        Token.SymbolIdentifier

lex_str_or_char_lit :: Lexer (Seq Token.LToken)
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
                    [c] -> pure [Located sp $ Token.Char c]
                    _ -> put_error (LexError.MulticharCharLit sp) >> pure []
              else pure [Located sp $ Token.String $ Text.pack contents]

        , get_loc >>= \ end_loc ->
          let sp = new_span_start_and_end start_loc end_loc
          in if open == '\''
              then put_error (LexError.UnclosedCharLit sp) >> pure []
              else put_error (LexError.UnclosedStrLit sp) >> pure []
        ]

lex_number :: Lexer (Seq Token.LToken)
lex_number =
    get_loc >>= \ start_loc ->

    choice [Just <$> lex_base, pure Nothing] >>= \ m_base ->
    lex_digits >>= \ digits ->
    choice [lex_float, pure []] >>= \ floats ->

    get_loc >>= \ end_loc ->

    let digits_no_underscore = filter ((/='_') . Located.unlocate) digits
        num_span = new_span_start_and_end start_loc end_loc
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

                illegal_digits = check_digits digit_legal digits_no_underscore

            in if null illegal_digits
                then pure [Located num_span (Token.Int tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (map Located.unlocate (reverse digits_no_underscore)))))]
                else mapM_ put_error illegal_digits >> pure []

        (Right (tok_base, _), _) ->
            let illegal_digits = check_digits isDigit (digits_no_underscore ++ floats)
                base_is_dec = if tok_base == Token.Dec then [] else [LexError.NonDecimalFloat num_span]

            in if null illegal_digits && null base_is_dec
                then pure [Located num_span (Token.Float $ read_digits ((^^) :: Rational -> Int -> Rational) 10 (zip [0..] (map Located.unlocate $ reverse digits_no_underscore) ++ zip [-1, -2..] (map Located.unlocate floats)))]
                else mapM_ put_error (illegal_digits ++ base_is_dec) >> pure []

        (Left err, _) -> put_error err >> pure []
    where
        lex_base = consume (=='0') >> consume isAlpha
        lex_digits = one_or_more (consume (\ c -> isHexDigit c || c == '_'))
        lex_float = consume (=='.') >> lex_digits

lex_space :: Lexer (Seq Token.LToken)
lex_space = consume isSpace >> pure []

make_bad_char :: Lexer (Seq Token.LToken)
make_bad_char = consume (const True) >>= \ (Located sp c) -> put_error (LexError.BadChar c sp) >> pure []
-- helper functions {{{1
remaining :: Location -> Text
remaining = Location.loc_remaining_in_file

at_end :: Location -> Bool
at_end = Text.null . remaining

new_span_start_and_end :: Location -> Location -> Span
-- start and end should be in the same file because the lex function never processes more than one file at a time
new_span_start_and_end start end = Span.new start 0 (Location.loc_ind end - Location.loc_ind start)

choice :: [Lexer a] -> Lexer a
choice [] = StateT $ \ _ -> WriterT Nothing
choice (fn:fns) = StateT $ \ loc -> WriterT $
    case runWriterT $ runStateT fn loc of
        Nothing -> runWriterT $ runStateT (choice fns) loc
        Just res -> Just res

consume :: (Char -> Bool) -> Lexer (Located Char)
consume p = StateT $ \ loc ->
    case Text.uncons $ remaining loc of
        Just (c, _) | p c -> lift $ Just (Located (Span.new loc 0 1) c, Location.seek_1 loc)
        _ -> lift Nothing

get_loc :: Lexer Location
get_loc = get

put_error :: LexError.Error -> Lexer Compiler.ErrorReportedPromise
put_error = lift . Compiler.tell_error

one_or_more :: Lexer a -> Lexer [a]
one_or_more a = a >>= \ res -> (res:) <$> choice [one_or_more a, pure []]
-- tests {{{1
-- TODO: update tests
case_lex :: Assertion
case_lex =
    let src = "abc *&* ( \"adji\n"
    in File.new "a" src >>= \ f ->
    case runWriter $ lex f of
        (([Located _ (Token.AlphaIdentifier "abc"), Located _ (Token.SymbolIdentifier "*&*"), Located _ (Token.SingleTypeToken Token.OParen)], _), Compiler.Diagnostics [LexError.UnclosedStrLit _] []) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

case_lex_empty :: Assertion
case_lex_empty =
    File.new "a" "" >>= \ f ->
    case runWriter $ lex f of
        (([], _), Compiler.Diagnostics [] []) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

lex_test :: (Location -> r) -> Text -> (r -> IO ()) -> IO ()
lex_test fn input check = File.new "a" input >>= \ f -> check $ fn $ Location.new f
lex_test' :: Lexer r -> Text -> (Maybe (Location, [LexError.Error], r) -> IO ()) -> IO ()
lex_test' fn = lex_test (((\ ((r, loc), Compiler.Diagnostics errs _) -> (loc, toList errs, r)) <$>) . runWriterT . runStateT fn)
lex_test_fail :: Show r => [Char] -> r -> IO a
lex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"

case_lex_one_token :: Assertion
case_lex_one_token =
    lex_test (runWriter . runStateT lex_one_token) "abc" $ \case
        (([Located _ (Token.AlphaIdentifier "abc")], l), Compiler.Diagnostics [] [])
            | remaining l == "" -> pure ()

        x -> lex_test_fail "lex_one_token" x

case_lex_comment_single :: Assertion
case_lex_comment_single =
    lex_test' lex_comment "// asdf\nabcde\n" $ \case
        Just (l', [], [])
            | remaining l' == "abcde\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_single2 :: Assertion
case_lex_comment_single2 =
    lex_test' lex_comment "// asdf" $ \case
        Just (l', [], [])
            | remaining l' == "" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline :: Assertion
case_lex_comment_multiline =
    lex_test' lex_comment "/* asdf\nasdf */more\n" $ \case
        Just (l', [], [])
            | remaining l' == "more\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline_nesting :: Assertion
case_lex_comment_multiline_nesting =
    lex_test' lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
        Just (l', [], [])
            | remaining l' == "more\n" -> pure ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_unclosed_multiline :: Assertion
case_lex_comment_unclosed_multiline =
    lex_test' lex_comment "/* asdf" $ \case
        Just (l', [LexError.UnclosedMComment _], [])
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
        Just (l, [], [Located _ (Token.AlphaIdentifier "a")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_with_numbers :: Assertion
case_lex_alpha_identifier_with_numbers =
    lex_test' lex_alpha_identifier "a12" $ \case
        Just (l, [], [Located _ (Token.AlphaIdentifier "a12")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_apostrophes :: Assertion
case_lex_alpha_identifier_apostrophes =
    lex_test' lex_alpha_identifier "a''" $ \case
        Just (l, [], [Located _ (Token.AlphaIdentifier "a''")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_underscore :: Assertion
case_lex_alpha_identifier_underscore =
    lex_test' lex_alpha_identifier "_a" $ \case
        Just (l, [], [Located _ (Token.AlphaIdentifier "_a")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_symbol_identifier :: Assertion
case_lex_symbol_identifier =
    lex_test' lex_symbol_identifier "*" $ \case
        Just (l, [], [Located _ (Token.SymbolIdentifier "*")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_multiple :: Assertion
case_lex_symbol_identifier_multiple =
    lex_test' lex_symbol_identifier "*^&*&" $ \case
        Just (l, [], [Located _ (Token.SymbolIdentifier "*^&*&")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_kw :: Assertion
case_lex_symbol_identifier_kw =
    lex_test' lex_symbol_identifier ":" $ \case
        Just (l, [], [Located _ (Token.SingleTypeToken Token.Colon)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_long_kw :: Assertion
case_lex_symbol_identifier_long_kw =
    lex_test' lex_symbol_identifier "->" $ \case
        Just (l, [], [Located _ (Token.SingleTypeToken Token.Arrow)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_symbol_identifier" x

case_lex_delimiter :: Assertion
case_lex_delimiter =
    lex_test' lex_delimiter "((:))" $ \case
        Just (l, [], [Located _ (Token.SingleTypeToken Token.OParen)])
            | remaining l == "(:))" -> pure ()
        x -> lex_test_fail "lex_delimiter" x
case_lex_delimiter_double_colon_then_symbol_identifier :: Assertion
case_lex_delimiter_double_colon_then_symbol_identifier =
    lex_test' lex_delimiter "::*&" $ \case
        Just (l, [], [Located _ (Token.SingleTypeToken Token.DoubleColon)])
            | remaining l == "*&" -> pure ()
        x -> lex_test_fail "lex_delimiter" x

case_lex_char_lit :: Assertion
case_lex_char_lit =
    lex_test' lex_str_or_char_lit "'c'" $ \case
        Just (l, [], [Located _ (Token.Char 'c')])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_unclosed :: Assertion
case_lex_char_lit_unclosed =
    lex_test' lex_str_or_char_lit "'c" $ \case
        Just (l, [LexError.UnclosedCharLit _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_multiple :: Assertion
case_lex_char_lit_multiple =
    lex_test' lex_str_or_char_lit "'cab'" $ \case
        Just (l, [LexError.MulticharCharLit _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_str_lit :: Assertion
case_lex_str_lit =
    lex_test' lex_str_or_char_lit "\"abcde\"" $ \case
        Just (l, [], [Located _ (Token.String "abcde")])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_str_lit_unclosed :: Assertion
case_lex_str_lit_unclosed =
    lex_test' lex_str_or_char_lit "\"cjfwoeifj" $ \case
        Just (l, [LexError.UnclosedStrLit _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_number_underscores :: Assertion
case_lex_number_underscores =
    lex_test' lex_number "12_34__5" $ \case
        Just (l, [], [Located _ (Token.Int Token.Dec 12345)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal :: Assertion
case_lex_number_decimal =
    lex_test' lex_number "1234" $ \case
        Just (l, [], [Located _ (Token.Int Token.Dec 1234)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_leading_0 :: Assertion
case_lex_number_decimal_leading_0 =
    lex_test' lex_number "01234" $ \case
        Just (l, [], [Located _ (Token.Int Token.Dec 1234)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_float :: Assertion
case_lex_number_float =
    lex_test' lex_number "1234.1234" $ \case
        Just (l, [], [Located _ (Token.Float 1234.1234)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary :: Assertion
case_lex_number_binary =
    lex_test' lex_number "0b101" $ \case
        Just (l, [], [Located _ (Token.Int Token.Bin 5)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_hex :: Assertion
case_lex_number_hex =
    lex_test' lex_number "0xf1abcABC" $ \case
        Just (l, [], [Located _ (Token.Int Token.Hex 4054567612)])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal :: Assertion
case_lex_number_octal =
    lex_test' lex_number "0o765" $ \case
        Just (l, [], [Located _ (Token.Int Token.Oct 501)])
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
        Just (l, [], [Located _ (Token.Int Token.Dec 123)])
            | remaining l == ".x" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_no_float_digits :: Assertion
case_lex_number_no_float_digits =
    lex_test' lex_number "123." $ \case
        Just (l, [], [Located _ (Token.Int Token.Dec 123)])
            | remaining l == "." -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_decimal :: Assertion
case_lex_number_binary_decimal =
    lex_test' lex_number "0b101.1" $ \case
        Just (l, [LexError.NonDecimalFloat _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_invalid :: Assertion
case_lex_number_binary_invalid =
    lex_test' lex_number "0b29a" $ \case
        Just (l, [LexError.InvalidIntDigit '2' _, LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_invalid :: Assertion
case_lex_number_decimal_invalid =
    lex_test' lex_number "20ab3" $ \case
        Just (l, [LexError.InvalidIntDigit 'a' _, LexError.InvalidIntDigit 'b' _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal_invalid :: Assertion
case_lex_number_octal_invalid =
    lex_test' lex_number "0o79a" $ \case
        Just (l, [LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_number_invalid_base :: Assertion
case_lex_number_invalid_base =
    lex_test' lex_number "0a98a" $ \case
        Just (l, [LexError.InvalidIntBase 'a' _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "lex_number" x

case_lex_space_space :: Assertion
case_lex_space_space =
    lex_test' lex_space " a" $ \case
        Just (l, [], [])
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_newline :: Assertion
case_lex_space_newline =
    lex_test' lex_space "\na" $ \case
        Just (l, [], [])
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_tab :: Assertion
case_lex_space_tab =
    lex_test' lex_space "\ta" $ \case
        Just (l, [], [])
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x
case_lex_space_vertical_tab :: Assertion
case_lex_space_vertical_tab =
    lex_test' lex_space "\va" $ \case
        Just (l, [], [])
            | remaining l == "a" -> pure ()
        x -> lex_test_fail "lex_space" x

case_lex_make_bad_char :: Assertion
case_lex_make_bad_char =
    lex_test' make_bad_char "a" $ \case
        Just (l, [LexError.BadChar 'a' _], [])
            | remaining l == "" -> pure ()
        x -> lex_test_fail "make_bad_char" x
case_lex_make_bad_char_empty :: Assertion
case_lex_make_bad_char_empty =
    lex_test' make_bad_char "" $ \case
        Nothing -> pure ()
        x -> lex_test_fail "make_bad_char" x

tests :: TestTree
tests = $(testGroupGenerator)
