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

import qualified Pipes
import qualified Pipes.Prelude

-- lexing {{{1
lex :: File -> Pipes.Producer Token.LToken (Compiler.WithDiagnostics LexError.Error Void) Void
lex f = evalStateT run (Location.new f)
    where
        eof = Located (Span.end_of_file f) (Token.T'EOF Token.EOF)

        run :: StateT Location (Pipes.Producer Token.LToken (Compiler.WithDiagnostics LexError.Error Void)) Void
        run =
            get >>= \ l ->
            if at_end l
                then lift yield_eof_forever
                else lex_one_token >> run

        yield_eof_forever = Pipes.yield eof >> yield_eof_forever
-- lex_one_token {{{2
lex_one_token :: StateT Location (Pipes.Producer Token.LToken (Compiler.WithDiagnostics LexError.Error Void)) ()
lex_one_token =
    StateT $ \ loc -> do
        let (chosen_tokens, next_loc) = head $ mapMaybe (\ ml -> runStateT ml loc)
                [ lex_comment

                , lex_delimiter

                , lex_alpha_identifier
                , lex_symbol_identifier

                , lex_str_or_char_lit
                , lex_number

                , lex_space
                , make_bad_char
                ]

        mapM_
            (\case
                Right tok -> Pipes.yield tok
                Left err -> lift (Compiler.tell_error err) >> pure ())
            chosen_tokens

        pure ((), next_loc)
-- MiniLexer {{{2
type MiniLexer = StateT Location Maybe
-- lexing functions {{{2
-- TODO: lex comment only if the // is not another operator
lex_comment :: MiniLexer [Either LexError.Error Token.LToken]
lex_comment =
    get_loc >>= \ start_loc ->
    let singleline = consume (=='/') >> consume (=='/') >> singleline_body
        singleline_body =
            choice
                [ consume (/='\n') >> singleline_body
                , consume (=='\n') >> pure []
                , pure []]

        multiline = consume (=='/') >> consume (=='*') >> multiline_body
        multiline_body =
            choice
                [ consume (=='*') >> consume (=='/') >> pure []
                , multiline >> multiline_body
                , consume (const True) >> multiline_body
                , get_loc >>= \ loc -> pure [Left $ LexError.UnclosedMComment $ new_span_start_and_end start_loc loc]
                ]
    in choice [singleline, multiline]

lex_delimiter :: MiniLexer [Either LexError.Error Token.LToken]
lex_delimiter =
    choice
        [ consume (==':') >>= \ c1 -> consume (==':') >>= \ c2 -> pure [Right $ Located (Located.just_span c1 <> Located.just_span c2) (Token.T'DoubleColon Token.DoubleColon)]
        , consume (=='(') >>= \ ch -> pure [Right $ Token.T'OParen <$> (Token.OParen <$ ch)]
        , consume (==')') >>= \ ch -> pure [Right $ Token.T'CParen <$> (Token.CParen <$ ch)]
        , consume (=='[') >>= \ ch -> pure [Right $ Token.T'OBrack <$> (Token.OBrack <$ ch)]
        , consume (==']') >>= \ ch -> pure [Right $ Token.T'CBrack <$> (Token.CBrack <$ ch)]
        , consume (=='{') >>= \ ch -> pure [Right $ Token.T'OBrace <$> (Token.OBrace <$ ch)]
        , consume (=='}') >>= \ ch -> pure [Right $ Token.T'CBrace <$> (Token.CBrace <$ ch)]
        , consume (==';') >>= \ ch -> pure [Right $ Token.T'Semicolon <$> (Token.Semicolon <$ ch)]
        , consume (==',') >>= \ ch -> pure [Right $ Token.T'Comma <$> (Token.Comma <$ ch)]
        ]

lex_id_or_kw :: (Char -> Bool) -> (Char -> Bool) -> Map Text Token.Token -> (Text -> Token.Token) -> MiniLexer [Either LexError.Error Token.LToken]
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
    pure [Right $ Located (new_span_start_and_end start_loc end_loc) tok]

lex_alpha_identifier :: MiniLexer [Either LexError.Error Token.LToken]
lex_alpha_identifier =
    lex_id_or_kw
        (\ ch -> isAlpha ch || ch == '_')
        (\ ch -> isAlpha ch || isDigit ch || ch == '_' || ch == '\'')
        [ ("_", Token.T'Underscore Token.Underscore)
        , ("root", Token.T'Root Token.Root)
        , ("let", Token.T'Let Token.Let)
        , ("letrec", Token.T'LetRec Token.LetRec)
        , ("where", Token.T'Where Token.Where)
        , ("data", Token.T'Data Token.Data)
        , ("impl", Token.T'Impl Token.Impl)
        , ("if", Token.T'If Token.If)
        , ("then", Token.T'Then Token.Then)
        , ("else", Token.T'Else Token.Else)
        , ("match", Token.T'Match Token.Match)
        , ("typesyn", Token.T'TypeSyn Token.TypeSyn)
        , ("true", Token.T'Bool $ Token.Bool True)
        , ("false", Token.T'Bool $ Token.Bool False)
        ]
        (\ t ->
            if Text.last t == ':'
                then Token.T'KeywordIdentifier $ Token.KeywordIdentifier $ Text.init t
                else Token.T'AlphaIdentifier $ Token.AlphaIdentifier t
        )

lex_symbol_identifier :: MiniLexer [Either LexError.Error Token.LToken]
lex_symbol_identifier =
    lex_id_or_kw
        (`elem` ("~!@#$%^&*+`-=|:./<>?\\" :: [Char]))
        (`elem` ("~!@#$%^&*+`-=|:./<>?\\" :: [Char]))
        [ ("->", Token.T'Arrow Token.Arrow)
        , ("#", Token.T'Hash Token.Hash)
        , ("=", Token.T'Equal Token.Equal)
        , (":", Token.T'Colon Token.Colon)
        , ("@", Token.T'At Token.At)
        , ("?", Token.T'Question Token.Question)
        , ("\\", Token.T'Backslash Token.Backslash)
        , ("^", Token.T'Caret Token.Caret)
        , ("`", Token.T'Backtick Token.Backtick) -- TODO: backticks are delimiters?
        ]
        (\ t ->
            if Text.last t == ':'
                then Token.T'KeywordIdentifier $ Token.KeywordIdentifier $ Text.init t
                else Token.T'SymbolIdentifier $ Token.SymbolIdentifier t
        )

lex_str_or_char_lit :: MiniLexer [Either LexError.Error Token.LToken]
lex_str_or_char_lit =
    get_loc >>= \ start_loc ->
    consume (\ c -> c == '\'' || c == '"') >>= \ (Located _ open) ->
    zero_or_more (Located.unlocate <$> consume (\ ch -> ch /= open && ch /= '\n')) >>= \ contents ->
    choice
        [ consume (==open) >>
          get_loc >>= \ end_loc ->
          let sp = new_span_start_and_end start_loc end_loc
          in if open == '\''
              then case contents of
                    [c] -> pure [Right $ Located sp $ Token.T'Char $ Token.Char c]
                    _ -> pure [Left $ LexError.Non1CharLit sp]
              else pure [Right $ Located sp $ Token.T'String $ Token.String $ Text.pack contents]

        , get_loc >>= \ end_loc ->
          let sp = new_span_start_and_end start_loc end_loc
          in if open == '\''
              then pure [Left $ LexError.UnclosedCharLit sp]
              else pure [Left $ LexError.UnclosedStrLit sp]
        ]

lex_number :: MiniLexer [Either LexError.Error Token.LToken]
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
                then pure [Right $ Located num_span (Token.T'Int $ Token.Int tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (map Located.unlocate (reverse digits_no_underscore)))))]
                else pure (map Left illegal_digits)

        (Right (tok_base, _), _) ->
            let illegal_digits = check_digits isDigit (digits_no_underscore ++ floats)
                base_is_dec = if tok_base == Token.Dec then [] else [LexError.NonDecimalFloat num_span]

            in if null illegal_digits && null base_is_dec
                then pure [Right $ Located num_span (Token.T'Float $ Token.Float $ read_digits ((^^) :: Rational -> Int -> Rational) 10 (zip [0..] (map Located.unlocate $ reverse digits_no_underscore) ++ zip [-1, -2..] (map Located.unlocate floats)))]
                else pure (map Left (illegal_digits ++ base_is_dec))

        (Left err, _) -> pure [Left err]
    where
        lex_base = consume (=='0') >> consume isAlpha
        lex_digits = one_or_more (consume (\ c -> isHexDigit c || c == '_'))
        lex_float = consume (=='.') >> lex_digits

lex_space :: MiniLexer [Either LexError.Error Token.LToken]
lex_space = consume isSpace >> pure []

make_bad_char :: MiniLexer [Either LexError.Error Token.LToken]
make_bad_char = consume (const True) >>= \ (Located sp c) -> pure [Left $ LexError.BadChar c sp]
-- helper functions {{{1
remaining :: Location -> Text
remaining = Location.loc_remaining_in_file

at_end :: Location -> Bool
at_end = Text.null . remaining

new_span_start_and_end :: Location -> Location -> Span
-- start and end should be in the same file because the lex function never processes more than one file at a time
new_span_start_and_end start end = Span.new start 0 (Location.loc_ind end - Location.loc_ind start)

choice :: [MiniLexer a] -> MiniLexer a
choice [] = StateT $ \ _ -> Nothing
choice (fn:fns) = StateT $ \ loc ->
    case runStateT fn loc of
        Nothing -> runStateT (choice fns) loc
        Just res -> Just res

consume :: (Char -> Bool) -> MiniLexer (Located Char)
consume p = StateT $ \ loc ->
    case Text.uncons $ remaining loc of
        Just (c, _) | p c -> Just (Located (Span.new loc 0 1) c, Location.seek_1 loc)
        _ -> Nothing

get_loc :: MiniLexer Location
get_loc = get

one_or_more :: MiniLexer a -> MiniLexer [a]
one_or_more a = a >>= \ res -> (res:) <$> choice [one_or_more a, pure []]

zero_or_more :: MiniLexer a -> MiniLexer [a]
zero_or_more a = choice [one_or_more a, pure []]
-- tests {{{1
-- TODO: update tests
take_token_stream_until_eof :: Monad m => Pipes.Producer Token.LToken m Void -> m [Token.LToken]
take_token_stream_until_eof = go []
    where
        go acc producer =  do
            Pipes.next producer >>= \case
                Right (Located _ (Token.T'EOF _), _) -> pure acc
                Right (tok, more) -> go (acc ++ [tok]) more
                Left void -> absurd void

case_lex :: Assertion
case_lex =
    let src = "abc *&* ( \"adji\n"
    in File.new "a" src >>= \ f ->
    case runWriter $ take_token_stream_until_eof $ lex f of
        ([Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "abc")), Located _ (Token.T'SymbolIdentifier (Token.SymbolIdentifier "*&*")), Located _ (Token.T'OParen Token.OParen)], Compiler.Diagnostics [LexError.UnclosedStrLit _] []) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

case_lex_empty :: Assertion
case_lex_empty =
    File.new "a" "" >>= \ f ->
    case runWriter $ take_token_stream_until_eof $ lex f of
        ([], Compiler.Diagnostics [] []) -> pure ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

minilex_test :: (Location -> r) -> Text -> (r -> IO ()) -> IO ()
minilex_test fn input check = File.new "a" input >>= \ f -> check $ fn $ Location.new f
minilex_test' :: MiniLexer r -> Text -> (Maybe (Location, r) -> IO ()) -> IO ()
minilex_test' fn = minilex_test (fmap (\ (r, loc) -> (loc, r)) . runStateT fn)
minilex_test_fail :: Show r => [Char] -> r -> IO a
minilex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"

case_lex_one_token :: Assertion
case_lex_one_token =
    minilex_test (runWriter . Pipes.Prelude.toListM' . runStateT lex_one_token) "abc" $ \case
        (([Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "abc"))], ((), l)), Compiler.Diagnostics [] [])
            | remaining l == "" -> pure ()

        x -> minilex_test_fail "lex_one_token" x

case_lex_comment_single :: Assertion
case_lex_comment_single =
    minilex_test' lex_comment "// asdf\nabcde\n" $ \case
        Just (l', [])
            | remaining l' == "abcde\n" -> pure ()
        x -> minilex_test_fail "lex_comment" x
case_lex_comment_single2 :: Assertion
case_lex_comment_single2 =
    minilex_test' lex_comment "// asdf" $ \case
        Just (l', [])
            | remaining l' == "" -> pure ()
        x -> minilex_test_fail "lex_comment" x
case_lex_comment_multiline :: Assertion
case_lex_comment_multiline =
    minilex_test' lex_comment "/* asdf\nasdf */more\n" $ \case
        Just (l', [])
            | remaining l' == "more\n" -> pure ()
        x -> minilex_test_fail "lex_comment" x
case_lex_comment_multiline_nesting :: Assertion
case_lex_comment_multiline_nesting =
    minilex_test' lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
        Just (l', [])
            | remaining l' == "more\n" -> pure ()
        x -> minilex_test_fail "lex_comment" x
case_lex_comment_unclosed_multiline :: Assertion
case_lex_comment_unclosed_multiline =
    minilex_test' lex_comment "/* asdf" $ \case
        Just (l', [Left (LexError.UnclosedMComment _)])
            | remaining l' == "" -> pure ()
        x -> minilex_test_fail "lex_comment" x
case_lex_comment_not_comment :: Assertion
case_lex_comment_not_comment =
    minilex_test' lex_comment "a" $ \case
        Nothing -> pure ()
        x -> minilex_test_fail "lex_comment" x

case_lex_alpha_identifier :: Assertion
case_lex_alpha_identifier =
    minilex_test' lex_alpha_identifier "a" $ \case
        Just (l, [Right (Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "a")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_with_numbers :: Assertion
case_lex_alpha_identifier_with_numbers =
    minilex_test' lex_alpha_identifier "a12" $ \case
        Just (l, [Right (Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "a12")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_apostrophes :: Assertion
case_lex_alpha_identifier_apostrophes =
    minilex_test' lex_alpha_identifier "a''" $ \case
        Just (l, [Right (Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "a''")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_underscore :: Assertion
case_lex_alpha_identifier_underscore =
    minilex_test' lex_alpha_identifier "_a" $ \case
        Just (l, [Right (Located _ (Token.T'AlphaIdentifier (Token.AlphaIdentifier "_a")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_alpha_identifier" x

case_lex_symbol_identifier :: Assertion
case_lex_symbol_identifier =
    minilex_test' lex_symbol_identifier "*" $ \case
        Just (l, [Right (Located _ (Token.T'SymbolIdentifier (Token.SymbolIdentifier "*")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_multiple :: Assertion
case_lex_symbol_identifier_multiple =
    minilex_test' lex_symbol_identifier "*^&*&" $ \case
        Just (l, [Right (Located _ (Token.T'SymbolIdentifier (Token.SymbolIdentifier "*^&*&")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_kw :: Assertion
case_lex_symbol_identifier_kw =
    minilex_test' lex_symbol_identifier ":" $ \case
        Just (l, [Right (Located _ (Token.T'Colon Token.Colon))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_symbol_identifier" x
case_lex_symbol_identifier_long_kw :: Assertion
case_lex_symbol_identifier_long_kw =
    minilex_test' lex_symbol_identifier "->" $ \case
        Just (l, [Right (Located _ (Token.T'Arrow Token.Arrow))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_symbol_identifier" x

case_lex_delimiter :: Assertion
case_lex_delimiter =
    minilex_test' lex_delimiter "((:))" $ \case
        Just (l, [Right (Located _ (Token.T'OParen Token.OParen))])
            | remaining l == "(:))" -> pure ()
        x -> minilex_test_fail "lex_delimiter" x
case_lex_delimiter_double_colon_then_symbol_identifier :: Assertion
case_lex_delimiter_double_colon_then_symbol_identifier =
    minilex_test' lex_delimiter "::*&" $ \case
        Just (l, [Right (Located _ (Token.T'DoubleColon Token.DoubleColon))])
            | remaining l == "*&" -> pure ()
        x -> minilex_test_fail "lex_delimiter" x

case_lex_char_lit :: Assertion
case_lex_char_lit =
    minilex_test' lex_str_or_char_lit "'c'" $ \case
        Just (l, [Right (Located _ (Token.T'Char (Token.Char 'c')))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_unclosed :: Assertion
case_lex_char_lit_unclosed =
    minilex_test' lex_str_or_char_lit "'c" $ \case
        Just (l, [Left (LexError.UnclosedCharLit _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_multiple :: Assertion
case_lex_char_lit_multiple =
    minilex_test' lex_str_or_char_lit "'cab'" $ \case
        Just (l, [Left (LexError.Non1CharLit _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_empty :: Assertion
case_lex_char_lit_empty =
    minilex_test' lex_str_or_char_lit "\'\'" $ \case
        Just (l, [Left (LexError.Non1CharLit _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x

case_lex_str_lit :: Assertion
case_lex_str_lit =
    minilex_test' lex_str_or_char_lit "\"abcde\"" $ \case
        Just (l, [Right (Located _ (Token.T'String (Token.String "abcde")))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x
case_lex_str_lit_unclosed :: Assertion
case_lex_str_lit_unclosed =
    minilex_test' lex_str_or_char_lit "\"cjfwoeifj" $ \case
        Just (l, [Left (LexError.UnclosedStrLit _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_str_or_char_lit" x

case_lex_number_underscores :: Assertion
case_lex_number_underscores =
    minilex_test' lex_number "12_34__5" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Dec 12345)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_decimal :: Assertion
case_lex_number_decimal =
    minilex_test' lex_number "1234" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Dec 1234)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_decimal_leading_0 :: Assertion
case_lex_number_decimal_leading_0 =
    minilex_test' lex_number "01234" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Dec 1234)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_float :: Assertion
case_lex_number_float =
    minilex_test' lex_number "1234.1234" $ \case
        Just (l, [Right (Located _ (Token.T'Float (Token.Float 1234.1234)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_binary :: Assertion
case_lex_number_binary =
    minilex_test' lex_number "0b101" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Bin 5)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_hex :: Assertion
case_lex_number_hex =
    minilex_test' lex_number "0xf1abcABC" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Hex 4054567612)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_octal :: Assertion
case_lex_number_octal =
    minilex_test' lex_number "0o765" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Oct 501)))])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_leading_point :: Assertion
case_lex_number_leading_point =
    minilex_test' lex_number ".123" $ \case
        Nothing -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_with_invalid_float :: Assertion
case_lex_number_with_invalid_float =
    minilex_test' lex_number "123.x" $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Dec 123)))])
            | remaining l == ".x" -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_no_float_digits :: Assertion
case_lex_number_no_float_digits =
    minilex_test' lex_number "123." $ \case
        Just (l, [Right (Located _ (Token.T'Int (Token.Int Token.Dec 123)))])
            | remaining l == "." -> pure ()
        x -> minilex_test_fail "lex_number" x

case_lex_number_binary_decimal :: Assertion
case_lex_number_binary_decimal =
    minilex_test' lex_number "0b101.1" $ \case
        Just (l, [Left (LexError.NonDecimalFloat _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x
case_lex_number_binary_invalid :: Assertion
case_lex_number_binary_invalid =
    minilex_test' lex_number "0b29a" $ \case
        Just (l, [Left (LexError.InvalidIntDigit '2' _), Left (LexError.InvalidIntDigit '9' _), Left (LexError.InvalidIntDigit 'a' _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x
case_lex_number_decimal_invalid :: Assertion
case_lex_number_decimal_invalid =
    minilex_test' lex_number "20ab3" $ \case
        Just (l, [Left (LexError.InvalidIntDigit 'a' _), Left (LexError.InvalidIntDigit 'b' _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x
case_lex_number_octal_invalid :: Assertion
case_lex_number_octal_invalid =
    minilex_test' lex_number "0o79a" $ \case
        Just (l, [Left (LexError.InvalidIntDigit '9' _), Left (LexError.InvalidIntDigit 'a' _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x
case_lex_number_invalid_base :: Assertion
case_lex_number_invalid_base =
    minilex_test' lex_number "0a98a" $ \case
        Just (l, [Left (LexError.InvalidIntBase 'a' _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "lex_number" x
case_lex_space_space :: Assertion
case_lex_space_space =
    minilex_test' lex_space " a" $ \case
        Just (l, [])
            | remaining l == "a" -> pure ()
        x -> minilex_test_fail "lex_space" x
case_lex_space_newline :: Assertion
case_lex_space_newline =
    minilex_test' lex_space "\na" $ \case
        Just (l, [])
            | remaining l == "a" -> pure ()
        x -> minilex_test_fail "lex_space" x
case_lex_space_tab :: Assertion
case_lex_space_tab =
    minilex_test' lex_space "\ta" $ \case
        Just (l, [])
            | remaining l == "a" -> pure ()
        x -> minilex_test_fail "lex_space" x
case_lex_space_vertical_tab :: Assertion
case_lex_space_vertical_tab =
    minilex_test' lex_space "\va" $ \case
        Just (l, [])
            | remaining l == "a" -> pure ()
        x -> minilex_test_fail "lex_space" x
case_lex_make_bad_char :: Assertion
case_lex_make_bad_char =
    minilex_test' make_bad_char "a" $ \case
        Just (l, [Left (LexError.BadChar 'a' _)])
            | remaining l == "" -> pure ()
        x -> minilex_test_fail "make_bad_char" x
case_lex_make_bad_char_empty :: Assertion
case_lex_make_bad_char_empty =
    minilex_test' make_bad_char "" $ \case
        Nothing -> pure ()
        x -> minilex_test_fail "make_bad_char" x
tests :: TestTree
tests = $(testGroupGenerator)
