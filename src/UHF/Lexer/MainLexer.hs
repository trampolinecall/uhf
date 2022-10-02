{-# LANGUAGE TupleSections #-}

module UHF.Lexer.MainLexer
    ( UHF.Lexer.MainLexer.lex

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.Token as Token

import qualified Data.Text as Text
import qualified Data.Decimal as Decimal

import Data.Maybe (mapMaybe, fromMaybe)
import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, digitToInt)

-- lexing {{{1
lex :: File.File -> ([LexError.LexError], [Token.LUnprocessedToken], Token.LNormalToken)
lex f =
    let (errs, toks) = run [] [] (Location.new_location f)
        eof = Location.Located (Location.eof_span f) (Token.EOF ())
    in (errs, toks, eof)
    where
        run errs_acc toks_acc l
            | Text.null $ remaining l = (errs_acc, toks_acc) -- TODO: somehow encode that the location is not at the end in the lex_one_token type signature
            | otherwise =
                let (l', e, t) = lex_one_token l

                    next_errs = errs_acc ++ e
                    next_toks = toks_acc ++ t

                in seq next_errs $ -- not sure if this actually helps, needs deep seq?
                    seq next_toks $
                    run next_errs next_toks l'
-- lex_one_token {{{2
lex_one_token :: Location.Location -> (Location.Location, [LexError.LexError], [Token.LUnprocessedToken])
lex_one_token loc = head $ mapMaybe (($ loc) . run_lexer) lex_choices
    where
        lex_choices =
            [ lex_comment

            , lex_alpha_identifier
            , lex_symbol_identifier

            , lex_str_or_char_lit
            , lex_number

            , lex_space
            , make_bad_char
            ]
-- lexing functions {{{2
-- TODO: replace all explicit calls of Lexer and run_lexer with combinators
newtype Lexer a = Lexer { run_lexer :: Location.Location -> Maybe (Location.Location, [LexError.LexError], a) }

instance Functor Lexer where
    fmap f a = Lexer $ \ loc ->
        case run_lexer a loc of
            Just (loc', errs, res) -> Just (loc', errs, f res)
            Nothing -> Nothing

instance Applicative Lexer where
    pure a = Lexer $ \ loc -> Just (loc, [], a)
    a <*> b = Lexer $ \ loc ->
        case run_lexer a loc of
            Just (loc', errs, ares) ->
                case run_lexer b loc' of
                    Just (loc'', errs', bres) -> Just (loc'', errs ++ errs', ares bres)
                    _ -> Nothing
            _ -> Nothing

instance Monad Lexer where
    a >>= b = Lexer $ \ loc ->
        case run_lexer a loc of
            Just (loc', errs, res) ->
                case run_lexer (b res) loc' of
                    Just (loc'', errs', res') -> Just (loc'', errs ++ errs', res')
                    _ -> Nothing
            _ -> Nothing

lex_comment :: Lexer [Token.LUnprocessedToken]
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

lex_id_or_kw :: (Char -> Bool) -> (Char -> Bool) -> [(String, Token.UnprocessedToken)] -> (String -> Token.UnprocessedToken) -> Lexer [Token.LUnprocessedToken]
lex_id_or_kw is_valid_start is_valid_char kws def =
    get_loc >>= \ start_loc ->
    consume is_valid_start >>= \ first_char ->
    choice [lex_rest, pure []] >>= \ more ->
    let full = Location.unlocate <$> first_char : more
        tok = fromMaybe (def full) (lookup full kws)
    in get_loc >>= \ end_loc ->
    pure [Location.Located (new_span_start_and_end start_loc end_loc) tok]
    where
        lex_rest = consume is_valid_char >>= \ c -> (c:) <$> choice [lex_rest, pure []]

{-
    Lexer $ \ loc ->
    case Text.uncons $ remaining loc of
        Just (first_char, _)
            | is_valid_start first_char ->
                let (_, more, loc') = (loc `seek` 1) `seek_while` is_valid_char
                    full = first_char : Text.unpack more
                    tok = fromMaybe (def full) (lookup full kws)
                in Just (loc', [], [Location.Located (Location.new_span loc 0 (length full)) tok])

        _ -> Nothing
-}

lex_alpha_identifier :: Lexer [Token.LUnprocessedToken]
lex_alpha_identifier =
    lex_id_or_kw
        (\ ch -> isAlpha ch || ch == '_')
        (\ ch -> isAlpha ch || isDigit ch || ch == '_' || ch == '\'')
        [ ("root", Token.SingleTypeToken Token.Root)
        , ("let", Token.SingleTypeToken Token.Let)
        , ("data", Token.SingleTypeToken Token.Data)
        , ("under", Token.SingleTypeToken Token.Under)
        , ("if", Token.SingleTypeToken Token.If)
        , ("else", Token.SingleTypeToken Token.Else)
        , ("case", Token.SingleTypeToken Token.Case)
        , ("true", Token.SingleTypeToken $ Token.BoolLit True)
        , ("false", Token.SingleTypeToken $ Token.BoolLit False)
        ]
        Token.AlphaIdentifier

lex_symbol_identifier :: Lexer [Token.LUnprocessedToken]
lex_symbol_identifier =
    lex_id_or_kw
        (`elem` ("~!@#$%^&*+`-=|:./<>?()[]\\{};,\n" :: String))
        (`elem` ("~!@#$%^&*+`-=|:./<>?" :: String))
        [ ("->", Token.SingleTypeToken Token.Arrow)
        , ("::", (Token.DoubleColon ()))
        , ("(", Token.SingleTypeToken Token.OParen)
        , (")", Token.SingleTypeToken Token.CParen)
        , ("[", Token.SingleTypeToken Token.OBrack)
        , ("]", Token.SingleTypeToken Token.CBrack)
        , ("\\", Token.Backslash ())
        , ("{", Token.OBrace)
        , ("}", Token.CBrace)
        , (";", Token.Semicolon)
        , (",", Token.SingleTypeToken Token.Comma)
        , ("=", Token.SingleTypeToken Token.Equal)
        , (":", Token.SingleTypeToken Token.Colon)
        , ("\n", Token.Newline Token.NLPhysical)
        ]
        Token.SymbolIdentifier

lex_str_or_char_lit :: Lexer [Token.LUnprocessedToken]
lex_str_or_char_lit = Lexer $ \ loc ->
    case Text.uncons $ remaining loc of
        Just (open, _) | open == '\'' || open == '"' ->
            let (_, str_contents, loc') = (loc `seek` 1) `seek_while` (\ ch -> ch /= open && ch /= '\n')
            in if loc' `matches` [open]
                then
                    let lit_span = Location.new_span loc 0 (Text.length str_contents + 2)
                        loc'' = loc' `seek` 1
                    in if open == '\''
                        then if Text.length str_contents /= 1
                            then Just (loc'', [LexError.MulticharCharLit lit_span], [])
                            else Just (loc'', [], [Location.Located lit_span $ Token.SingleTypeToken $ Token.CharLit $ Text.head str_contents])
                        else Just (loc'', [], [Location.Located lit_span $ Token.SingleTypeToken $ Token.StringLit $ Text.unpack str_contents])
                else if open == '\''
                        then Just (loc', [LexError.UnclosedCharLit $ Location.new_span loc 0 $ Text.length str_contents], [])
                        else Just (loc', [LexError.UnclosedStrLit $ Location.new_span loc 0 $ Text.length str_contents], [])

        _ -> Nothing

lex_number :: Lexer [Token.LUnprocessedToken]
lex_number = Lexer $ \ loc ->
    let lex_base = consume (=='0') >> consume isAlpha

        lex_digits = consume isHexDigit >>= \ c -> (c:) <$> (choice [lex_digits, pure []])

        lex_float = consume (=='.') >> lex_digits

        lex_whole_thing =
            choice [Just <$> lex_base, pure Nothing] >>= \ (m_base) ->
            lex_digits >>= \ digits ->
            choice [lex_float, pure []] >>= \ m_float ->
            pure (m_base, digits, m_float)

    in case run_lexer lex_whole_thing loc of
        Nothing -> Nothing
        Just (_, _:_, _) -> error "unreachable"

        Just (loc', [], (m_base, digits, floats)) ->
            let num_span = new_span_start_and_end loc loc'
                ei_tok_base = case m_base of
                    Just (Location.Located _ 'o') -> Right (Token.Oct, 8)
                    Just (Location.Located _ 'x') -> Right (Token.Hex, 16)
                    Just (Location.Located _ 'b') -> Right (Token.Bin, 2)
                    Nothing -> Right (Token.Dec, 10)

                    Just (Location.Located base_sp c) -> Left $ LexError.InvalidIntBase c base_sp

                read_digits power num_base = sum . map (\ (place, value) -> num_base `power` place * (fromIntegral $ digitToInt value))

                check_digits verify = mapMaybe (\ (Location.Located sp c) -> if verify c then Nothing else Just $ LexError.InvalidIntDigit c sp)

            in case (ei_tok_base, floats) of
                (Right (tok_base, base_num), []) ->
                    let digit_legal = case tok_base of
                            Token.Oct -> isOctDigit
                            Token.Hex -> isHexDigit
                            Token.Bin -> \ c -> c == '0' || c == '1'
                            Token.Dec -> isDigit

                        illegal_digits = check_digits digit_legal digits

                    in if null illegal_digits
                        then Just (loc', [], [Location.Located num_span (Token.SingleTypeToken $ Token.IntLit tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (map Location.unlocate (reverse digits)))))])
                        else Just (loc', illegal_digits, [])

                (Right (tok_base, _), _) ->
                    let illegal_digits = check_digits isDigit (digits ++ floats)
                        base_is_dec = if tok_base == Token.Dec then [] else [LexError.NonDecimalFloat num_span]

                    in if null illegal_digits && null base_is_dec
                        then Just (loc', [], [Location.Located num_span (Token.SingleTypeToken $ Token.FloatLit $ read_digits ((^^) :: Decimal.Decimal -> Int -> Decimal.Decimal) 10 (zip [0..] (map Location.unlocate $ reverse digits) ++ zip [-1, -2..] (map Location.unlocate floats)))])
                        else Just (loc', illegal_digits ++ base_is_dec, [])

                (Left err, _) -> Just (loc', [err], [])

lex_space :: Lexer [Token.LUnprocessedToken]
lex_space = consume isSpace >> pure []

make_bad_char :: Lexer [Token.LUnprocessedToken]
make_bad_char =
    consume (const True) >>= \ (Location.Located sp c) ->
    Lexer (\ loc -> Just (loc, [LexError.BadChar c sp], []))
-- helper functions {{{1
remaining :: Location.Location -> Text.Text
remaining l = Text.drop (Location.ind l) (File.contents $ Location.file l)

new_span_start_and_end :: Location.Location -> Location.Location -> Location.Span
-- start and end should be in the same file because the lex function never processes more than one file at a time
new_span_start_and_end start end = Location.new_span start 0 (Location.ind end - Location.ind start)

matches :: Location.Location -> String -> Bool
matches l s = text_matches s (remaining l)

text_matches :: String -> Text.Text -> Bool
text_matches s t = Text.unpack (Text.take (length s) t) == s

seek_while :: Location.Location -> (Char -> Bool) -> (Location.Span, Text.Text, Location.Location)
seek_while l p =
    let fits = Text.takeWhile p (remaining l)
    in (Location.new_span l 0 (Text.length fits), fits, l `seek` Text.length fits)

seek :: Location.Location -> Int -> Location.Location
seek = flip Location.seek

choice :: [Lexer a] -> Lexer a
choice [] = Lexer $ \ _ -> Nothing
choice (fn:fns) = Lexer $ \ loc ->
    case run_lexer fn loc of
        Nothing -> run_lexer (choice fns) loc
        Just res -> Just res

consume :: (Char -> Bool) -> Lexer (Location.Located Char)
consume p = Lexer $ \ loc ->
    case Text.uncons $ remaining loc of
        Just (c, _)
            | p c -> Just (loc `seek` 1, [], Location.Located (Location.new_span loc 0 1) c)
        _ -> Nothing

get_loc :: Lexer Location.Location
get_loc = Lexer $ \ loc -> Just (loc, [], loc)

put_error :: LexError.LexError -> Lexer ()
put_error err = Lexer $ \ loc -> Just (loc, [err], ())
-- tests {{{1
-- TODO: update tests
case_remaining :: Assertion
case_remaining = "fghijkl" @=? remaining (Location.seek 5 $ Location.new_location (File.File "filename" "abcdefghijkl"))

case_lex :: Assertion
case_lex =
    let src = "abc *&* ( \"adji\n"
        f = File.File "a" src
    in case UHF.Lexer.MainLexer.lex f of
        ([LexError.UnclosedStrLit _], [Location.Located _ (Token.AlphaIdentifier "abc"), Location.Located _ (Token.SymbolIdentifier "*&*"), Location.Located _ (Token.SingleTypeToken Token.OParen), Location.Located _ (Token.Newline Token.NLPhysical)], _) -> return ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

case_lex_empty :: Assertion
case_lex_empty =
    let f = File.File "a" ""
    in case UHF.Lexer.MainLexer.lex f of
        ([], [], _) -> return ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

lex_test :: (Location.Location -> r) -> Text.Text -> (r -> IO ()) -> IO ()
lex_test fn input check = check $ fn $ Location.new_location $ File.File "a" input
lex_test' :: Lexer r -> Text.Text -> (Maybe (Location.Location, [LexError.LexError], r) -> IO ()) -> IO ()
lex_test' fn = lex_test (run_lexer fn)
lex_test_fail :: Show r => String -> r -> IO a
lex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"

case_lex_one_token :: Assertion
case_lex_one_token =
    lex_test lex_one_token "abc" $ \case
        (l, [], [Location.Located _ (Token.AlphaIdentifier "abc")])
            | remaining l == "" -> return ()

        x -> lex_test_fail "lex_one_token" x

case_lex_comment_single :: Assertion
case_lex_comment_single =
    lex_test' lex_comment "// asdf\nabcde\n" $ \case
        Just (l', [], [])
            | remaining l' == "abcde\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_single2 :: Assertion
case_lex_comment_single2 =
    lex_test' lex_comment "// asdf" $ \case
        Just (l', [], [])
            | remaining l' == "" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline :: Assertion
case_lex_comment_multiline =
    lex_test' lex_comment "/* asdf\nasdf */more\n" $ \case
        Just (l', [], [])
            | remaining l' == "more\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline_nesting :: Assertion
case_lex_comment_multiline_nesting =
    lex_test' lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
        Just (l', [], [])
            | remaining l' == "more\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_unclosed_multiline :: Assertion
case_lex_comment_unclosed_multiline =
    lex_test' lex_comment "/* asdf" $ \case
        Just (l', [LexError.UnclosedMComment _], [])
            | remaining l' == "" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_not_comment :: Assertion
case_lex_comment_not_comment =
    lex_test' lex_comment "a" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_comment" x

case_lex_alpha_identifier :: Assertion
case_lex_alpha_identifier =
    lex_test' lex_alpha_identifier "a" $ \case
        Just (l, [], [Location.Located _ (Token.AlphaIdentifier "a")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_with_numbers :: Assertion
case_lex_alpha_identifier_with_numbers =
    lex_test' lex_alpha_identifier "a12" $ \case
        Just (l, [], [Location.Located _ (Token.AlphaIdentifier "a12")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_apostrophes :: Assertion
case_lex_alpha_identifier_apostrophes =
    lex_test' lex_alpha_identifier "a''" $ \case
        Just (l, [], [Location.Located _ (Token.AlphaIdentifier "a''")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_underscore :: Assertion
case_lex_alpha_identifier_underscore =
    lex_test' lex_alpha_identifier "_a" $ \case
        Just (l, [], [Location.Located _ (Token.AlphaIdentifier "_a")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_symbol_identifier :: Assertion
case_lex_symbol_identifier =
    lex_test' lex_symbol_identifier "*" $ \case
        Just (l, [], [Location.Located _ (Token.SymbolIdentifier "*")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_multiple :: Assertion
case_lex_symbol_identifier_multiple =
    lex_test' lex_symbol_identifier "*^&*&" $ \case
        Just (l, [], [Location.Located _ (Token.SymbolIdentifier "*^&*&")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_kw :: Assertion
case_lex_symbol_identifier_kw =
    lex_test' lex_symbol_identifier ":" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken Token.Colon)])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_long_kw :: Assertion
case_lex_symbol_identifier_long_kw =
    lex_test' lex_symbol_identifier "->" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken Token.Arrow)])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_char_lit :: Assertion
case_lex_char_lit =
    lex_test' lex_str_or_char_lit "'c'" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.CharLit 'c'))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_unclosed :: Assertion
case_lex_char_lit_unclosed =
    lex_test' lex_str_or_char_lit "'c" $ \case
        Just (l, [LexError.UnclosedCharLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_multiple :: Assertion
case_lex_char_lit_multiple =
    lex_test' lex_str_or_char_lit "'cab'" $ \case
        Just (l, [LexError.MulticharCharLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_str_lit :: Assertion
case_lex_str_lit =
    lex_test' lex_str_or_char_lit "\"abcde\"" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.StringLit "abcde"))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_str_lit_unclosed :: Assertion
case_lex_str_lit_unclosed =
    lex_test' lex_str_or_char_lit "\"cjfwoeifj" $ \case
        Just (l, [LexError.UnclosedStrLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_number_decimal :: Assertion
case_lex_number_decimal =
    lex_test' lex_number "1234" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_leading_0 :: Assertion
case_lex_number_decimal_leading_0 =
    lex_test' lex_number "01234" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_float :: Assertion
case_lex_number_float =
    lex_test' lex_number "1234.1234" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.FloatLit 1234.1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary :: Assertion
case_lex_number_binary =
    lex_test' lex_number "0b101" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Bin 5))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_hex :: Assertion
case_lex_number_hex =
    lex_test' lex_number "0xf1abcABC" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Hex 4054567612))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal :: Assertion
case_lex_number_octal =
    lex_test' lex_number "0o765" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Oct 501))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_leading_point :: Assertion
case_lex_number_leading_point =
    lex_test' lex_number ".123" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_with_invalid_float :: Assertion
case_lex_number_with_invalid_float =
    lex_test' lex_number "123.x" $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 123))])
            | remaining l == ".x" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_no_float_digits :: Assertion
case_lex_number_no_float_digits =
    lex_test' lex_number "123." $ \case
        Just (l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 123))])
            | remaining l == "." -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_decimal :: Assertion
case_lex_number_binary_decimal =
    lex_test' lex_number "0b101.1" $ \case
        Just (l, [LexError.NonDecimalFloat _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_invalid :: Assertion
case_lex_number_binary_invalid =
    lex_test' lex_number "0b29a" $ \case
        Just (l, [LexError.InvalidIntDigit '2' _, LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_invalid :: Assertion
case_lex_number_decimal_invalid =
    lex_test' lex_number "20ab3" $ \case
        Just (l, [LexError.InvalidIntDigit 'a' _, LexError.InvalidIntDigit 'b' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal_invalid :: Assertion
case_lex_number_octal_invalid =
    lex_test' lex_number "0o79a" $ \case
        Just (l, [LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_invalid_base :: Assertion
case_lex_number_invalid_base =
    lex_test' lex_number "0a98a" $ \case
        Just (l, [LexError.InvalidIntBase 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_space_space :: Assertion
case_lex_space_space =
    lex_test' lex_space " a" $ \case
        Just (l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_newline :: Assertion
case_lex_space_newline =
    lex_test' lex_space "\na" $ \case
        Just (l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_tab :: Assertion
case_lex_space_tab =
    lex_test' lex_space "\ta" $ \case
        Just (l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_vertical_tab :: Assertion
case_lex_space_vertical_tab =
    lex_test' lex_space "\va" $ \case
        Just (l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x

case_lex_make_bad_char :: Assertion
case_lex_make_bad_char =
    lex_test' make_bad_char "a" $ \case
        Just (l, [LexError.BadChar 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "make_bad_char" x
case_lex_make_bad_char_empty :: Assertion
case_lex_make_bad_char_empty =
    lex_test' make_bad_char "" $ \case
        Nothing -> return ()
        x -> lex_test_fail "make_bad_char" x

tests :: TestTree
tests = $(testGroupGenerator)
