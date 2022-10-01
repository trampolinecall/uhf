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
import qualified UHF.Lexer.DFA as DFA
import qualified UHF.Token as Token

import qualified Data.Text as Text
import qualified Data.Decimal as Decimal

import Data.Maybe (mapMaybe, isJust, fromMaybe)
import Data.Either (lefts)
import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, digitToInt)

-- datatypes {{{1
newtype Lexer = Lexer { location :: Location.Location }
      deriving (Eq)

instance Show Lexer where
    show l =
        "Lexer { location = " ++ show (location l) ++ ", " ++
        show (Text.unpack (Text.reverse (Text.take 5 (rev_passed l)))) ++ " | " ++ show (Text.unpack (Text.take 5 (remaining l))) ++ " }"

-- lexing {{{1
lex :: File.File -> ([LexError.LexError], [Token.LUnprocessedToken], Token.LNormalToken)
lex f =
    let (errs, toks) = run [] [] (Just $ new_lexer f)
        eof = Location.Located (Location.eof_span f) (Token.EOF ())
    in (errs, toks, eof)
    where
        run e t Nothing = (e, t)
        run errs_acc toks_acc (Just l) =
            let (l', e, t) = lex' l

                next_errs = errs_acc ++ e
                next_toks = toks_acc ++ t

            in seq next_errs $ -- not sure if this actually helps, needs deep seq?
                seq next_toks $
                run next_errs next_toks l'
-- lex' {{{2
lex' :: Lexer -> (Maybe Lexer, [LexError.LexError], [Token.LUnprocessedToken])
lex' lexer = head $ mapMaybe ($ lexer) lex_choices
    where
        lex_choices =
            [ lex_eof
            , lex_comment

            , lex_alpha_identifier
            , lex_symbol_identifier

            , lex_str_or_char_lit
            , lex_number

            , lex_space
            , make_bad_char
            ]
-- lexing functions {{{2
-- TODO: rewrite this to be a monad in order to use combinators, also use Writer for output of LexErrors / tokens?
type LexFn = Lexer -> Maybe (Maybe Lexer, [LexError.LexError], [Token.LUnprocessedToken])

lex_eof :: LexFn
lex_eof lexer
    | Text.null $ remaining lexer = Just (Nothing, [], [])
    | otherwise = Nothing

lex_comment :: LexFn
lex_comment lexer
    | lexer `matches` "//" =
        let (_, _, lexer') = (lexer `seek` 2) `seek_while` (/='\n')
            lexer'' = lexer' `seek` 1 -- to skip newline
        in Just (Just lexer'', [], [])

    | lexer `matches` "/*" =
        let lexer' = lexer `seek` 2

            len_until_comment_end t
                | text_matches "*/" t = Just 0

                | text_matches "/*" t =
                    let dropped_slash_star = Text.drop 2 t
                    in len_until_comment_end dropped_slash_star >>= \ inner_comment_len ->
                    let dropped_inner_comment = Text.drop (inner_comment_len + 2) dropped_slash_star
                    in len_until_comment_end dropped_inner_comment >>= \ after_len ->
                    Just $ 2 + inner_comment_len + 2 + after_len

                | Text.null t = Nothing
                | otherwise = (1 +) <$> len_until_comment_end (Text.drop 1 t)

        in case len_until_comment_end $ remaining lexer' of
            Just len -> Just (Just $ lexer' `seek` (len + 2), [], [])
            Nothing ->
                let input_length = Text.length $ remaining lexer'
                in Just (Just $ lexer' `seek` input_length, [LexError.UnclosedMComment $ lexer_span lexer 0 (input_length + 2)], [])

    | otherwise = Nothing

lex_id_or_kw :: (Char -> Bool) -> (Char -> Bool) -> [(String, Token.UnprocessedToken)] -> (String -> Token.UnprocessedToken) -> LexFn
lex_id_or_kw is_valid_start is_valid_char kws def lexer =
    case Text.uncons $ remaining lexer of
        Just (first_char, _)
            | is_valid_start first_char ->
                let (_, more, lexer') = (lexer `seek` 1) `seek_while` is_valid_char
                    full = first_char : Text.unpack more
                    tok = fromMaybe (def full) (lookup full kws)
                in Just (Just lexer', [], [Location.Located (lexer_span lexer 0 (length full)) tok])

        _ -> Nothing

lex_alpha_identifier :: LexFn
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

lex_symbol_identifier :: LexFn
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

lex_str_or_char_lit :: LexFn
lex_str_or_char_lit lexer =
    case Text.uncons $ remaining lexer of
        Just (open, _) | open == '\'' || open == '"' ->
            let (_, str_contents, lexer') = (lexer `seek` 1) `seek_while` (\ ch -> ch /= open && ch /= '\n')
            in if lexer' `matches` [open]
                then
                    let lit_span = lexer_span lexer 0 (Text.length str_contents + 2)
                        lexer'' = lexer' `seek` 1
                    in if open == '\''
                        then if Text.length str_contents /= 1
                            then Just (Just lexer'', [LexError.MulticharCharLit lit_span], [])
                            else Just (Just lexer'', [], [Location.Located lit_span $ Token.SingleTypeToken $ Token.CharLit $ Text.head str_contents])
                        else Just (Just lexer'', [], [Location.Located lit_span $ Token.SingleTypeToken $ Token.StringLit $ Text.unpack str_contents])
                else if open == '\''
                        then Just (Just lexer', [LexError.UnclosedCharLit $ lexer_span lexer 0 $ Text.length str_contents], [])
                        else Just (Just lexer', [LexError.UnclosedStrLit $ lexer_span lexer 0 $ Text.length str_contents], [])

        _ -> Nothing

lex_number :: LexFn
lex_number lexer =
    let dfa = DFA.DFA
            [ DFA.State -- started at 0
                (\ (base, digits, float) -> \case
                    Just c | isDigit c -> DFA.StateNum 2 (base, digits ++ ['0', c], float)
                    Just c | isAlpha c -> DFA.StateNum 1 (Just c, digits, float)
                    _ -> DFA.Reject
                )

            , DFA.State -- gotten base
                (\ (base, digits, float) -> \case
                    Just c | isHexDigit c -> DFA.StateNum 2 (base, digits ++ [c], float)
                    _ -> DFA.Reject
                )

            , DFA.State -- digits
                (\ (base, digits, float) -> \case
                    Just c | isHexDigit c -> DFA.StateNum 2 (base, digits ++ [c], float)
                    Just '.' -> DFA.StateNum 3 (base, digits, float)
                    _ -> DFA.Accept (base, digits, float)
                )

            , DFA.State -- gotten decimal point
                (\ (base, digits, float) -> \case
                    Just c | isHexDigit c -> DFA.StateNum 4 (base, digits, float ++ [c])
                    _ -> DFA.Reject
                )

            , DFA.State -- float digits
                (\ (base, digits, float) -> \case
                    Just c | isHexDigit c -> DFA.StateNum 4 (base, digits, float ++ [c])
                    _ -> DFA.Accept (base, digits, float)
                )

            ]
            (\ res@(base, digits, float) -> \case
                Just '0' -> DFA.StateNum 0 res
                Just c | isDigit c -> DFA.StateNum 2 (base, digits ++ [c], float)
                _ -> DFA.Reject
            )

    in case lex_dfa dfa lexer (Nothing, "", "") of
        Just (num_span, (base, digits, floats), lexer') ->
            let ei_tok_base = case base of
                    Just 'o' -> Right (Token.Oct, 8)
                    Just 'x' -> Right (Token.Hex, 16)
                    Just 'b' -> Right (Token.Bin, 2)
                    Nothing -> Right (Token.Dec, 10)

                    Just c -> Left $ LexError.InvalidIntBase c (lexer_span lexer 1 1)

                base_len = if isJust base then 2 else 0

                read_digits power num_base = sum . map (\ (place, value) -> num_base `power` place * (fromIntegral $ digitToInt value))

            in case (ei_tok_base, floats) of
                (Right (tok_base, base_num), "") ->
                    let digit_legal = case tok_base of
                            Token.Oct -> isOctDigit
                            Token.Hex -> isHexDigit
                            Token.Bin -> \ c -> c == '0' || c == '1'
                            Token.Dec -> isDigit

                        digits_legal =
                            map
                            (\ (i, c) ->
                                if digit_legal c
                                    then Right ()
                                    else Left $ LexError.InvalidIntDigit c (lexer_span lexer i 1)
                            )
                            (zip [base_len..] digits)

                        illegal_digits = lefts digits_legal

                    in if null illegal_digits
                        then Just (Just lexer', [], [Location.Located num_span (Token.SingleTypeToken $ Token.IntLit tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (reverse digits))))])
                        else Just (Just lexer', illegal_digits, [])

                (Right (tok_base, _), _) ->
                    let digits_legal =
                            map
                            ( \ (i, c) ->
                                if isDigit c || c == '.'
                                    then Right ()
                                    else Left $ LexError.InvalidIntDigit c (lexer_span lexer i 1)
                            )
                            (zip [base_len..] (digits ++ "." ++ floats))

                        illegal_digits = lefts digits_legal

                        base_is_dec = if tok_base == Token.Dec then [] else [LexError.NonDecimalFloat num_span]

                    in if null illegal_digits && null base_is_dec
                        then Just (Just lexer', [], [Location.Located num_span (Token.SingleTypeToken $ Token.FloatLit $ read_digits ((^^) :: Decimal.Decimal -> Int -> Decimal.Decimal) 10 (zip [0..] (reverse digits) ++ zip [-1, -2..] floats))])
                        else Just (Just lexer', illegal_digits ++ base_is_dec, [])

                (Left err, _) -> Just (Just lexer', [err], [])

        Nothing -> Nothing

lex_space :: LexFn
lex_space lexer =
    case Text.uncons $ remaining lexer of
        Just (x, _) | isSpace x -> Just (Just $ lexer `seek` 1, [], [])
        _ -> Nothing

make_bad_char :: LexFn
make_bad_char lexer =
    case Text.uncons $ remaining lexer of
        Nothing -> Nothing
        Just (x, _) -> Just (Just $ lexer `seek` 1, [LexError.BadChar x $ lexer_span lexer 0 1], [])
-- helper functions {{{1
l_contents :: Lexer -> Text.Text
l_contents = File.contents . Location.file . location

l_ind :: Lexer -> Int
l_ind = Location.ind . location

remaining :: Lexer -> Text.Text
remaining l = Text.drop (l_ind l) (l_contents l)

passed :: Lexer -> Text.Text
passed l = Text.take (l_ind l) (l_contents l)

rev_passed :: Lexer -> Text.Text
rev_passed = Text.reverse . passed

new_lexer :: File.File -> Lexer
new_lexer f = Lexer (Location.new_location f)

lexer_span :: Lexer -> Int -> Int -> Location.Span
lexer_span lexer start len = Location.new_span (location lexer) start len

matches :: Lexer -> String -> Bool
matches l s = text_matches s (remaining l)

text_matches :: String -> Text.Text -> Bool
text_matches s t = Text.unpack (Text.take (length s) t) == s

lex_dfa :: DFA.DFA r -> Lexer -> r -> Maybe (Location.Span, r, Lexer)
lex_dfa dfa l start_res =
    case DFA.run_dfa dfa (remaining l) start_res of
        Just (len_consumed, res) -> Just (lexer_span l 0 len_consumed, res, l `seek` len_consumed)
        Nothing -> Nothing
seek_while :: Lexer -> (Char -> Bool) -> (Location.Span, Text.Text, Lexer)
seek_while l p =
    let fits = Text.takeWhile p (remaining l)
    in (lexer_span l 0 (Text.length fits), fits, l `seek` Text.length fits)

seek :: Lexer -> Int -> Lexer
seek l n = l { location = Location.seek n (location l) }
-- tests {{{1
case_l_contents :: Assertion
case_l_contents = "abcdefghijkl" @=? l_contents (Lexer (Location.new_location (File.File "filename" "abcdefghijkl")))
case_remaining :: Assertion
case_remaining = "fghijkl" @=? remaining (Lexer (Location.seek 5 $ Location.new_location (File.File "filename" "abcdefghijkl")))
case_passed :: Assertion
case_passed = "abcde" @=? passed (Lexer (Location.seek 5 $ Location.new_location (File.File "filename" "abcdefghijkl")))
case_rev_passed :: Assertion
case_rev_passed = "edcba" @=? rev_passed (Lexer (Location.seek 5 $ Location.new_location (File.File "filename" "abcdefghijkl")))

case_new_lexer :: Assertion
case_new_lexer =
    let f = File.File "a" "abc"
    in Lexer (Location.new_location f) @=? new_lexer f

case_lex :: Assertion
case_lex =
    let src = "abc *&* ( \"adji\n"
        f = File.File "a" src
    in case UHF.Lexer.MainLexer.lex f of
        ([LexError.UnclosedStrLit _], [Location.Located _ (Token.AlphaIdentifier "abc"), Location.Located _ (Token.SymbolIdentifier "*&*"), Location.Located _ (Token.SingleTypeToken Token.OParen)], _) -> return ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

case_lex_empty :: Assertion
case_lex_empty =
    let f = File.File "a" ""
    in case UHF.Lexer.MainLexer.lex f of
        ([], [], _) -> return ()
        x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

lex_test :: (Lexer -> r) -> Text.Text -> (r -> IO ()) -> IO ()
lex_test fn input check = check $ fn $ new_lexer $ File.File "a" input
lex_test_fail :: Show r => String -> r -> IO a
lex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"

case_lex' :: Assertion
case_lex' =
    lex_test lex' "abc" $ \case
        (Just l, [], [Location.Located _ (Token.AlphaIdentifier "abc")])
            | remaining l == "" -> return ()

        x -> lex_test_fail "lex'" x

case_lex'_empty :: Assertion
case_lex'_empty =
    lex_test lex' "" $ \case
        (Nothing, [], []) -> return ()
        x -> lex_test_fail "lex'" x

case_lex_eof_end :: Assertion
case_lex_eof_end =
    lex_test lex_eof "" $ \case
        Just (Nothing, [], []) -> return ()
        x -> lex_test_fail "lex_eof" x
case_lex_eof_not_end :: Assertion
case_lex_eof_not_end =
    lex_test lex_eof "a" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_eof" x
case_lex_eof_with_dedents :: Assertion
case_lex_eof_with_dedents =
    let f = File.File "a" ""
        l = Lexer (Location.new_location f)
    in case lex_eof l of
        Just (Nothing, [], []) -> return ()
        x -> lex_test_fail "lex_eof" x
case_lex_eof_empty_file :: Assertion
case_lex_eof_empty_file =
    let f = File.File "a" ""
        l = Lexer (Location.new_location f)
    in case lex_eof l of
        Just (Nothing, [], []) -> return ()
        x -> lex_test_fail "lex_eof" x

case_lex_comment_single :: Assertion
case_lex_comment_single =
    lex_test lex_comment "// asdf\nabcde\n" $ \case
        Just (Just l', [], [])
            | remaining l' == "abcde\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_single2 :: Assertion
case_lex_comment_single2 =
    lex_test lex_comment "// asdf" $ \case
        Just (Just l', [], [])
            | remaining l' == "" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline :: Assertion
case_lex_comment_multiline =
    lex_test lex_comment "/* asdf\nasdf */more\n" $ \case
        Just (Just l', [], [])
            | remaining l' == "more\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_multiline_nesting :: Assertion
case_lex_comment_multiline_nesting =
    lex_test lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
        Just (Just l', [], [])
            | remaining l' == "more\n" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_unclosed_multiline :: Assertion
case_lex_comment_unclosed_multiline =
    lex_test lex_comment "/* asdf" $ \case
        Just (Just l', [LexError.UnclosedMComment _], [])
            | remaining l' == "" -> return ()
        x -> lex_test_fail "lex_comment" x
case_lex_comment_not_comment :: Assertion
case_lex_comment_not_comment =
    lex_test lex_comment "a" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_comment" x

case_lex_alpha_identifier :: Assertion
case_lex_alpha_identifier =
    lex_test lex_alpha_identifier "a" $ \case
        Just (Just l, [], [Location.Located _ (Token.AlphaIdentifier "a")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_with_numbers :: Assertion
case_lex_alpha_identifier_with_numbers =
    lex_test lex_alpha_identifier "a12" $ \case
        Just (Just l, [], [Location.Located _ (Token.AlphaIdentifier "a12")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_apostrophes :: Assertion
case_lex_alpha_identifier_apostrophes =
    lex_test lex_alpha_identifier "a''" $ \case
        Just (Just l, [], [Location.Located _ (Token.AlphaIdentifier "a''")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_alpha_identifier_underscore :: Assertion
case_lex_alpha_identifier_underscore =
    lex_test lex_alpha_identifier "_a" $ \case
        Just (Just l, [], [Location.Located _ (Token.AlphaIdentifier "_a")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_symbol_identifier :: Assertion
case_lex_symbol_identifier =
    lex_test lex_symbol_identifier "*" $ \case
        Just (Just l, [], [Location.Located _ (Token.SymbolIdentifier "*")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_multiple :: Assertion
case_lex_symbol_identifier_multiple =
    lex_test lex_symbol_identifier "*^&*&" $ \case
        Just (Just l, [], [Location.Located _ (Token.SymbolIdentifier "*^&*&")])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_kw :: Assertion
case_lex_symbol_identifier_kw =
    lex_test lex_symbol_identifier ":" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken Token.Colon)])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x
case_lex_symbol_identifier_long_kw :: Assertion
case_lex_symbol_identifier_long_kw =
    lex_test lex_symbol_identifier "->" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken Token.Arrow)])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_alpha_identifier" x

case_lex_char_lit :: Assertion
case_lex_char_lit =
    lex_test lex_str_or_char_lit "'c'" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.CharLit 'c'))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_unclosed :: Assertion
case_lex_char_lit_unclosed =
    lex_test lex_str_or_char_lit "'c" $ \case
        Just (Just l, [LexError.UnclosedCharLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_char_lit_multiple :: Assertion
case_lex_char_lit_multiple =
    lex_test lex_str_or_char_lit "'cab'" $ \case
        Just (Just l, [LexError.MulticharCharLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_str_lit :: Assertion
case_lex_str_lit =
    lex_test lex_str_or_char_lit "\"abcde\"" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.StringLit "abcde"))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x
case_lex_str_lit_unclosed :: Assertion
case_lex_str_lit_unclosed =
    lex_test lex_str_or_char_lit "\"cjfwoeifj" $ \case
        Just (Just l, [LexError.UnclosedStrLit _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_str_or_char_lit" x

case_lex_number_decimal :: Assertion
case_lex_number_decimal =
    lex_test lex_number "1234" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_leading_0 :: Assertion
case_lex_number_decimal_leading_0 =
    lex_test lex_number "01234" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Dec 1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_float :: Assertion
case_lex_number_float =
    lex_test lex_number "1234.1234" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.FloatLit 1234.1234))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary :: Assertion
case_lex_number_binary =
    lex_test lex_number "0b101" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Bin 5))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_hex :: Assertion
case_lex_number_hex =
    lex_test lex_number "0xf1abcABC" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Hex 4054567612))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal :: Assertion
case_lex_number_octal =
    lex_test lex_number "0o765" $ \case
        Just (Just l, [], [Location.Located _ (Token.SingleTypeToken (Token.IntLit Token.Oct 501))])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_leading_point :: Assertion
case_lex_number_leading_point =
    lex_test lex_number ".123" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_with_invalid_float :: Assertion
case_lex_number_with_invalid_float =
    lex_test lex_number "123.x" $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_no_float_digits :: Assertion
case_lex_number_no_float_digits =
    lex_test lex_number "123." $ \case
        Nothing -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_decimal :: Assertion
case_lex_number_binary_decimal =
    lex_test lex_number "0b101.1" $ \case
        Just (Just l, [LexError.NonDecimalFloat _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_binary_invalid :: Assertion
case_lex_number_binary_invalid =
    lex_test lex_number "0b29a" $ \case
        Just (Just l, [LexError.InvalidIntDigit '2' _, LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_decimal_invalid :: Assertion
case_lex_number_decimal_invalid =
    lex_test lex_number "20ab3" $ \case
        Just (Just l, [LexError.InvalidIntDigit 'a' _, LexError.InvalidIntDigit 'b' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_octal_invalid :: Assertion
case_lex_number_octal_invalid =
    lex_test lex_number "0o79a" $ \case
        Just (Just l, [LexError.InvalidIntDigit '9' _, LexError.InvalidIntDigit 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_number_invalid_base :: Assertion
case_lex_number_invalid_base =
    lex_test lex_number "0a98a" $ \case
        Just (Just l, [LexError.InvalidIntBase 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "lex_number" x

case_lex_space_space :: Assertion
case_lex_space_space =
    lex_test lex_space " a" $ \case
        Just (Just l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_newline :: Assertion
case_lex_space_newline =
    lex_test lex_space "\na" $ \case
        Just (Just l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_tab :: Assertion
case_lex_space_tab =
    lex_test lex_space "\ta" $ \case
        Just (Just l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x
case_lex_space_vertical_tab :: Assertion
case_lex_space_vertical_tab =
    lex_test lex_space "\va" $ \case
        Just (Just l, [], [])
            | remaining l == "a" -> return ()
        x -> lex_test_fail "lex_space" x

case_lex_make_bad_char :: Assertion
case_lex_make_bad_char =
    lex_test make_bad_char "a" $ \case
        Just (Just l, [LexError.BadChar 'a' _], [])
            | remaining l == "" -> return ()
        x -> lex_test_fail "make_bad_char" x
case_lex_make_bad_char_empty :: Assertion
case_lex_make_bad_char_empty =
    lex_test make_bad_char "" $ \case
        Nothing -> return ()
        x -> lex_test_fail "make_bad_char" x

tests :: TestTree
tests = $(testGroupGenerator)
