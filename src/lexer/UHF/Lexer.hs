{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module UHF.Lexer
    ( UHF.Lexer.lex
    , tests
    ) where

import Test.HUnit
import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.Lexer.DFA as DFA
import qualified Data.Text as Text
import Data.Maybe (mapMaybe, isJust)
import Data.Either (lefts)
import Data.Char (isAlpha, isDigit, isOctDigit, isHexDigit, isSpace, digitToInt)
import Safe (lastMay)
import qualified UHF.Token as Token

-- datatypes {{{1
data Lexer
    = Lexer
      { file :: File.File
      , location :: Int
      , line :: Int
      , col :: Int
      , indent_stack :: [IndentFrame]
      }
      deriving (Eq)

instance Show Lexer where
    show l =
        "Lexer { file = " ++ show (file l) ++ ", at " ++ show (line l) ++ ":" ++ show (col l) ++ " (" ++ show (location l) ++ ")," ++
        show (Text.unpack (Text.reverse (Text.take 5 (rev_passed l)))) ++ " | " ++ show (Text.unpack (Text.take 5 (remaining l))) ++ " }"

data IndentFrame
    = IndentationSensitive Int
    | IndentationInsensitive
    deriving (Eq, Show)

data LexError
    = BadChar Char Location.Span
    | UnclosedMComment Location.Span
    | UnclosedStrLit Location.Span
    | UnclosedCharLit Location.Span
    | MulticharCharLit Location.Span
    | InvalidIntBase Char Location.Span
    | InvalidIntDigit Char Location.Span
    | NonDecimalFloat Location.Span
    | MissingDigits Location.Span
    -- TODO: add 4 fields: new indent level, before indent level, two closest indentation levels
    | BadDedent Location.Span
    deriving (Eq, Show)
-- lexing {{{1
lex :: File.File -> ([LexError], [Location.Located Token.Token])
lex f =
    let run _ Nothing = ([], [])
        run last_tok (Just l) =
            let (l', errs, toks) = lex' l last_tok
                (errs', toks') = run (lastMay toks) l'
            in (errs ++ errs', toks ++ toks')

    in run Nothing (Just $ new_lexer f)
-- lex' {{{2
lex' :: Lexer -> Maybe (Location.Located Token.Token) -> (Maybe Lexer, [LexError], [Location.Located Token.Token])
lex' lexer last_tok =
    let lex_choices =
            [ lex_eof
            , lex_comment

            , lex_alpha_identifier
            , lex_symbol_identifier

            , lex_str_or_char_lit
            , lex_number

            , lex_space
            , make_bad_char
            ]

        (include_indent, lexer', errs, toks) = head $ mapMaybe ($ lexer) lex_choices
    in if include_indent
        then
            let (indent_stack', indent_errs, indent_toks) = lex_indent lexer last_tok
            in ( case lexer' of
                     Just l -> Just $ l { indent_stack = indent_stack' }
                     Nothing -> Nothing
               , indent_errs ++ errs
               , indent_toks ++ toks
               )

        else (lexer', errs, toks)
-- lexing functions {{{2
type LexFn = Lexer -> Maybe (Bool, Maybe Lexer, [LexError], [Location.Located Token.Token])

lex_eof :: LexFn
lex_eof lexer
    | Text.null $ remaining lexer =
        let all_dedents =
                case concatMap make_dedent (init $ indent_stack lexer) of
                    [] -> []
                    dedents -> Location.Located (lexer_span lexer 0 1) Token.Newline : dedents

            make_dedent (IndentationSensitive _) = [Location.Located (lexer_span lexer 0 1) Token.Dedent]
            make_dedent _ = []

        in Just (False, Nothing, [], all_dedents)

    | otherwise = Nothing

lex_comment :: LexFn
lex_comment lexer
    | lexer `matches` "//" =
        let (_, _, lexer') = (lexer `seek` 2) `seek_while` (/='\n')
            lexer'' = lexer' `seek` 1 -- to skip newline
        in Just (False, Just $ lexer'', [], [])

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
            Just len -> Just (False, Just $ lexer' `seek` (len + 2), [], [])
            Nothing ->
                let input_length = Text.length $ remaining lexer'
                in Just (False, Just $ lexer' `seek` input_length, [UnclosedMComment $ lexer_span lexer 0 (input_length + 2)], [])

    | otherwise = Nothing

lex_alpha_identifier :: LexFn
lex_alpha_identifier lexer =
    let is_valid_start ch = isAlpha ch || ch == '_'
        is_valid_iden_char ch = isAlpha ch || isDigit ch || ch == '_' || ch == '\''
    in case Text.uncons $ remaining lexer of
        Just (c, _)
            | is_valid_start c ->
                let (_, more_iden, lexer') = (lexer `seek` 1) `seek_while` is_valid_iden_char
                    full_iden = c : Text.unpack more_iden

                    tok = case full_iden of
                            "root" -> Token.Root
                            "let" -> Token.Let
                            "data" -> Token.Data
                            "under" -> Token.Under
                            "if" -> Token.If
                            "else" -> Token.Else
                            "case" -> Token.Case
                            "true" -> Token.BoolLit True
                            "false" -> Token.BoolLit False
                            _ -> Token.AlphaIdentifier full_iden

                in Just (True, Just lexer', [], [Location.Located (lexer_span lexer 0 (length full_iden)) tok])

        _ -> Nothing

lex_symbol_identifier :: LexFn
lex_symbol_identifier lexer
    | lexer `matches` "(" = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.OParen])
    | lexer `matches` ")" = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.CParen])
    | lexer `matches` "[" = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.OBrack])
    | lexer `matches` "]" = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.CBrack])
    | lexer `matches` "," = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.Comma])
    | lexer `matches` "=" = Just (True, Just $ lexer `seek` 1, [], [Location.Located (lexer_span lexer 0 1) Token.Equal])
    | lexer `matches` "::" = Just (True, Just $ lexer `seek` 2, [], [Location.Located (lexer_span lexer 0 1) Token.DoubleColon])
    | otherwise =
        let is_valid_char = (`elem` ("!#$%&*+-./:<=>?@^`~" :: String))
            (iden_sp, iden, lexer') = lexer `seek_while` is_valid_char
        in if Text.null iden
            then Nothing
            else Just (True, Just lexer', [], [Location.Located iden_sp (Token.SymbolIdentifier $ Text.unpack iden)])

lex_str_or_char_lit :: LexFn
lex_str_or_char_lit lexer =
    case Text.uncons $ remaining lexer of
        Just (open, _) | open == '\'' || open == '"' ->
            let (_, str_contents, lexer') = (lexer `seek` 1) `seek_while` (\ ch -> ch /= open && ch /= '\n')
            in if lexer `matches` [open]
                then
                    let lit_span = lexer_span lexer 0 (Text.length str_contents + 2)
                        lexer'' = lexer' `seek` 1
                    in if open == '\''
                        then if Text.length str_contents /= 1
                            then Just (True, Just lexer'', [MulticharCharLit lit_span], [])
                            else Just (True, Just lexer'', [], [Location.Located lit_span $ Token.CharLit $ Text.head str_contents])
                        else Just (True, Just lexer'', [], [Location.Located lit_span $ Token.StringLit $ Text.unpack str_contents])
                else if open == '\''
                        then Just (True, Just lexer', [UnclosedCharLit $ lexer_span lexer 0 $ Text.length str_contents], [])
                        else Just (True, Just lexer', [UnclosedStrLit $ lexer_span lexer 0 $ Text.length str_contents], [])

        _ -> Nothing

lex_number :: LexFn
lex_number lexer =
    let dfa = DFA.DFA
            [ DFA.State -- started at 0
                (\ (base, digits, float) -> \case
                    Just c | isHexDigit c -> DFA.StateNum 2 (base, digits ++ ['0', c], float)
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
                Just c | isDigit c -> DFA.StateNum 1 (base, digits ++ [c], float)
                _ -> DFA.Reject
            )

    in case lex_dfa dfa lexer (Nothing, "", "") of
        Just (num_span, (base, digits, floats), lexer') ->
            let ei_tok_base = case base of
                    Just 'o' -> Right (Token.Oct, 8)
                    Just 'h' -> Right (Token.Hex, 16)
                    Just 'b' -> Right (Token.Bin, 2)
                    Nothing -> Right (Token.Dec, 10)

                    Just c -> Left $ InvalidIntBase c (lexer_span lexer 1 1)

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
                                    else Left $ InvalidIntDigit c (lexer_span lexer i 1)
                            ) 
                            (zip [base_len..] digits)

                        illegal_digits = lefts digits_legal

                    in if null illegal_digits
                        then Just (True, Just lexer', [], [Location.Located num_span (Token.IntLit tok_base (read_digits ((^) :: Integer -> Int -> Integer) base_num (zip [0..] (reverse digits))))])
                        else Just (True, Just lexer', illegal_digits, [])

                (Right (tok_base, _), _) ->
                    let digits_legal =
                            map
                            ( \ (i, c) ->
                                if isDigit c || c == '.'
                                    then Right ()
                                    else Left $ InvalidIntDigit c (lexer_span lexer i 1)
                            )
                            (zip [base_len..] (digits ++ "." ++ floats))

                        illegal_digits = lefts digits_legal

                        base_is_dec = if tok_base == Token.Dec then [] else [NonDecimalFloat num_span]

                    in if null illegal_digits && null base_is_dec
                        then Just (True, Just lexer', [], [Location.Located num_span (Token.FloatLit $ read_digits (**) 10 (zip [0..] (reverse digits) ++ zip [-1, -2..] floats))])
                        else Just (True, Just lexer', illegal_digits ++ base_is_dec, [])

                (Left err, _) -> Just (True, Just lexer', [err], [])

        Nothing -> Nothing

lex_space :: LexFn
lex_space lexer =
    case Text.uncons $ remaining lexer of
        Just (x, _) | isSpace x -> Just (False, Just $ lexer `seek` 1, [], [])
        _ -> Nothing

make_bad_char :: LexFn
make_bad_char lexer =
    case Text.uncons $ remaining lexer of
        Nothing -> Nothing
        Just (x, _) -> Just (True, Just $ lexer `seek` 1, [BadChar x $ lexer_span lexer 0 1], [])
-- lex_indent {{{2
lex_indent :: Lexer -> Maybe (Location.Located Token.Token) -> ([IndentFrame], [LexError], [Location.Located Token.Token])
lex_indent _ _ = undefined
-- helper functions {{{1
l_contents :: Lexer -> Text.Text
l_contents = File.contents . file

remaining :: Lexer -> Text.Text
remaining l = Text.drop (location l) (l_contents l)

passed :: Lexer -> Text.Text
passed l = Text.take (location l) (l_contents l)

rev_passed :: Lexer -> Text.Text
rev_passed = Text.reverse . passed

new_lexer :: File.File -> Lexer
new_lexer f = Lexer f 0 1 1 [IndentationSensitive 0]

lexer_location :: Lexer -> Location.Location
lexer_location lexer = Location.Location (file lexer) (line lexer) (col lexer)

lexer_span :: Lexer -> Int -> Int -> Location.Span
lexer_span lexer start len =
    let start_lexer = lexer `seek` start
        before_end_lexer = start_lexer `seek` (len - 1)
        end_lexer = start_lexer `seek` len
    in Location.Span (lexer_location start_lexer) (lexer_location before_end_lexer) (lexer_location end_lexer)

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
seek lexer 0 = lexer
seek lexer n =
    let
        num_nl
            | n < 0 = Text.count "\n" $ Text.take (-n) (Text.drop (location lexer + n) (l_contents lexer))
            | otherwise = Text.count "\n" $ Text.take n (Text.drop (location lexer) (l_contents lexer))

        line'
            | n < 0 = line lexer - num_nl
            | otherwise = line lexer + num_nl

        col'
            | num_nl == 0 = col lexer + n
            | otherwise =
                1 + Text.length (Text.takeWhile (/='\n') $ Text.reverse $ Text.take (location lexer + n) (l_contents lexer))

    in lexer
       { location = location lexer + n
       , line = line'
       , col = col'
       }
-- tests {{{1
tests :: Test
tests = test
    [ "l_contents" ~:
        "abcdefghijkl" ~=? l_contents (Lexer (File.File "filename" "abcdefghijkl") 0 1 1 [])

    , "remaining" ~:
        "fghijkl" ~=? remaining (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "passed" ~:
        "abcde" ~=? passed (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "rev_passed" ~:
        "edcba" ~=? rev_passed (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "new_lexer" ~:
        let f = File.File "a" "abc"
        in Lexer f 0 1 1 [IndentationSensitive 0] ~=? new_lexer f

    , "lexer_location" ~:
        let f = File.File "a" "abc"
        in Location.Location f 2 4 ~=? lexer_location (Lexer f 0 2 4 [])

    , "lexer_span" ~:
        let f = File.File "a" "abcdef"
        in Location.Span (Location.Location f 1 1) (Location.Location f 1 2) (Location.Location f 1 3) ~=? lexer_span (new_lexer f) 0 2

    , "seek" ~:
        [ let l = new_lexer $ File.File "a" "abc"
          in l ~=? seek l 0

        , "forward" ~:
            [ let f = File.File "a" "abcd\nefgh"
              in Lexer f 2 1 3 [] ~=? seek (Lexer f 0 1 1 []) 2
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 3 1 4 [] ~=? seek (Lexer f 0 1 1 []) 3
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 4 1 5 [] ~=? seek (Lexer f 0 1 1 []) 4
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 5 2 1 [] ~=? seek (Lexer f 0 1 1 []) 5
            ]

        , "backward" ~:
            [ let f = File.File "a" "abcd\nefgh"
              in Lexer f 6 2 2 [] ~=? seek (Lexer f 8 2 4 []) (-2)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 5 2 1 [] ~=? seek (Lexer f 8 2 4 []) (-3)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 4 1 5 [] ~=? seek (Lexer f 8 2 4 []) (-4)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 3 1 4 [] ~=? seek (Lexer f 8 2 4 []) (-5)
            ]
        ]

    , "lex" ~:
        case UHF.Lexer.lex (File.File "a" "abc *&* ( \"adji") of
            ([UnclosedStrLit _], [Location.Located _ (Token.AlphaIdentifier "abc"), Location.Located _ (Token.SymbolIdentifier "*&*"), Location.Located _ Token.OParen]) -> return ()
            x -> assertFailure $ "lex lexed incorrectly: returned '" ++ show x ++ "'"

    , let lex_test fn input check = TestCase $ check $ fn $ new_lexer $ File.File "a" input
          lex_test_fail fn_name res = assertFailure $ "'" ++ fn_name ++ "' lexed incorrectly: returned '" ++ show res ++ "'"
      in "lexing" ~:
            [ "lex'" ~:
                [ lex_test (flip lex' Nothing) "abc" $ \case
                      (Nothing, [], [Location.Located _ (Token.AlphaIdentifier "abc")]) -> return ()
                      x -> lex_test_fail "lex'" x
                , lex_test (flip lex' Nothing) "" $ \case
                      (Nothing, [], []) -> return ()
                      x -> lex_test_fail "lex'" x
                ]

            , "lex_eof" ~:
                  [ lex_test lex_eof "" $ \case
                        Just (False, Nothing, [], []) -> return ()
                        x -> lex_test_fail "lex_eof" x
                  , lex_test lex_eof "a" $ \case
                        Nothing -> return ()
                        x -> lex_test_fail "lex_eof" x
                  , TestCase $
                        let f = File.File "a" ""
                            l = Lexer f 0 1 1 [IndentationInsensitive, IndentationSensitive 4, IndentationInsensitive, IndentationSensitive 0]
                        in case lex_eof l of
                            Just (False, Nothing, [], [Location.Located _ Token.Newline, Location.Located _ Token.Dedent]) -> return ()
                            x -> lex_test_fail "lex_eof" x
                  ]

            , "lex_comment" ~:
                  [ lex_test lex_comment "// asdf\nabcde\n" $ \case
                        Just (False, Just l', [], [])
                            | remaining l' == "abcde\n" -> return ()
                        x -> lex_test_fail "lex_comment" x
                  , lex_test lex_comment "// asdf" $ \case
                        Just (False, Just l', [], [])
                            | remaining l' == "" -> return ()
                        x -> lex_test_fail "lex_comment" x
                  , lex_test lex_comment "/* asdf\nasdf */more\n" $ \case
                        Just (False, Just l', [], [])
                            | remaining l' == "more\n" -> return ()
                        x -> lex_test_fail "lex_comment" x
                  , lex_test lex_comment "/* asdf /* asdf */ asdf */more\n" $ \case
                        Just (False, Just l', [], [])
                            | remaining l' == "more\n" -> return ()
                        x -> lex_test_fail "lex_comment" x
                  , lex_test lex_comment "/* asdf" $ \case
                        Just (False, Just l', [UnclosedMComment _], [])
                            | remaining l' == "" -> return ()
                        x -> lex_test_fail "lex_comment" x
                  , lex_test lex_comment "a" $ \case
                        Nothing -> return ()
                        x -> lex_test_fail "lex_comment" x
                  ]

            , "lex_alpha_identifier" ~:
                  [ lex_test lex_alpha_identifier "a" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.AlphaIdentifier "a")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  , lex_test lex_alpha_identifier "a12" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.AlphaIdentifier "a12")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  , lex_test lex_alpha_identifier "a''" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.AlphaIdentifier "a''")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  , lex_test lex_alpha_identifier "_a" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.AlphaIdentifier "_a")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  ]
            , "lex_symbol_identifier" ~:
                  [ lex_test lex_symbol_identifier "*" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.SymbolIdentifier "*")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  , lex_test lex_symbol_identifier "*^&*&" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.SymbolIdentifier "*^&*&")])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_alpha_identifier" x
                  ]

            , "lex_str_or_char_lit" ~:
                  [ "character literals" ~:
                        [ lex_test lex_str_or_char_lit "'c'" $ \case
                              Just (True, Just l, [], [Location.Located _ (Token.CharLit 'c')])
                                  | remaining l == "" -> return ()
                              x -> lex_test_fail "lex_str_or_char_lit" x
                        , lex_test lex_str_or_char_lit "'c" $ \case
                              Just (True, Just l, [UnclosedCharLit _], [])
                                  | remaining l == "" -> return ()
                              x -> lex_test_fail "lex_str_or_char_lit" x
                        , lex_test lex_str_or_char_lit "'cab'" $ \case
                              Just (True, Just l, [MulticharCharLit _], [])
                                  | remaining l == "" -> return ()
                              x -> lex_test_fail "lex_str_or_char_lit" x
                        ]
                  , "string literals" ~:
                        [ lex_test lex_str_or_char_lit "\"abcde\"" $ \case
                              Just (True, Just l, [], [Location.Located _ (Token.StringLit "abcde")])
                                  | remaining l == "" -> return ()
                              x -> lex_test_fail "lex_str_or_char_lit" x
                        , lex_test lex_str_or_char_lit "\"cjfwoeifj" $ \case
                              Just (True, Just l, [UnclosedStrLit _], [])
                                  | remaining l == "" -> return ()
                              x -> lex_test_fail "lex_str_or_char_lit" x
                        ]
                  ]

            , "lex_number" ~:
                  [ lex_test lex_number "1234" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Dec 1234)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "01234" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Dec 1234)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "1234.1234" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.FloatLit 1234.1234)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0b101" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Bin 5)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0xf1abcABC" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Hex 4054567612)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0o765" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Oct 501)])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number ".123" $ \case
                        Nothing -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "123." $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Dec 123)])
                            | remaining l == "." -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "123.x" $ \case
                        Just (True, Just l, [], [Location.Located _ (Token.IntLit Token.Dec 123)])
                            | remaining l == ".x" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0b101.1" $ \case
                        Just (True, Just l, [NonDecimalFloat _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0b29a" $ \case
                        Just (True, Just l, [InvalidIntDigit '2' _, InvalidIntDigit '9' _, InvalidIntDigit 'a' _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "20ab3" $ \case
                        Just (True, Just l, [InvalidIntDigit 'a' _, InvalidIntDigit 'b' _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0o79a" $ \case
                        Just (True, Just l, [InvalidIntDigit '9' _, InvalidIntDigit 'a' _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0b.1" $ \case
                        Just (True, Just l, [MissingDigits _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x

                  , lex_test lex_number "0a98a" $ \case
                        Just (True, Just l, [InvalidIntBase 'a' _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "lex_number" x
                  ]
            , "lex_space" ~:
                  [ lex_test lex_space " a" $ \case
                        Just (False, Just l, [], [])
                            | remaining l == "a" -> return ()
                        x -> lex_test_fail "lex_space" x
                  , lex_test lex_space "\na" $ \case
                        Just (False, Just l, [], [])
                            | remaining l == "a" -> return ()
                        x -> lex_test_fail "lex_space" x
                  , lex_test lex_space "\ta" $ \case
                        Just (False, Just l, [], [])
                            | remaining l == "a" -> return ()
                        x -> lex_test_fail "lex_space" x
                  , lex_test lex_space "\va" $ \case
                        Just (False, Just l, [], [])
                            | remaining l == "a" -> return ()
                        x -> lex_test_fail "lex_space" x
                  ]
            , "make_bad_char" ~:
                  [ lex_test make_bad_char "a" $ \case
                        Just (True, Just l, [BadChar 'a' _], [])
                            | remaining l == "" -> return ()
                        x -> lex_test_fail "make_bad_char" x
                  , lex_test make_bad_char "" $ \case
                        Nothing -> return ()
                        x -> lex_test_fail "make_bad_char" x
                  ]
            -- , "lex_indent" ~: []
            -- inserts indent when indentation increased, inserts new indentation frame
            -- inserts newline when indentation same
            -- inserts dedents when indentation is less, removes indentation frames
            -- inserts dedents when indentation is less, removes indentation frames multiple
            -- inserts { when {, adds indentation frame
            -- inserts } when }, removes indentation frame
            -- inserts ; when ;
            ]
    ]
