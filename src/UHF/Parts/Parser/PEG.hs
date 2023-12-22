{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.Parser.PEG
    ( TokenStream

    , Parser
    , fail

    , eval_parser
    , run_parser

    , is_tt

    , peek
    , consume
    , consume'
    , advance

    , choice
    , star
    , delim_star
    , plus
    , optional

    , tests
    ) where

import UHF.Prelude

import qualified Data.InfList as InfList

import UHF.Source.EqIgnoringSpans (eqis, expected_assert_eqis)
import qualified UHF.Data.Token as Token
import qualified UHF.Source.Located as Located
import qualified UHF.Parts.Parser.Error as Error

type TokenStream = InfList.InfList (Int, Token.LToken)

newtype Parser r = Parser { extract_parser :: [Error.Error] -> TokenStream -> ([Error.Error], Maybe (r, TokenStream)) }

instance Functor Parser where
    fmap f parser = parser >>= \ res -> pure (f res)

instance Applicative Parser where
    pure a = Parser $ \ errors toks -> (errors, Just (a, toks))
    op <*> a =
        op >>= \ op' ->
        a >>= \ a' ->
        pure (op' a')

instance Monad Parser where
     (Parser a) >>= b = Parser $ \ errors toks ->
        let (errors', res) = a errors toks
        in case res of
            Just (r, toks') -> extract_parser (b r) errors' toks'
            Nothing -> (errors', Nothing)

run_parser :: Parser a -> TokenStream -> ([Error.Error], Maybe (a, TokenStream))
run_parser p = extract_parser p []

eval_parser :: Parser a -> TokenStream -> ([Error.Error], Maybe a)
eval_parser p toks =
    let (errors, res) = extract_parser p [] toks
    in case res of
        Just (r, _) -> (errors, Just r)
        _ -> (errors, Nothing)

-- having an error for every Nothing result is enforced by the fact that in the public interface the only way to create a Nothing result is through these functions
fail :: Error.Error -> Parser a
fail error = Parser $ \ errors _ -> (error : errors, Nothing)

is_tt :: Token.TokenType -> Token.Token -> Bool
is_tt ty tok = ty == Token.to_token_type tok

peek :: Parser Token.LToken
peek = Parser $ \ errors toks -> (errors, Just (snd $ InfList.head toks, toks))

consume :: (Int -> Token.LToken -> Error.Error) -> Token.TokenType -> Parser Token.LToken
consume make_err exp = Parser $
    \ errors ((tok_i, tok) InfList.::: more_toks) ->
        if is_tt exp (Located.unlocate tok)
            then (errors, Just (tok, more_toks))
            else (make_err tok_i tok : errors, Nothing)

consume' :: Text -> Token.TokenType -> Parser Token.LToken
consume' name exp = consume (\ tok_i tok -> Error.BadToken tok_i tok exp name) exp

advance :: Parser ()
advance = Parser $ \ bt toks -> (bt, Just ((), InfList.tail toks))

-- combinators {{{1

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = Parser $ \ errors toks -> try_choices errors choices toks
    where
        try_choices errors (choice:more_choices) toks =
            let (errors', res) = extract_parser choice errors toks
            in case res of
                Just r -> (errors', Just r)
                Nothing -> try_choices errors' more_choices toks

        try_choices errors [] _ = (errors, Nothing)

star :: Parser a -> Parser [a]
star a = star' a []

star' :: Parser a -> [a] -> Parser [a]
star' a acc = Parser $ \ errors toks ->
    star'' errors acc toks
    where
        star'' errors a_acc toks =
            let (errors', res) = extract_parser a errors toks
            in case res of
                (Just (r, toks')) -> star'' errors' (a_acc ++ [r]) toks'
                Nothing -> (errors', Just (a_acc, toks))

plus :: Parser a -> Parser [a]
plus a = Parser $ \ errors toks ->
    let (errors', res) = extract_parser a errors toks
    in case res of
        Just (r, toks') -> extract_parser (star' a [r]) errors' toks'
        Nothing -> (errors', Nothing)

delim_star :: Parser a -> Parser d -> Parser [a]
delim_star = delim_star' []
    where
        delim_star' acc thing delim =
            optional thing >>= \case
                Just thing_res ->
                    optional delim >>= \case
                        Just _ ->
                            delim_star' (acc ++ [thing_res]) thing delim -- this may or may not add more elements, allowing for a trailing delimiter

                        Nothing -> pure (acc ++ [thing_res]) -- no delimiter, cannot continue

                Nothing -> pure acc

optional :: Parser a -> Parser (Maybe a)
optional a = Parser $ \ errors toks ->
    case extract_parser a errors toks of
        (errors', Just (r, toks')) -> (errors', Just (Just r, toks'))
        (errors', Nothing) -> (errors', Just (Nothing, toks))

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

-- tests {{{1
dummy_eof :: IO Token.LToken
dummy_eof = Located.dummy_locate (Token.EOF ())
add_eofs :: [Token.LToken] -> IO TokenStream
add_eofs t = dummy_eof >>= \ dummy_eof -> pure (InfList.zip (InfList.iterate (+1) 0) (t InfList.+++ InfList.repeat dummy_eof))

case_peek :: Assertion
case_peek =
    Located.dummy_locate (Token.SingleTypeToken Token.OParen) >>= \ t ->
    add_eofs [t] >>= \ tokstream ->
    ([], Just t) `expected_assert_eqis` eval_parser peek tokstream

test_consume :: [TestTree]
test_consume =
    let t = Located.dummy_locate (Token.SingleTypeToken Token.OParen)
    in
        [ testCase "consume with True" $
            t >>= \ t -> add_eofs [t] >>= \ tokstream ->
            let expect = Token.SingleTypeToken Token.OParen
            in ([], Just t) `expected_assert_eqis` eval_parser (consume (\ tok_i tok -> Error.BadToken tok_i tok expect "')'") expect) tokstream
        , testCase "consume with False" $
            t >>= \ t -> add_eofs [t] >>= \ tokstream ->
            let expect = Token.SingleTypeToken Token.CParen
            in ([Error.BadToken 0 t expect "')'"], Nothing) `expected_assert_eqis` eval_parser (consume (\ tok_i tok -> Error.BadToken tok_i tok expect "')'") expect) tokstream
        ]

case_advance :: Assertion
case_advance =
    Located.dummy_locate (Token.SingleTypeToken Token.OParen) >>= \ t1 ->
    Located.dummy_locate (Token.SingleTypeToken Token.CParen) >>= \ t2 ->
    dummy_eof >>= \ dummy_eof ->

    add_eofs [t1, t2] >>= \ tokstream ->

    case run_parser advance tokstream of
        ([], Just ((), tokstream'))
            | tokstream' InfList.!!! 0 `eqis` (1, t2) &&
              tokstream' InfList.!!! 1 `eqis` (2, dummy_eof) -> pure ()

        (errors, Just (r, tokstream')) ->
            assertFailure $ "did not advance correctly, got: " ++ show (errors, Just (r, InfList.take 5 tokstream')) ++ " (only 5 first tokens shown)"

        res@(_, Nothing) ->
            assertFailure $ "did not advance correctly, got: " ++ show res

test_choice :: [TestTree]
test_choice =
    let oparen_consume = consume' "oparen" (Token.SingleTypeToken Token.OParen)
        cparen_consume = consume' "cparen" (Token.SingleTypeToken Token.CParen)

        oparen = Located.dummy_locate $ Token.SingleTypeToken Token.OParen
        cparen = Located.dummy_locate $ Token.SingleTypeToken Token.CParen

        obrace = Located.dummy_locate $ Token.SingleTypeToken Token.OBrace

        expect_oparen got_i got = Error.BadToken got_i got (Token.SingleTypeToken Token.OParen) "oparen"
        expect_cparen got_i got = Error.BadToken got_i got (Token.SingleTypeToken Token.CParen) "cparen"

        parser = choice [oparen_consume, cparen_consume]

    in
        [ testCase "(" $
            oparen >>= \ oparen ->
            add_eofs [oparen] >>= \ toks ->
            ([], Just oparen) `expected_assert_eqis` eval_parser parser toks

        , testCase ")" $
            cparen >>= \ cparen ->
            add_eofs [cparen] >>= \ toks ->
            ([expect_oparen 0 cparen], Just cparen) `expected_assert_eqis` eval_parser parser toks

        , testCase "not matched" $
            obrace >>= \ obrace ->
            add_eofs [obrace] >>= \ toks ->
            ([expect_cparen 0 obrace, expect_oparen 0 obrace], Nothing) `expected_assert_eqis` eval_parser parser toks
        ]

test_star :: [TestTree]
test_star =
    let oparen = Located.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Located.dummy_locate $ Token.SingleTypeToken Token.OBrace

        oparen_consume = consume' "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = star oparen_consume

    in
        [ testCase "none" $
            other >>= \ other ->
            add_eofs [other] >>= \ toks ->
            ([expect_oparen 0 other], Just []) `expected_assert_eqis` eval_parser parser toks

        , testCase "once" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, other] >>= \ toks ->
            ([expect_oparen 1 other], Just [oparen]) `expected_assert_eqis` eval_parser parser toks

        , testCase "multiple" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, oparen, other] >>= \ toks ->
            ([expect_oparen 2 other], Just [oparen, oparen]) `expected_assert_eqis` eval_parser parser toks
        ]

test_delim_star :: [TestTree]
test_delim_star =
    let oparen = Located.dummy_locate $ Token.SingleTypeToken Token.OParen
        delim = Located.dummy_locate $ Token.SingleTypeToken Token.Comma
        other = Located.dummy_locate $ Token.SingleTypeToken Token.OBrace

        oparen_consume = consume' "oparen" (Token.SingleTypeToken Token.OParen)
        delim_consume = consume' "delim" (Token.SingleTypeToken Token.Comma)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"
        expect_delim got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.Comma) "delim"

        parser = delim_star oparen_consume delim_consume

    in
        [ testCase "none" $
            other >>= \ other ->
            add_eofs [other] >>= \ toks ->
            ([expect_oparen 0 other], Just []) `expected_assert_eqis` eval_parser parser toks

        , testCase "once" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, other] >>= \ toks ->
            ([expect_delim 1 other], Just [oparen]) `expected_assert_eqis` eval_parser parser toks

        , testCase "once trailing" $
            oparen >>= \ oparen ->
            delim >>= \ delim ->
            other >>= \ other ->
            add_eofs [oparen, delim, other] >>= \ toks ->
            ([expect_oparen 2 other], Just [oparen]) `expected_assert_eqis` eval_parser parser toks

        , testCase "multiple" $
            oparen >>= \ oparen ->
            delim >>= \ delim ->
            other >>= \ other ->
            add_eofs [oparen, delim, oparen, other] >>= \ toks ->
            ([expect_delim 3 other], Just [oparen, oparen]) `expected_assert_eqis` eval_parser parser toks

        , testCase "multiple trailing" $
            oparen >>= \ oparen ->
            delim >>= \ delim ->
            other >>= \ other ->
            add_eofs [oparen, delim, oparen, delim, other] >>= \ toks ->
            ([expect_oparen 4 other], Just [oparen, oparen]) `expected_assert_eqis` eval_parser parser toks
        ]

test_plus :: [TestTree]
test_plus =
    let oparen = Located.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Located.dummy_locate $ Token.SingleTypeToken Token.OBrace

        oparen_consume = consume' "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = plus oparen_consume

    in
        [ testCase "none" $
            other >>= \ other ->
            add_eofs [other] >>= \ toks ->
            ([expect_oparen 0 other], Nothing) `expected_assert_eqis` eval_parser parser toks

        , testCase "once" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, other] >>= \ toks ->
            ([expect_oparen 1 other], Just [oparen]) `expected_assert_eqis` eval_parser parser toks

        , testCase "multiple" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, oparen, other] >>= \ toks ->
            ([expect_oparen 2 other], Just [oparen, oparen]) `expected_assert_eqis` eval_parser parser toks
        ]

test_optional :: [TestTree]
test_optional =
    let oparen = Located.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Located.dummy_locate $ Token.SingleTypeToken Token.OBrace
        oparen_consume = consume' "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = optional oparen_consume
    in
        [ testCase "none" $
            other >>= \ other ->
            add_eofs [other] >>= \ toks ->
            ([expect_oparen 0 other], Just Nothing) `expected_assert_eqis` eval_parser parser toks

        , testCase "once" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, other] >>= \ toks ->
            ([], Just $ Just oparen) `expected_assert_eqis` eval_parser parser toks

        , testCase "multiple" $
            oparen >>= \ oparen ->
            other >>= \ other ->
            add_eofs [oparen, oparen, other] >>= \ toks ->
            ([], Just $ Just oparen) `expected_assert_eqis` eval_parser parser toks
        ]

tests :: TestTree
tests = $(testGroupGenerator)
