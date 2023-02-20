module UHF.Parser.PEG
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

import UHF.Util.Prelude

import qualified UHF.Parser.Error as Error

import qualified UHF.IO.Located as Located

import qualified UHF.Token as Token

import qualified Data.InfList as InfList

type TokenStream = InfList.InfList (Int, Token.LToken)

newtype Parser r = Parser { extract_parser :: [Error.BacktrackingError] -> TokenStream -> ([Error.BacktrackingError], Maybe (r, TokenStream)) }

instance Functor Parser where
    fmap f parser = parser >>= \ res -> pure (f res)

instance Applicative Parser where
    pure a = Parser $ \ bt_errors toks -> (bt_errors, Just (a, toks))
    op <*> a =
        op >>= \ op' ->
        a >>= \ a' ->
        pure (op' a')

instance Monad Parser where
     (Parser a) >>= b = Parser $ \ bt_errors toks ->
        let (bt_errors', res) = a bt_errors toks
        in case res of
            Just (r, toks') -> extract_parser (b r) bt_errors' toks'
            Nothing -> (bt_errors', Nothing)

run_parser :: Parser a -> TokenStream -> ([Error.BacktrackingError], Maybe (a, TokenStream))
run_parser p = extract_parser p []

eval_parser :: Parser a -> TokenStream -> ([Error.BacktrackingError], Maybe a)
eval_parser p toks =
    let (bt_errors, res) = extract_parser p [] toks
    in case res of
        Just (r, _) -> (bt_errors, Just r)
        _ -> (bt_errors, Nothing)

-- having an error for every Nothing result is enforced by the fact that in the public interface the only way to create a Nothing result is through these functions
fail :: Error.BacktrackingError -> Parser a
fail bt_error = Parser $ \ bt_errors _ -> (bt_error : bt_errors, Nothing)

is_tt :: Token.TokenType -> Token.Token -> Bool
is_tt ty tok = ty == Token.to_token_type tok

peek :: Parser Token.LToken
peek = Parser $ \ bt_errors toks -> (bt_errors, Just (snd $ InfList.head toks, toks))

consume :: (Int -> Token.LToken -> Error.BacktrackingError) -> Token.TokenType -> Parser Token.LToken
consume make_err exp = Parser $
    \ bt_errors ((tok_i, tok) InfList.::: more_toks) ->
        if is_tt exp (Located.unlocate tok)
            then (bt_errors, Just (tok, more_toks))
            else (make_err tok_i tok : bt_errors, Nothing)

consume' :: Text -> Token.TokenType -> Parser Token.LToken
consume' name exp = consume (\ tok_i tok -> Error.BadToken tok_i tok exp name) exp

advance :: Parser ()
advance = Parser $ \ bt toks -> (bt, Just ((), InfList.tail toks))

-- combinators {{{1

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = Parser $ \ bt_errors toks -> try_choices bt_errors choices toks
    where
        try_choices bt_errors (choice:more_choices) toks =
            let (bt_errors', res) = extract_parser choice bt_errors toks
            in case res of
                Just r -> (bt_errors', Just r)
                Nothing -> try_choices bt_errors' more_choices toks

        try_choices bt_errors [] _ = (bt_errors, Nothing)

star :: Parser a -> Parser [a]
star a = star' a []

star' :: Parser a -> [a] -> Parser [a]
star' a acc = Parser $ \ bt_errors toks ->
    star'' bt_errors acc toks
    where
        star'' bt_errors a_acc toks =
            let (bt_errors', res) = extract_parser a bt_errors toks
            in case res of
                (Just (r, toks')) -> star'' bt_errors' (a_acc ++ [r]) toks'
                Nothing -> (bt_errors', Just (a_acc, toks))

plus :: Parser a -> Parser [a]
plus a = Parser $ \ bt_errors toks ->
    let (bt_errors', res) = extract_parser a bt_errors toks
    in case res of
        Just (r, toks') -> extract_parser (star' a [r]) bt_errors' toks'
        Nothing -> (bt_errors', Nothing)

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
optional a = Parser $ \ bt_errors toks ->
    case extract_parser a bt_errors toks of
        (bt_errors', Just (r, toks')) -> (bt_errors', Just (Just r, toks'))
        (bt_errors', Nothing) -> (bt_errors', Just (Nothing, toks))

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

-- tests {{{1
dummy_eof :: Token.LToken
dummy_eof = Located.dummy_locate (Token.EOF ())
add_eofs :: [Token.LToken] -> TokenStream
add_eofs t = InfList.zip (InfList.iterate (+1) 0) (t InfList.+++ InfList.repeat dummy_eof)

case_peek :: Assertion
case_peek =
    let t = Located.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in ([], Just t) @=? eval_parser peek tokstream

test_consume :: [TestTree]
test_consume =
    let t = Located.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in
        [ testCase "consume with True" $
            let expect = Token.SingleTypeToken Token.OParen
            in ([], Just t) @=? eval_parser (consume (\ tok_i tok -> Error.BadToken tok_i tok expect "')'") expect) tokstream
        , testCase "consume with False" $
            let expect = Token.SingleTypeToken Token.CParen
            in ([Error.BadToken 0 t expect "')'"], Nothing) @=? eval_parser (consume (\ tok_i tok -> Error.BadToken tok_i tok expect "')'") expect) tokstream
        ]

case_advance :: Assertion
case_advance =
    let t1 = Located.dummy_locate (Token.SingleTypeToken Token.OParen)
        t2 = Located.dummy_locate (Token.SingleTypeToken Token.CParen)
        tokstream = add_eofs [t1, t2]

    in case run_parser advance tokstream of
        ([], Just ((), tokstream'))
            | tokstream' InfList.!!! 0 == (1, t2) &&
              tokstream' InfList.!!! 1 == (2, dummy_eof) -> pure ()

        (bt_errors, Just (r, tokstream')) ->
            assertFailure $ "did not advance correctly, got: " ++ show (bt_errors, Just (r, InfList.take 5 tokstream')) ++ " (only 5 first tokens shown)"

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
            let toks = add_eofs [oparen]

            in ([], Just oparen) @=? eval_parser parser toks

        , testCase ")" $
            let toks = add_eofs [cparen]

            in ([expect_oparen 0 cparen], Just cparen) @=? eval_parser parser toks

        , testCase "not matched" $
            let toks = add_eofs [obrace]

            in ([expect_cparen 0 obrace, expect_oparen 0 obrace], Nothing) @=? eval_parser parser toks
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
            let toks = add_eofs [other]
            in ([expect_oparen 0 other], Just []) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([expect_oparen 1 other], Just [oparen]) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([expect_oparen 2 other], Just [oparen, oparen]) @=? eval_parser parser toks
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
            let toks = add_eofs [other]
            in ([expect_oparen 0 other], Just []) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([expect_delim 1 other], Just [oparen]) @=? eval_parser parser toks

        , testCase "once trailing" $
            let toks = add_eofs [oparen, delim, other]
            in ([expect_oparen 2 other], Just [oparen]) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, delim, oparen, other]
            in ([expect_delim 3 other], Just [oparen, oparen]) @=? eval_parser parser toks

        , testCase "multiple trailing" $
            let toks = add_eofs [oparen, delim, oparen, delim, other]
            in ([expect_oparen 4 other], Just [oparen, oparen]) @=? eval_parser parser toks
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
            let toks = add_eofs [other]
            in ([expect_oparen 0 other], Nothing) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([expect_oparen 1 other], Just [oparen]) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([expect_oparen 2 other], Just [oparen, oparen]) @=? eval_parser parser toks
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
            let toks = add_eofs [other]
            in ([expect_oparen 0 other], Just Nothing) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([], Just $ Just oparen) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([], Just $ Just oparen) @=? eval_parser parser toks
        ]

tests :: TestTree
tests = $(testGroupGenerator)
