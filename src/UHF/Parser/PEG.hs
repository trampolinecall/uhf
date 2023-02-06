{-# LANGUAGE TupleSections #-}

module UHF.Parser.PEG
    ( TokenStream

    , Parser
    , fail
    , recoverable

    , eval_parser
    , run_parser

    , is_tt

    , peek
    , consume
    , advance

    , choice
    , star
    , plus
    , optional

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.Parser.Error as Error

import qualified UHF.IO.Location as Location

import qualified UHF.Token as Token

import qualified Data.InfList as InfList

type TokenStream = InfList.InfList (Int, Token.LToken)

-- TODO: allow each thing to provide a custom error function

newtype Parser r = Parser { extract_parser :: ([Error.OtherError] -> [Error.BacktrackingError] -> TokenStream -> ([Error.OtherError], [Error.BacktrackingError], Maybe (Maybe r, TokenStream))) }

instance Functor Parser where
    fmap f parser = parser >>= \ res -> pure (f res)

instance Applicative Parser where
    pure a = Parser $ \ other_errors bt_errors toks -> (other_errors, bt_errors, Just (Just a, toks))
    op <*> a =
        op >>= \ op' ->
        a >>= \ a' ->
        pure (op' a')

instance Monad Parser where
     (Parser a) >>= b = Parser $ \ other_errors bt_errors toks ->
        let (other_errors', bt_errors', res) = a other_errors bt_errors toks
        in case res of
            Just (Just r, toks') -> extract_parser (b r) other_errors' bt_errors' toks'
            Just (Nothing, toks') -> (other_errors', bt_errors', Just (Nothing, toks'))
            Nothing -> (other_errors', bt_errors', Nothing)

run_parser :: Parser a -> TokenStream -> ([Error.OtherError], [Error.BacktrackingError], Maybe (Maybe a, TokenStream))
run_parser p = extract_parser p [] []

eval_parser :: Parser a -> TokenStream -> ([Error.OtherError], [Error.BacktrackingError], Maybe a)
eval_parser p toks =
    let (other_errors, bt_errors, res) = extract_parser p [] [] toks
    in case res of
        Just (Just r, _) -> (other_errors, bt_errors, Just r)
        _ -> (other_errors, bt_errors, Nothing)

-- having an error for every Nothing result is enforced by the fact that in the public interface the only way to create a Nothing result is through these functions
fail :: [Error.OtherError] -> Error.BacktrackingError -> Parser a
fail add_other_errors bt_error = Parser $ \ other_errors bt_errors _ -> (other_errors ++ add_other_errors, bt_error : bt_errors, Nothing)

-- TODO: come up with a better name for this
recoverable :: [Error.OtherError] -> Maybe a -> Parser a
recoverable add_other_errors res = Parser $ \ other_errors bt_errors toks -> (other_errors ++ add_other_errors, bt_errors, Just (res, toks))

is_tt :: Token.TokenType -> Token.Token -> Bool
is_tt ty tok = ty == Token.to_token_type tok

peek :: Parser Token.LToken
peek = Parser $ \ other_errors bt_errors toks -> (other_errors, bt_errors, Just (Just $ snd $ InfList.head toks, toks))

consume :: Text -> Token.TokenType -> Parser Token.LToken
consume name exp = Parser $
    \ other_errors bt_errors ((tok_i, tok) InfList.::: more_toks) ->
        if is_tt exp (Location.unlocate tok)
            then (other_errors, bt_errors, Just (Just tok, more_toks))
            else
                let err = Error.BadToken tok_i tok exp name
                in (other_errors, err : bt_errors, Nothing)

advance :: Parser ()
advance = Parser $ \ o bt toks -> (o, bt, Just (Just (), InfList.tail toks))

-- combinators

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = Parser $ \ other_errors bt_errors toks -> try_choices other_errors bt_errors choices toks
    where
        try_choices other_errors bt_errors (choice:more_choices) toks =
            let (other_errors', bt_errors', res) = extract_parser choice other_errors bt_errors toks
            in case res of
                Just r -> (other_errors', bt_errors', Just r)
                Nothing -> try_choices other_errors' bt_errors' more_choices toks

        try_choices other_errors bt_errors [] _ = (other_errors, bt_errors, Nothing)

star :: Parser a -> Parser [Maybe a]
star a = star' a []

star' :: Parser a -> [Maybe a] -> Parser [Maybe a]
star' a acc = Parser $ \ other_errors bt_errors toks ->
    star'' other_errors bt_errors acc toks
    where
        star'' other_errors bt_errors a_acc toks =
            let (other_errors', bt_errors', res) = extract_parser a other_errors bt_errors toks
            in case res of
                (Just (r, toks')) -> star'' other_errors' bt_errors' (r:a_acc) toks'
                Nothing -> (other_errors', bt_errors', Just (Just a_acc, toks))

plus :: Parser a -> Parser [Maybe a]
plus a = Parser $ \ other_errors bt_errors toks ->
    let (other_errors', bt_errors', res) = extract_parser a other_errors bt_errors toks
    in case res of
        Just (r, toks') -> extract_parser (star' a [r]) other_errors' bt_errors' toks'
        Nothing -> (other_errors', bt_errors', Nothing)

optional :: Parser a -> Parser (Maybe a)
optional a = Parser $ \ other_errors bt_errors toks ->
    case extract_parser a other_errors bt_errors toks of
        (other_errors', bt_errors', Just (r, toks')) -> (other_errors', bt_errors', Just (Just r, toks'))
        (other_errors', bt_errors', Nothing) -> (other_errors', bt_errors', Just (Just Nothing, toks))

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

-- tests {{{1
test_is_tt :: [TestTree]
test_is_tt =
    [ testCase "is_tt same" undefined
    , testCase "is_tt different" undefined
    ]

-- TODO: test recoverable and other_errors

dummy_eof :: Token.LToken
dummy_eof = Location.dummy_locate (Token.EOF ())
add_eofs :: [Token.LToken] -> TokenStream
add_eofs t = InfList.zip (InfList.iterate (+1) 0) (t InfList.+++ InfList.repeat dummy_eof)

case_peek :: Assertion
case_peek =
    let t = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in ([], [], Just t) @=? eval_parser peek tokstream

test_consume :: [TestTree]
test_consume =
    let t = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in
        [ testCase "consume with True" $
            let expect = Token.SingleTypeToken Token.OParen
            in ([], [], Just t) @=? eval_parser (consume "'('" expect) tokstream
        , testCase "consume with False" $
            let expect = Token.SingleTypeToken Token.CParen
            in ([], [Error.BadToken 0 t expect "')'"], Nothing) @=? eval_parser (consume "')'" expect) tokstream
        ]

case_advance :: Assertion
case_advance =
    let t1 = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        t2 = Location.dummy_locate (Token.SingleTypeToken Token.CParen)
        tokstream = add_eofs [t1, t2]

    in case run_parser advance tokstream of
        ([], [], Just (Just (), tokstream'))
            | tokstream' InfList.!!! 0 == (1, t2) &&
              tokstream' InfList.!!! 1 == (2, dummy_eof) -> pure ()

        (other_errors, bt_errors, Just (r, tokstream')) ->
            assertFailure $ "did not advance correctly, got: " ++ show (other_errors, bt_errors, Just (r, InfList.take 5 tokstream')) ++ " (only 5 first tokens shown)"

        res@(_, _, Nothing) ->
            assertFailure $ "did not advance correctly, got: " ++ show res

test_choice :: [TestTree]
test_choice =
    let oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)
        cparen_consume = consume "cparen" (Token.SingleTypeToken Token.CParen)

        oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        cparen = Location.dummy_locate $ Token.SingleTypeToken Token.CParen

        obrace = Location.dummy_locate $ Token.SingleTypeToken Token.OBrace

        expect_oparen got_i got = Error.BadToken got_i got (Token.SingleTypeToken Token.OParen) "oparen"
        expect_cparen got_i got = Error.BadToken got_i got (Token.SingleTypeToken Token.CParen) "cparen"

        parser = choice [oparen_consume, cparen_consume]

    in
        [ testCase "(" $
            let toks = add_eofs [oparen]

            in ([], [], Just oparen) @=? eval_parser parser toks

        , testCase ")" $
            let toks = add_eofs [cparen]

            in ([], [expect_oparen 0 cparen], Just cparen) @=? eval_parser parser toks

        , testCase "not matched" $
            let toks = add_eofs [obrace]

            in ([], [expect_cparen 0 obrace, expect_oparen 0 obrace], Nothing) @=? eval_parser parser toks
        ]

test_star :: [TestTree]
test_star =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Location.dummy_locate $ Token.SingleTypeToken Token.OBrace

        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = star oparen_consume

    in
        [ testCase "none" $
            let toks = add_eofs [other]
            in ([], [expect_oparen 0 other], Just []) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([], [expect_oparen 1 other], Just [Just oparen]) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([], [expect_oparen 2 other], Just [Just oparen, Just oparen]) @=? eval_parser parser toks
        ]

test_plus :: [TestTree]
test_plus =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Location.dummy_locate $ Token.SingleTypeToken Token.OBrace

        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = plus oparen_consume

    in
        [ testCase "none" $
            let toks = add_eofs [other]
            in ([], [expect_oparen 0 other], Nothing) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([], [expect_oparen 1 other], Just [Just oparen]) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([], [expect_oparen 2 other], Just [Just oparen, Just oparen]) @=? eval_parser parser toks
        ]

test_optional :: [TestTree]
test_optional =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        other = Location.dummy_locate $ Token.SingleTypeToken Token.OBrace
        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        expect_oparen got_ind got = Error.BadToken got_ind got (Token.SingleTypeToken Token.OParen) "oparen"

        parser = optional oparen_consume
    in
        [ testCase "none" $
            let toks = add_eofs [other]
            in ([], [expect_oparen 0 other], Just $ Nothing) @=? eval_parser parser toks

        , testCase "once" $
            let toks = add_eofs [oparen, other]
            in ([], [], Just $ Just oparen) @=? eval_parser parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen, other]
            in ([], [], Just $ Just oparen) @=? eval_parser parser toks
        ]

tests :: TestTree
tests = $(testGroupGenerator)
