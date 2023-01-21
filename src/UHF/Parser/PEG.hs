{-# LANGUAGE TupleSections #-}

module UHF.Parser.PEG
    ( TokenStream

    , Parser
    , ParseResult(..)
    , fail
    , recoverable

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

import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location

import qualified UHF.Token as Token

import qualified Data.InfList as InfList

import qualified Data.Data as Data

type TokenStream = InfList.InfList Token.LToken

-- TODO: allow each thing to provide a custom error function

type Parser = StateT TokenStream ParseResult
newtype ParseResult r = ParseResult ([ParseError.ParseError], Either ParseError.ParseError r) deriving (Eq, Show)

instance Functor ParseResult where
    fmap f (ParseResult (e, ei)) = ParseResult (e, f <$> ei)

instance Applicative ParseResult where
    pure = ParseResult . ([], ) . Right
    ParseResult (a_e, a_ei) <*> ParseResult (b_e, b_ei) = ParseResult (a_e ++ b_e, a_ei <*> b_ei)

instance Monad ParseResult where
    ParseResult (a_e, a_ei) >>= b =
        case a_ei of
            Right a ->
                let ParseResult (b_e, b_ei) = b a
                in ParseResult (a_e ++ b_e, b_ei)

            Left e -> ParseResult (a_e, Left e)

fail :: [ParseError.ParseError] -> ParseError.ParseError -> Parser a
fail errs err = StateT $ \ _ -> ParseResult (errs, Left err)

recoverable :: [ParseError.ParseError] -> a -> Parser a
recoverable errs res = StateT $ \ toks -> ParseResult (errs, Right (res, toks))

-- TODO: this does not work properly because this needs to compare the constructor of SingleTypeToken too
is_tt :: Token.TokenType -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

peek :: Parser Token.LToken
peek = StateT $ \ toks -> ParseResult ([], Right (InfList.head toks, toks))

consume :: Text -> Token.TokenType -> Parser Token.LToken
consume name exp = StateT $
    \ (tok InfList.::: more_toks) ->
        if is_tt exp (Location.unlocate tok)
            then ParseResult ([], Right (tok, more_toks))
            else ParseResult ([], Left $ ParseError.BadToken tok exp name)

advance :: Parser ()
advance = StateT $ \ toks -> ParseResult ([], Right ((), InfList.tail toks))

-- combinators

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = StateT $ \ toks -> try_choices choices [] toks
    where
        try_choices (c:cs) breaking_acc toks =
            case runStateT c toks of
                res@(ParseResult (_, Right _)) -> res
                ParseResult (_, Left e) -> try_choices cs (e:breaking_acc) toks

        try_choices [] breaking_acc (tok InfList.::: _) = ParseResult ([], Left (ParseError.NoneMatched tok breaking_acc))

star :: Parser a -> Parser [a]
star a = StateT $ \ toks ->
    star' [] [] toks
    where
        star' err_acc a_acc toks =
            case runStateT a toks of
                ParseResult (es, Right (r, toks')) -> star' (err_acc ++ es) (a_acc ++ [r]) toks'
                ParseResult (_, Left _) -> ParseResult (err_acc, Right (a_acc, toks))
                -- both errors do not apply if it doesn't work because that just means the list ends there

plus :: Parser a -> Parser [a]
plus a =
    a >>= \ a_res ->
    star a >>= \ more_as ->
    pure (a_res : more_as)

optional :: Parser a -> Parser (Maybe a)
optional a = StateT $ \ toks ->
    case runStateT a toks of
        ParseResult (es, Right (r, toks')) -> ParseResult (es, Right (Just r, toks'))
        ParseResult (_, Left _) -> ParseResult ([], Right (Nothing, toks))

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

-- tests {{{1
test_is_tt =
    [ testCase "is_tt same" undefined
    , testCase "is_tt different" undefined
    ]

dummy_eof = Location.dummy_locate (Token.EOF ())
add_eofs t = t InfList.+++ InfList.repeat dummy_eof

case_peek =
    let t = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in (ParseResult ([], Right t)) @=? evalStateT peek tokstream

test_consume =
    let t = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        tokstream = add_eofs [t]
    in
        [ testCase "consume with True" $
            let expect = Token.SingleTypeToken Token.OParen
            in (ParseResult ([], Right t)) @=? evalStateT (consume "'('" expect) tokstream
        , testCase "consume with False" $
            let expect = Token.SingleTypeToken Token.CParen
            in (ParseResult ([], Left $ ParseError.BadToken t expect "')'")) @=? evalStateT (consume "')'" expect) tokstream
        ]

case_advance =
    let t1 = Location.dummy_locate (Token.SingleTypeToken Token.OParen)
        t2 = Location.dummy_locate (Token.SingleTypeToken Token.CParen)
        tokstream = add_eofs [t1, t2]

    in case runStateT advance tokstream of
        ParseResult ([], Right ((), tokstream'))
            | tokstream' InfList.!!! 0 == t2 &&
              tokstream' InfList.!!! 1 == dummy_eof -> pure ()

        ParseResult (recoverable_errors, Right ((), tokstream')) ->
            assertFailure $ "did not advance correctly, got: " ++ show (InfList.take 5 tokstream') ++ " (only 5 first tokens shown)and recoverable errors " ++ show recoverable_errors

        ParseResult (recoverable_errors, Left errors) ->
            assertFailure $ "did not advance correctly, got result with Left: errors " ++ show errors ++ " and recoverable errors " ++ show recoverable_errors


test_choice =
    let oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)
        cparen_consume = consume "cparen" (Token.SingleTypeToken Token.CParen)

        parser = choice [oparen_consume, cparen_consume]

    in
        [ testCase "1" $
            let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
                toks = add_eofs [oparen]

            in ParseResult ([], Right oparen) @=? evalStateT parser toks

        , testCase "2" $
            let cparen = Location.dummy_locate $ Token.SingleTypeToken Token.CParen
                toks = add_eofs [cparen]

            in ParseResult ([], Right cparen) @=? evalStateT parser toks

        , testCase "not matched" $
            let obrace = Location.dummy_locate $ Token.SingleTypeToken Token.OBrace
                toks = add_eofs [obrace]

            in ParseResult ([], Left $ ParseError.NoneMatched obrace [ParseError.BadToken obrace (Token.SingleTypeToken Token.CParen) "cparen", ParseError.BadToken obrace (Token.SingleTypeToken Token.OParen) "oparen"]) @=? evalStateT parser toks
        ]

test_star =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        oparen_type = Token.SingleTypeToken Token.OParen

        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        parser = star oparen_consume

    in
        [ testCase "none" $
            let toks = add_eofs []
            in ParseResult ([], Right []) @=? evalStateT parser toks

        , testCase "once" $
            let toks = add_eofs [oparen]
            in ParseResult ([], Right [oparen]) @=? evalStateT parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen]
            in ParseResult ([], Right [oparen, oparen]) @=? evalStateT parser toks
        ]

test_plus =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        oparen_type = Token.SingleTypeToken Token.OParen

        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        parser = plus oparen_consume

    in
        [ testCase "none" $
            let toks = add_eofs []
            in ParseResult ([], Left $ ParseError.BadToken dummy_eof oparen_type "oparen") @=? evalStateT parser toks

        , testCase "once" $
            let toks = add_eofs [oparen]
            in ParseResult ([], Right [oparen]) @=? evalStateT parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen]
            in ParseResult ([], Right [oparen, oparen]) @=? evalStateT parser toks
        ]

test_optional =
    let oparen = Location.dummy_locate $ Token.SingleTypeToken Token.OParen
        oparen_type = Token.SingleTypeToken Token.OParen

        oparen_consume = consume "oparen" (Token.SingleTypeToken Token.OParen)

        parser = optional oparen_consume
    in
        [ testCase "none" $
            let toks = add_eofs []
            in ParseResult ([], Right Nothing) @=? evalStateT parser toks

        , testCase "once" $
            let toks = add_eofs [oparen]
            in ParseResult ([], Right $ Just oparen) @=? evalStateT parser toks

        , testCase "multiple" $
            let toks = add_eofs [oparen, oparen]
            in ParseResult ([], Right $ Just oparen) @=? evalStateT parser toks
        ]

tests :: TestTree
tests = $(testGroupGenerator)
