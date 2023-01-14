{-# LANGUAGE TupleSections #-}

module UHF.Parser.PEG
    ( TokenStream

    , Parser
    , ParseResult(..)
    , return_fail
    , return_recoverable

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

type TokenStream = InfList.InfList Token.LNormalToken

-- TODO: also clean up this too
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

return_fail :: [ParseError.ParseError] -> ParseError.ParseError -> Parser a
return_fail errs err = StateT $ \ _ -> ParseResult (errs, Left err)

return_recoverable :: [ParseError.ParseError] -> a -> Parser a
return_recoverable errs res = StateT $ \ toks -> ParseResult (errs, Right (res, toks))

is_tt :: Token.TokenType -> Token.NormalToken -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

peek :: Parser Token.LNormalToken
peek = StateT $ \ toks -> ParseResult ([], Right (InfList.head toks, toks))

consume :: Text -> Token.TokenType -> Parser Token.LNormalToken
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
-- TODO: there is probably a way to make this less repetitive

test_parse_result_fmap :: [TestTree]
test_parse_result_fmap =
    [ testCase "on failed" undefined
    , testCase "on recoverable" undefined
    , testCase "on success" undefined
    ]

test_parse_result_applicative :: [TestTree]
test_parse_result_applicative =
    [ testCase "failed x undefined" undefined -- should not crash because Failed first argument short circuits out second argument
    , testCase "recoverable x failed" undefined
    , testCase "recoverable x recoverable" undefined
    , testCase "recoverable x success" undefined
    , testCase "success x failed" undefined
    , testCase "success x recoverable" undefined
    , testCase "success x success" undefined
    ]

test_parse_result_monad :: [TestTree]
test_parse_result_monad =
    [ testCase "failed x undefined" undefined -- same as above
    , testCase "recoverable x failed" undefined
    , testCase "recoverable x recoverable" undefined
    , testCase "recoverable x success" undefined
    , testCase "success x failed" undefined
    , testCase "success x recoverable" undefined
    , testCase "success x success" undefined
    ]

test_is_tt =
    [ testCase "is_tt same" undefined
    , testCase "is_tt different" undefined
    ]

case_peek = undefined

test_consume =
    [ testCase "consume with True" undefined
    , testCase "consume with False" undefined
    ]

case_advance = undefined

test_choice =
    [ testCase "1" undefined
    , testCase "2" undefined
    , testCase "not matched" undefined
    ]

test_star =
    [ testCase "none" undefined
    , testCase "once" undefined
    , testCase "multiple" undefined
    ]

test_plus =
    [ testCase "none" undefined
    , testCase "once" undefined
    , testCase "multiple" undefined
    ]

test_optional =
    [ testCase "none" undefined
    , testCase "once" undefined
    , testCase "multiple" undefined
    ]

tests :: TestTree
tests = $(testGroupGenerator)
