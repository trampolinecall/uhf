module UHF.Parser.Parser
    ( TokenStream

    , Parser
    , ParseResult(..)
    , return_fail
    , return_recoverable

    , is_tt
    , alpha_iden

    , peek
    , consume
    , advance

    , choice
    , star
    , plus
    , optional

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location

import qualified UHF.Token as Token

import qualified UHF.Util.InfList as InfList

import qualified Data.Data as Data

import qualified Control.Monad.Trans.State as State

type TokenStream = InfList.InfList Token.LNormalToken

-- TODO: also clean up this too
-- TODO: allow each thing to provide a custom error function

type Parser = State.StateT TokenStream ParseResult

data ParseResult r
    = Failed [ParseError.ParseError] ParseError.ParseError
    | Recoverable [ParseError.ParseError] r
    | Success r
    deriving (Show, Eq)

instance Functor ParseResult where
    fmap _ (Failed es e) = Failed es e
    fmap f (Recoverable es r) = Recoverable es (f r)
    fmap f (Success r) = Success (f r)

instance Applicative ParseResult where
    pure = Success

    a <*> b =
        case a of
            Failed a_es a_e -> Failed a_es a_e

            Recoverable a_es a_v ->
                case a_v <$> b of
                    Failed b_es b_e -> Failed (a_es ++ b_es) b_e
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> a_v <$> b

instance Monad ParseResult where
    a >>= b =
        case a of
            Failed a_es a_e -> Failed a_es a_e

            Recoverable a_es a_v ->
                case b a_v of
                    Failed b_es b_e -> Failed (a_es ++ b_es) b_e
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> b a_v

return_fail :: [ParseError.ParseError] -> ParseError.ParseError -> Parser a
return_fail errs err = State.StateT $ \ _ -> Failed errs err

return_recoverable :: [ParseError.ParseError] -> a -> Parser a
return_recoverable errs res = State.StateT $ \ toks -> Recoverable errs (res, toks)

is_tt :: Token.NormalToken -> Token.NormalToken -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

alpha_iden :: Token.NormalToken
alpha_iden = Token.AlphaIdentifier []

peek :: Parser Token.LNormalToken
peek = State.StateT $ \ toks -> Success (InfList.head toks, toks)

consume :: String -> Token.NormalToken -> Parser Token.LNormalToken
consume name exp = State.StateT $
    \ (tok InfList.::: more_toks) ->
        if is_tt (Location.unlocate tok) exp
            then Success (tok, more_toks)
            else Failed [] $ ParseError.BadToken tok exp name

advance :: Parser ()
advance = State.StateT $ \ toks -> Success ((), InfList.drop1 toks)

-- combinators

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = State.StateT $ \ toks ->
    try_choices choices [] toks
    where
        try_choices (c:cs) breaking_acc toks =
            case State.runStateT c toks of
                Success r -> return r
                Recoverable es r -> Recoverable es r
                Failed _ err -> try_choices cs (err:breaking_acc) toks
                -- ignore recoverable errors because they only apply if the choice could be decided
                -- e.g. a hypothetical recoverable error like "invalid function name" or something doesn't apply if it can't decide whether or not it is parsing a function or a datatype

        try_choices [] breaking_acc (tok InfList.::: _) = Failed [] (ParseError.NoneMatched tok breaking_acc)

star :: Parser a -> Parser [a]
star a = State.StateT $ \ toks ->
    star' [] [] toks
    where
        star' err_acc a_acc toks =
            case State.runStateT a toks of
                Success (r, toks') -> star' err_acc (a_acc ++ [r]) toks'
                Recoverable es (r, toks') -> star' (err_acc ++ es) (a_acc ++ [r]) toks'
                Failed _ _ ->
                -- both errors do not apply if it doesn't work because that just means the list ends there
                    if null err_acc
                        then Success (a_acc, toks)
                        else Recoverable err_acc (a_acc, toks)

plus :: Parser a -> Parser [a]
plus a =
    a >>= \ a_res ->
    star a >>= \ more_as ->
    return (a_res : more_as)

optional :: Parser a -> Parser (Maybe a)
optional a = State.StateT $ \ toks ->
    case State.runStateT a toks of
        Success (r, toks') -> Success (Just r, toks')
        Recoverable e (r, toks') -> Recoverable e (Just r, toks')
        Failed _ _ -> Success (Nothing, toks)

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

test_is_alpha_iden =
    [ testCase "is_alpha_iden alpha identifier" undefined
    , testCase "is_alpha_iden not alpha identifier" undefined
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
