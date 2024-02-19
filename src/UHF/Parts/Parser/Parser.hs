{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module UHF.Parts.Parser.Parser
    ( TokenStream
    , Errors
    , Parser

    , run_parser

    , is_tt

    , fail

    , peek
    , consume
    , consume'
    , advance

    , star

    , tests
    ) where

import UHF.Prelude

import qualified Data.InfList as InfList
import qualified Data.List.NonEmpty as NonEmpty

-- import UHF.Source.EqIgnoringSpans (eqis, expected_assert_eqis)
import qualified UHF.Data.Token as Token
import qualified UHF.Source.Located as Located
import qualified UHF.Parts.Parser.Error as Error

type TokenStream = InfList.InfList Token.LToken
type Errors = NonEmpty Error.Error

newtype Parser r = Parser { extract_parser :: ExceptT Errors (State TokenStream) r } deriving (Functor, Applicative, Monad)

run_parser :: Parser a -> TokenStream -> (Either Errors a, TokenStream)
run_parser p = runState (runExceptT (extract_parser p))

is_tt :: Token.TokenType -> Token.Token -> Bool
is_tt ty tok = ty == Token.to_token_type tok

fail :: Errors -> Parser a
fail err = Parser $ ExceptT $ state $ \ toks -> (Left err, toks)

peek :: Parser Token.LToken
peek = Parser $ state $ \ toks -> (InfList.head toks, toks)

consume :: (Token.LToken -> Errors) -> Token.TokenType -> Parser Token.LToken
consume make_err expect = Parser $ ExceptT $ state $
    \ (tok InfList.::: more_toks) ->
        if is_tt expect (Located.unlocate tok)
            then (Right tok, more_toks)
            else (Left $ make_err tok, more_toks)

-- TODO: REMOVE this, also remove todo call
consume' :: Text -> Token.TokenType -> Parser Token.LToken
consume' name expect = consume (\ token -> NonEmpty.singleton $ Error.BadToken todo token expect name) expect

advance :: Parser Token.LToken
advance = Parser $ state $ \ toks -> (InfList.head toks, InfList.tail toks)

-- combinators {{{1

star :: (Token.Token -> Bool) -> Parser a -> Parser [a]
star = go []
    where
        go acc stop_pred parse_thing = do
            stop <- stop_pred . Located.unlocate <$> peek
            if stop
               then pure acc
               else do
                   thing <- parse_thing
                   go (thing:acc) stop_pred parse_thing

-- TODO: star_synchronize :: (Token.Token -> Bool) -> (Token.Token -> Bool) -> Parser a -> Parser [a]
-- TODO: star_delim :: ((Token.Token, Token.Token) -> Bool) -> Parser a -> Parser a -> Parser [a]

-- tests {{{1
-- TODO: tests

tests :: TestTree
tests = $(testGroupGenerator)
