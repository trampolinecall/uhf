{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module UHF.Parser
    ( parse

    , ParseError.ParseError
    ) where

import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.Token as Token
import qualified UHF.IO.Location as Location

import qualified UHF.AST.Decl as Decl

import qualified Data.Maybe as Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Monad.State as State

type TokenStream = [Location.Located Token.Token]
type Errors = NonEmpty.NonEmpty ParseError.ParseError

type ParseFn r = State.State TokenStream (ParseResult r)
type LookaheadMatches = Token.Token -> Bool

-- ParseResult {{{1
data ParseResult r
    = Failed
    | Success r

instance Functor ParseResult where
    fmap _ Failed = Failed
    fmap f (Success a) = Success (f a)

instance Applicative ParseResult where
    pure = Success

    (Success f) <*> (Success b) = Success $ f b
    _ <*> _ = Failed

instance Monad ParseResult where
    (Success a) >>= f = f a
    Failed >>= _ = Failed

-- parse {{{1
parse :: TokenStream -> ([ParseError.ParseError], [Decl.Decl])
parse toks =
    let r_res = State.evalState parse' toks
    in case r_res of
        Success res -> ([], res)
        Failed -> ([], []) -- TODO: failed should return errors
        -- Recoverable errs res -> (NonEmpty.toList errs, res)

parse' :: ParseFn [Decl.Decl]
parse' = _

-- decls {{{2
-- decl {{{3
decl_lookahead_matches :: LookaheadMatches
decl_lookahead_matches = _
decl_parse :: ParseFn Decl.Decl
decl_parse = _
-- type signatures {{{3

-- helpers {{{1
consume :: (Token.Token -> Maybe r) -> {- (Maybe (Location.Located Token.Token) -> Errors) -> -} ParseFn r
consume pred =
    State.state $ \case
        orig_toks@(tok:more_toks) ->
            case pred $ Location.unlocate tok of
                Just res -> (Success res, more_toks)
                _ -> (Failed, orig_toks)

        [] -> (Failed, [])

{-
parse_list :: ParseConcept r -> ParseConcept [r]
parse_list = _
-}
