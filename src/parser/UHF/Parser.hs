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

import qualified Data.Data as Data
import qualified Data.Maybe as Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Monad.State as State

import qualified Safe

type TokenStream = [Location.Located Token.Token]
type Errors = NonEmpty.NonEmpty ParseError.ParseError

type ParseFn r = State.State TokenStream (ParseResult r)
type LookaheadMatches = Token.Token -> Bool

-- ParseResult {{{1
data ParseResult r
    = Failed
    | FailedLost
    | Success r

instance Functor ParseResult where
    fmap _ Failed = Failed
    fmap f (Success a) = Success (f a)

instance Applicative ParseResult where
    pure = Success

    (Success f) <*> (Success b) = Success $ f b
    _ <*> _ = Failed

instance Monad ParseResult where
    Success a >>= f = f a
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
parse' = error "not implemented yet"

-- decls {{{2
-- decl {{{3
-- decl_lookahead_matches :: LookaheadMatches
-- decl_lookahead_matches = _

decl_parse :: ParseFn Decl.Decl
decl_parse = error "not implemented yet"
-- type signatures {{{3
-- type_signature_lookahead_matches :: LookaheadMatches
-- type_signature_lookahead_matches = _

type_signature_parse :: ParseFn Decl.Decl
type_signature_parse = error "not implemented yet"
    -- consume (is_tt_u Token.Type)
-- definition {{{3
-- definition_lookahead_matches :: LookaheadMatches
-- definition_lookahead_matches = _

definition_parse :: ParseFn Decl.Decl
definition_parse = error "not implemented yet"
-- helpers {{{1
peek :: State.State TokenStream (Maybe (Location.Located Token.Token))
peek = State.state $ \ toks -> (Safe.headMay toks, toks)

peek_matches :: (Maybe Token.Token -> Bool) -> State.State TokenStream Bool
peek_matches pred = pred <$> (Location.unlocate <$>) <$> peek

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

is_tt_v :: a -> Token.Token -> Token.Token -> Maybe a
is_tt_v v a b = if is_tt a b then Just v else Nothing

is_tt_u :: Token.Token -> Token.Token -> Maybe ()
is_tt_u = is_tt_v ()

consume :: (Token.Token -> Maybe r) -> {- (Maybe (Location.Located Token.Token) -> Errors) -> -} ParseFn r
consume pred =
    State.state $ \ toks ->
        case toks of
            tok:more_toks ->
                case pred $ Location.unlocate tok of
                    Just res -> (Success res, more_toks)
                    _ -> (Failed, toks)

            [] -> (Failed, [])
