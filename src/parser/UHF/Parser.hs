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
import qualified UHF.AST.Type as Type
import qualified UHF.AST.Expr as Expr

import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Monad.State as State

import qualified Safe

type TokenStream = [Token.LToken]
type Errors = NonEmpty.NonEmpty ParseError.ParseError

type ParseFn r = State.State TokenStream r
type Parser r = ParseFn (ParseResult Errors r)
type TokenPredicate = Token.Token -> Bool
type TokenPredicateM a = Token.Token -> Maybe a
type MTokenPredicate = Maybe Token.Token -> Bool

-- ParseResult {{{1
data ParseResult e r
    = Failed e
    | Success r

instance Functor (ParseResult e) where
    fmap _ (Failed e) = (Failed e)
    fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (ParseResult e) where
    pure = Success

    (Success f) <*> (Success b) = Success $ f b
    (Failed e) <*> (Success _) = Failed e
    (Success _) <*> (Failed e) = Failed e
    (Failed e1) <*> (Failed e2) = Failed (e1 <> e2)

instance Semigroup e => Monad (ParseResult e) where
    Success a >>= f = f a
    Failed e >>= _ = Failed e

-- parse {{{1
parse :: TokenStream -> ([ParseError.ParseError], [Decl.Decl])
parse toks =
    let r_res = State.evalState parse' toks
    in case r_res of
        Success res -> ([], res)
        Failed e -> (NonEmpty.toList e, []) -- TODO: failed should return errors
        -- Recoverable errs res -> (NonEmpty.toList errs, res)

parse' :: Parser [Decl.Decl]
parse' = decl_parse >>= \case -- TODO
    Failed e -> return $ Failed e
    Success r -> return $ Success [r]
-- decls {{{2
decl_lookahead_matches :: TokenPredicate
decl_lookahead_matches = is_tt (Token.AlphaIdentifier [])

decl_parse :: Parser Decl.Decl
decl_parse =
    consume (\case
        Token.AlphaIdentifier name -> Just name
        _ -> Nothing)
        (\ t -> NonEmpty.singleton $ ParseError.BadTokenWithName t [(Token.AlphaIdentifier [], "name", "declaration")]) >>=
    \case
        Success decl_name ->
            choice
                [ (maybe False (is_tt Token.Equal), advance >> binding_parse decl_name)
                , (maybe False (is_tt Token.Colon), advance >> type_signature_parse decl_name)
                ]
                (peek >>= \ tok ->
                return (Failed $ NonEmpty.singleton $ ParseError.BadToken tok [(Token.Colon, "type signature"), (Token.Equal, "declaration")]))
        Failed e -> return $ Failed e

binding_parse :: [String] -> Parser Decl.Decl
binding_parse decl_name =
    assert_consume (is_tt_u Token.Equal) >>
    expr_parse >>= \case
        Success ex -> return $ Success $ Decl.Binding decl_name ex
        Failed e -> return $ Failed e

type_signature_parse :: [String] -> Parser Decl.Decl
type_signature_parse decl_name =
    assert_consume (is_tt_u Token.Colon) >>
    type_parse >>= \case
        Success ty -> return $ Success $ Decl.TypeSignature decl_name ty
        Failed e -> return $ Failed e
-- types {{{2
type_parse :: Parser Type.Type
type_parse = return $ Failed $ NonEmpty.singleton $ ParseError.NotImpl "types" -- TODO
-- exprs {{{2
expr_parse :: Parser Expr.Expr
expr_parse = return $ Failed $ NonEmpty.singleton $ ParseError.NotImpl "expressions" -- TODO
-- helpers {{{1
peek :: State.State TokenStream (Maybe (Location.Located Token.Token))
peek = State.state $ \ toks -> (Safe.headMay toks, toks)

peek_matches :: (Maybe Token.Token -> Bool) -> State.State TokenStream Bool
peek_matches p = p <$> (Location.unlocate <$>) <$> peek

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

is_tt_v :: a -> Token.Token -> Token.Token -> Maybe a
is_tt_v v a b = if is_tt a b then Just v else Nothing

is_tt_u :: Token.Token -> Token.Token -> Maybe ()
is_tt_u = is_tt_v ()

consume :: TokenPredicateM r -> (Maybe (Token.LToken) -> Errors) -> Parser r
consume p e =
    State.state $ \case
        orig_toks@(tok:more_toks) ->
            case p $ Location.unlocate tok of
                Just res -> (Success res, more_toks)
                _ -> (Failed $ e $ Just tok, orig_toks)

        [] -> (Failed $ e Nothing, [])

assert_consume :: TokenPredicateM r -> Parser r
assert_consume p = consume p (\ _ -> error "assert_consume predicate failed")

advance :: ParseFn ()
advance = State.state $ \ toks -> ((), drop 1 toks)

choice :: [(MTokenPredicate, ParseFn a)] -> ParseFn a -> ParseFn a
choice choices def =
    peek >>= \ tok ->
    let unlocated = Location.unlocate <$> tok
    in case List.findIndex (($ unlocated) . fst) choices of
        Just i -> snd $ choices !! i
        Nothing -> def

{-
parse_list :: ParseConcept r -> ParseConcept [r]
parse_list = _
-}
