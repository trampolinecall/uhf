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
import qualified Data.List.NonEmpty as NonEmpty
import qualified Control.Monad.State as State

type TokenStream = [Token.LToken]
type Errors = NonEmpty.NonEmpty ParseError.ParseError

type ParseFn r = State.State TokenStream r
type Parser r = ParseFn (ParseResult Errors r)
type TokenPredicate = Token.Token -> Bool
type TokenPredicateM a = Token.Token -> Maybe a

-- ParseResult {{{1
data ParseResult e r
    = Failed e (Maybe TokenPredicate)
    | Recoverable e r (Maybe TokenPredicate)
    | Success r

instance Functor (ParseResult e) where
    fmap _ (Failed e s) = Failed e s
    fmap f (Recoverable e a s) = Recoverable e (f a) s
    fmap f (Success a) = Success (f a)

instance Semigroup e => Applicative (ParseResult e) where
    pure = Success

    (Success f) <*> (Success b) = Success $ f b
    (Failed e s) <*> (Success _) = Failed e s
    (Success _) <*> (Failed e s) = Failed e s
    (Failed e1 _) <*> (Failed e2 s) = Failed (e1 <> e2) s -- not sure how this should work

instance Semigroup e => Monad (ParseResult e) where
    Success a >>= f = f a
    Failed e s >>= _ = Failed e s

-- parse {{{1
parse :: TokenStream -> Token.LToken -> ([ParseError.ParseError], [Decl.Decl])
parse toks eof_tok =
    let r_res = State.evalState parse' (toks ++ repeat eof_tok)
    in case r_res of
        Success res -> ([], res)
        Failed errs _ -> (NonEmpty.toList errs, [])
        Recoverable errs res _ -> (NonEmpty.toList errs, res)

parse' :: Parser [Decl.Decl]
parse' = parse_list (is_tt Token.EOF) decl_parse
-- decls {{{2
decl_lookahead_matches :: TokenPredicate
decl_lookahead_matches = is_tt (Token.AlphaIdentifier [])

decl_parse :: Parser Decl.Decl
decl_parse =
    consume (\case
        Token.AlphaIdentifier name -> Just name
        _ -> Nothing)
        (\ t -> (nonempty_singleton $ ParseError.BadToken t [(Token.AlphaIdentifier [], "declaration", Just "name")], Just decl_lookahead_matches)) >>=
    \case
        Success decl_name ->
            choice
                [ (is_tt Token.Equal, advance >> binding_parse decl_name)
                , (is_tt Token.Colon, advance >> type_signature_parse decl_name)
                ]
                (peek >>= \ tok ->
                return (Failed (nonempty_singleton $ ParseError.BadToken tok [(Token.Colon, "type signature", Nothing), (Token.Equal, "declaration", Nothing)]) Nothing))
        Failed e s -> return $ Failed e s

binding_parse :: [String] -> Parser Decl.Decl
binding_parse decl_name =
    assert_consume (is_tt_u Token.Equal) >>
    expr_parse >>= \case
        Success ex -> return $ Success $ Decl.Binding decl_name ex
        Failed e s -> return $ Failed e s

type_signature_parse :: [String] -> Parser Decl.Decl
type_signature_parse decl_name =
    assert_consume (is_tt_u Token.Colon) >>
    type_parse >>= \case
        Success ty -> return $ Success $ Decl.TypeSignature decl_name ty
        Failed e s -> return $ Failed e s
-- types {{{2
type_parse :: Parser Type.Type
type_parse =
    consume (\case
        Token.AlphaIdentifier iden -> Just iden
        _ -> Nothing)
        (\ tok -> (nonempty_singleton $ ParseError.BadToken tok [(Token.AlphaIdentifier [], "type", Nothing)], Nothing))
        >>=
    \case
        Success iden ->
            return $ Success $ Type.Identifier iden
        Failed e s -> return $ Failed e s


-- exprs {{{2
expr_parse :: Parser Expr.Expr
expr_parse =
    peek >>= \ tok ->
    return $ Failed (nonempty_singleton $ ParseError.NotImpl (Location.Located (Location.just_span tok) "expressions")) Nothing -- TODO
-- helpers {{{1
peek :: State.State TokenStream Token.LToken
peek = State.state $ \ toks -> (head toks, toks)

peek_matches :: (Token.Token -> Bool) -> State.State TokenStream Bool
peek_matches p = p <$> Location.unlocate <$> peek

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

is_tt_v :: a -> Token.Token -> Token.Token -> Maybe a
is_tt_v v a b = if is_tt a b then Just v else Nothing

is_tt_u :: Token.Token -> Token.Token -> Maybe ()
is_tt_u = is_tt_v ()

consume :: TokenPredicateM r -> (Token.LToken -> (Errors, Maybe TokenPredicate)) -> Parser r
consume p e =
    State.state $ \case
        orig_toks@(tok:more_toks) ->
            case p $ Location.unlocate tok of
                Just res -> (Success res, more_toks)
                _ ->
                    let (errs, sync_p) = e tok
                    in (Failed errs sync_p, orig_toks)

        [] -> error "unreachable"

assert_consume :: TokenPredicateM r -> Parser r
assert_consume p = consume p (\ _ -> error "assert_consume predicate failed")

advance :: ParseFn ()
advance = State.state $ \ toks -> ((), drop 1 toks)

choice :: [(TokenPredicate, ParseFn a)] -> ParseFn a -> ParseFn a
choice choices def =
    peek >>= \ tok ->
    let unlocated = Location.unlocate tok
    in case List.findIndex (($ unlocated) . fst) choices of
        Just i -> snd $ choices !! i
        Nothing -> def

parse_list :: TokenPredicate -> Parser a -> Parser [a]
parse_list stop = p' [] []
    where
        p' e_acc p_acc p =
            p >>= \case
                Failed err m_sync_p ->
                    maybe_sync m_sync_p >>
                    maybe_continue (e_acc ++ NonEmpty.toList err) p_acc p

                Recoverable err res m_sync_p ->
                    maybe_sync m_sync_p >>
                    maybe_continue (e_acc ++ NonEmpty.toList err) (p_acc ++ [res]) p

                Success res ->
                    maybe_continue e_acc (p_acc ++ [res]) p

        maybe_sync Nothing = return ()
        maybe_sync (Just sync_p) = sync sync_p
        maybe_continue e_acc p_acc p =
            peek >>= \ tok ->
            case stop $ Location.unlocate tok of
                True -> p' e_acc p_acc p
                False ->
                    case e_acc of
                        [] -> return $ Success p_acc
                        e:e' -> return $ Recoverable (e NonEmpty.:| e') p_acc Nothing


sync :: TokenPredicate -> ParseFn ()
sync sync_p = parse_list sync_p (consume (const $ Just ()) (error "unreachable")) >> return ()

-- TODO: use NonEmpty.singleton when a new stack resolver comes out with ghc 9.2.2
--  NonEmpty.singleton is only in base-1.15 or higher, but the only lts stack resolver that has that version of base
--  uses ghc 9.0.2, which is currently missing profiling libraries for base
--  this ghc bug is fixed in ghc 9.2.2, but there is not an lts stack resolver that uses ghc 9.2.2 yet
nonempty_singleton :: a -> NonEmpty.NonEmpty a
nonempty_singleton a = a NonEmpty.:| []
