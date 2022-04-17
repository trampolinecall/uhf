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

p_then :: Parser a -> (a -> Parser b) -> Parser b
p_then a b =
    a >>= \case
        Success a_res -> b a_res

        Recoverable a_errs a_res a_sync ->
            m_sync a_sync >>
            b a_res >>= add_to_b_res a_errs

        Failed a_errs a_sync_p ->
            m_sync a_sync_p >>
            return (Failed a_errs a_sync_p)

    where
        add_to_b_res a_errs (Success b_res) = return $ Recoverable a_errs b_res Nothing
        add_to_b_res a_errs (Recoverable b_errs b_res b_sync_p) = return $ Recoverable (a_errs `nonempty_append` b_errs) b_res b_sync_p
        add_to_b_res a_errs (Failed b_errs b_sync_p) = return $ Failed (a_errs `nonempty_append` b_errs) b_sync_p
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
decl_lookahead_matches Token.Data = True
decl_lookahead_matches Token.Under = True
decl_lookahead_matches (Token.AlphaIdentifier []) = True
decl_lookahead_matches _ = False
--
decl_parse :: Parser Decl.Decl
decl_parse =
    choice
        [ (is_tt Token.Data, advance >> data_parse)
        , (is_tt Token.Under, advance >> under_parse)
        , (is_tt (Token.AlphaIdentifier []), assert_consume (\ (Token.AlphaIdentifier name) -> Just name) >>= type_sig_or_function_parse)
        ]
        (peek >>= \ tok ->
        return (Failed
            (nonempty_singleton $ ParseError.BadToken tok
                [ (Token.Data, "datatype declaration", Nothing)
                , (Token.Under, "'under' block", Nothing)
                , (Token.AlphaIdentifier [], "function declaration", Just "name")
                ])
            (Just decl_lookahead_matches)))

data_parse :: Parser Decl.Decl
data_parse =
    peek >>= \ tok -> -- TODO: maybe use the 'data' token
    return (Failed (nonempty_singleton $ ParseError.NotImpl $ Location.Located (Location.just_span tok) "datatype declarations") (Just decl_lookahead_matches))

under_parse :: Parser Decl.Decl
under_parse =
    peek >>= \ tok -> -- TODO: maybe use the 'under' token
    return (Failed (nonempty_singleton $ ParseError.NotImpl $ Location.Located (Location.just_span tok) "'under' blocks") (Just decl_lookahead_matches))

type_sig_or_function_parse :: [String] -> Parser Decl.Decl
type_sig_or_function_parse name =
    choice
        [ (is_tt Token.Equal, advance >> binding_parse name)
        , (is_tt Token.Colon, advance >> type_signature_parse name)
        ]
        (peek >>= \ tok ->
        return (Failed (nonempty_singleton $ ParseError.BadToken tok [(Token.Colon, "type signature", Nothing), (Token.Equal, "declaration", Nothing)]) Nothing))

binding_parse :: [String] -> Parser Decl.Decl
binding_parse decl_name =
    expr_parse `p_then` \ ex ->
    return (Success $ Decl.Binding decl_name ex)

type_signature_parse :: [String] -> Parser Decl.Decl
type_signature_parse decl_name =
    type_parse `p_then` \ ty ->
    return (Success $ Decl.TypeSignature decl_name ty)
-- types {{{2
type_parse :: Parser Type.Type
type_parse =
    consume (\case
        Token.AlphaIdentifier iden -> Just iden
        _ -> Nothing)
        (\ tok -> (nonempty_singleton $ ParseError.BadToken tok [(Token.AlphaIdentifier [], "type", Nothing)], Nothing)) `p_then` \ iden ->
    return (Success $ Type.Identifier iden)
-- exprs {{{2
expr_parse :: Parser Expr.Expr
expr_parse =
    peek >>= \ tok ->
    return $ Failed (nonempty_singleton $ ParseError.NotImpl (Location.Located (Location.just_span tok) "expressions")) Nothing -- TODO
-- helpers {{{1
peek :: State.State TokenStream Token.LToken
peek = State.state $ \ toks -> (head toks, toks)

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

assert_consume :: TokenPredicateM r -> ParseFn r
assert_consume p =
    consume p (const $ error "assert_consume predicate failed") >>= \case
        Success r -> return r
        _ -> error "assert_consume predicate failed"

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
                    m_sync m_sync_p >>
                    maybe_continue (e_acc ++ NonEmpty.toList err) p_acc p

                Recoverable err res m_sync_p ->
                    m_sync m_sync_p >>
                    maybe_continue (e_acc ++ NonEmpty.toList err) (p_acc ++ [res]) p

                Success res ->
                    maybe_continue e_acc (p_acc ++ [res]) p

        maybe_continue e_acc p_acc p =
            peek >>= \ tok ->
            let unlocated = Location.unlocate tok
            in case (is_tt Token.EOF unlocated, stop unlocated) of
                (False, False) -> p' e_acc p_acc p
                _ ->
                    case e_acc of
                        [] -> return $ Success p_acc
                        e:e' -> return $ Recoverable (e NonEmpty.:| e') p_acc Nothing

sync :: TokenPredicate -> ParseFn ()
sync sync_p = parse_list sync_p (consume (const $ Just ()) (error "unreachable")) >> return ()
m_sync :: Maybe TokenPredicate -> ParseFn ()
m_sync Nothing = return ()
m_sync (Just sync_p) = sync sync_p

-- TODO: use NonEmpty.singleton when a new stack resolver comes out with ghc 9.2.2
--  NonEmpty.singleton is only in base-4.15 or higher, but the only lts stack resolver that has that version of base
--  uses ghc 9.0.2, which is currently missing profiling libraries for base
--  this ghc bug is fixed in ghc 9.2.2, but there is not an lts stack resolver that uses ghc 9.2.2 yet
nonempty_singleton :: a -> NonEmpty.NonEmpty a
nonempty_singleton a = a NonEmpty.:| []

-- TODO: use NonEmpty.append when there is a stack resolver that has base-4.16
nonempty_append :: NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a -> NonEmpty.NonEmpty a
nonempty_append (a NonEmpty.:| as) (b NonEmpty.:| bs) =
    a NonEmpty.:| (as ++ b:bs)
