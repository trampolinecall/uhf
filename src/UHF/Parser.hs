{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    -- ( parse

    -- , ParseError.ParseError
    -- , tests
    -- ) where
    where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.Token as Token
import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.AST.Decl as Decl
import qualified UHF.AST.Type as Type
import qualified UHF.AST.Expr as Expr

import qualified Data.Data as Data
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

-- TODO: clean up

type TokenStream = [Token.LToken]
-- TODO: type Errors = NonEmpty.NonEmpty ParseError.ParseError
type Errors = ()
type TokenPredicate = Token.Token -> Bool

-- Parser {{{1
data Parser e r =
    Parser { run_parser :: (TokenStream -> ParseResult e (r, TokenStream)) }

instance Functor (Parser e) where
    fmap f (Parser parser) =
        Parser $ \ toks ->
            let res = parser toks
            in (\ (a, toks') -> (f a, toks')) <$> res

instance Semigroup e => Applicative (Parser e) where
    pure a = Parser $ \ toks -> pure (a, toks)

    (Parser parser_f) <*> (Parser parser_v) = Parser $ \ toks ->
        -- this is actually in the ParseResult monad and not in the Parser monad
        parser_f toks >>= \ (f, toks') ->
        parser_v toks' >>= \ (v, toks'') ->
        Success (f v, toks'')

instance Semigroup e => Monad (Parser e) where
    (Parser parser_a) >>= b = Parser $ \ toks ->
        parser_a toks >>= \ (a, toks') ->
        run_parser (b a) toks'
-- ParseResult {{{1
data ParseResult e r
    -- TODO: synchronization predicate
    = Failed e -- (Maybe TokenPredicate)
    | Recoverable e r
    | Success r

instance (Eq e, Eq r) => Eq (ParseResult e r) where
    (Failed e1 {- _ -}) == (Failed e2 {- _ -}) = e1 == e2
    (Recoverable e1 r1) == (Recoverable e2 r2) = e1 == e2 && r1 == r2 -- TODO: check token streams?
    (Success r1) == (Success r2) = r1 == r2
    _ == _ = False

instance (Show e, Show r) => Show (ParseResult e r) where
    show pr =
        case pr of
            (Failed e {- s -}) -> "Failed " ++ show e -- ++ " " ++ show_sync s
            (Recoverable e r) -> "Recoverable " ++ show e ++ " " ++ show r
            (Success r) -> "Success " ++ show r
        where
            show_sync (Just _) = "<synchronization predicate>"
            show_sync Nothing = "<no synchronization predicate>"

instance Functor (ParseResult e) where
    fmap _ (Failed e {- sync -}) = Failed e {- sync -}
    fmap f (Recoverable e r) = Recoverable e (f r)
    fmap f (Success r) = Success (f r)

instance Semigroup e => Applicative (ParseResult e) where
    pure = Success

    a <*> b =
        case a of
            Failed a_e {- sync -} -> Failed a_e {- sync -}

            Recoverable a_e a_v ->
                case a_v <$> b of
                    Failed b_e {- sync -} -> Failed (a_e <> b_e) {- sync -}
                    Recoverable b_e b_v -> Recoverable (a_e <> b_e) b_v
                    Success b_v -> Recoverable a_e b_v

            Success a_v -> a_v <$> b

instance Semigroup e => Monad (ParseResult e) where
    a >>= b =
        case a of
            Failed a_e {- sync -} -> Failed a_e {- sync -}

            Recoverable a_e a_v ->
                case b a_v of
                    Failed b_e {- sync -} -> Failed (a_e <> b_e) {- sync -}
                    Recoverable b_e b_v -> Recoverable (a_e <> b_e) b_v
                    Success b_v -> Recoverable a_e b_v

            Success a_v -> b a_v
-- helpers {{{1
peek :: Parser Errors Token.LToken
peek = Parser $ \ toks -> Success $ (head toks, toks)

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

consume :: TokenPredicate -> Parser Errors Token.LToken
consume p = Parser $
    \ (tok:more_toks) ->
        if p $ Location.unlocate tok
            then Success $ (tok, more_toks)
            else Failed ()

advance :: Parser Errors ()
advance = Parser $ \ toks -> Success ((), drop 1 toks)
-- combinators {{{1

-- sequence combinator is >>=

choice :: [Parser Errors a] -> Parser Errors a
choice choices = Parser $ \ toks ->
    try_choices choices toks
    where
        try_choices (c:cs) toks =
            case run_parser c toks of
                Success r -> return r
                Recoverable e r -> Recoverable e r
                -- TODO: collect these
                Failed _ -> try_choices cs toks

        try_choices [] _ = Failed ()

star :: Parser Errors a -> Parser Errors [a]
star a = Parser $ \ toks ->
    star' [] [] toks
    where
        star' err_acc a_acc toks =
            case run_parser a toks of
                Success (r, toks') -> star' err_acc (a_acc ++ [r]) toks'
                Recoverable e (r, toks') -> star' (err_acc <> [e]) (a_acc ++ [r]) toks'
                Failed _ -> Success (a_acc, toks)
                    -- if null err_acc TODO
                        -- then Success ([], toks)
                        -- else Recoverable err_acc ([], toks)

plus :: Parser Errors a -> Parser Errors [a]
plus a = Parser $ \ toks ->
    plus' [] [] toks
    where
        plus' err_acc a_acc toks =
            case run_parser a toks of
                Success (r, toks') -> plus' err_acc (a_acc ++ [r]) toks'
                Recoverable e (r, toks') -> plus' (err_acc <> [e]) (a_acc ++ [r]) toks'
                Failed _ ->
                    if length a_acc == 0
                        then Failed () -- TODO err_acc list
                        else Success (a_acc, toks)

optional :: Parser Errors a -> Parser Errors (Maybe a)
optional a = Parser $ \ toks ->
    case run_parser a toks of
        Success (r, toks') -> Success (Just r, toks')
        Recoverable e (r, toks') -> Recoverable e (Just r, toks')
        Failed _ -> Success (Nothing, toks)

-- andpred :: Parser Errors a -> Parser Errors ()
-- notpred :: Parser Errors a -> Parser Errors ()

{- TODO: synchronization (also "synchronization predicate" todo at definition of ParseResult)
sync :: TokenPredicate -> Parser Errors ()
sync sync_p =
    parse_list
        (\case
            Token.EOF -> True
            t -> sync_p t)
        (consume (const $ Just ()) (error "unreachable")) >> return ()
m_sync :: Maybe TokenPredicate -> Parser Errors ()
m_sync Nothing = return ()
m_sync (Just sync_p) = sync sync_p
-}

-- TODO: use NonEmpty.singleton when a new stack resolver comes out with ghc 9.2.2
--  NonEmpty.singleton is only in base-4.15 or higher, but the only lts stack resolver that has that version of base
--  uses ghc 9.0.2, which is currently missing profiling libraries for base
--  this ghc bug is fixed in ghc 9.2.2, but there is not an lts stack resolver that uses ghc 9.2.2 yet
nonempty_singleton :: a -> NonEmpty.NonEmpty a
nonempty_singleton a = a NonEmpty.:| []
-- predicates {{{2
is_alpha_iden :: TokenPredicate
is_alpha_iden = is_tt (Token.AlphaIdentifier [])
-- parse {{{1
{- -- TODO
parse :: TokenStream -> Token.LToken -> ([ParseError.ParseError], [Decl.Decl])
parse toks eof_tok =
    let r_res = State.evalState parse' (toks ++ repeat eof_tok)
    in case r_res of
        Success res -> ([], res)
        Failed errs _ -> (NonEmpty.toList errs, [])
        Recoverable errs res _ -> (NonEmpty.toList errs, res)

parse' :: Parser Errors [Decl.Decl]
parse' = parse_list (is_tt Token.EOF) decl_parse
-- decls {{{2
decl_lookahead_matches :: TokenPredicate
decl_lookahead_matches Token.Data = True
decl_lookahead_matches Token.Under = True
decl_lookahead_matches (Token.AlphaIdentifier []) = True
decl_lookahead_matches _ = False
--
decl_parse :: Parser Errors Decl.Decl
decl_parse =
    choice
        [ (is_tt Token.Data, advance >> data_parse)
        , (is_tt Token.Under, advance >> under_parse)
        , (is_alpha_iden, assert_consume is_alpha_iden_m >>= type_sig_or_function_parse)
        ]
        (peek >>= \ tok ->
        return (Failed
            (nonempty_singleton $ ParseError.BadToken tok
                [ (Token.Data, "datatype declaration", Nothing)
                , (Token.Under, "'under' block", Nothing)
                , (Token.AlphaIdentifier [], "function declaration", Just "name")
                ])
            (Just decl_lookahead_matches)))

data_parse :: Parser Errors Decl.Decl
data_parse =
    peek >>= \ tok -> -- TODO: maybe use the 'data' token
    return (Failed (nonempty_singleton $ ParseError.NotImpl $ Location.Located (Location.just_span tok) "datatype declarations") (Just decl_lookahead_matches))

under_parse :: Parser Errors Decl.Decl
under_parse =
    peek >>= \ tok -> -- TODO: maybe use the 'under' token
    return (Failed (nonempty_singleton $ ParseError.NotImpl $ Location.Located (Location.just_span tok) "'under' blocks") (Just decl_lookahead_matches))

type_sig_or_function_parse :: [String] -> Parser Errors Decl.Decl
type_sig_or_function_parse name =
    choice
        [ (is_tt Token.Equal, advance >> binding_parse name)
        , (is_tt Token.Colon, advance >> type_signature_parse name)
        ]
        (peek >>= \ tok ->
        return (Failed (nonempty_singleton $ ParseError.BadToken tok [(Token.Colon, "type signature", Nothing), (Token.Equal, "declaration", Nothing)]) Nothing))

binding_parse :: [String] -> Parser Errors Decl.Decl
binding_parse decl_name =
    expr_parse `p_then` \ ex ->
    return (Success $ Decl.Binding decl_name ex)

type_signature_parse :: [String] -> Parser Errors Decl.Decl
type_signature_parse decl_name =
    type_parse `p_then` \ ty ->
    return (Success $ Decl.TypeSignature decl_name ty)
-- types {{{2
type_parse :: Parser Errors Type.Type
type_parse =
    consume is_alpha_iden_m (\ tok -> (nonempty_singleton $ ParseError.BadToken tok [(Token.AlphaIdentifier [], "type", Nothing)], Nothing)) `p_then` \ iden ->
    return (Success $ Type.Identifier iden)
-- exprs {{{2
expr_parse :: Parser Errors Expr.Expr
expr_parse =
    peek >>= \ tok ->
    return $ Failed (nonempty_singleton $ ParseError.NotImpl (Location.Located (Location.just_span tok) "expressions")) Nothing -- TODO
-}
-- tests {{{1
{- -- TODO
test_p_then :: [TestTree]
test_p_then =
    let f, r, s :: Parser [Int] Bool
        f', r', s' :: Bool -> Parser [Int] String

        f = return $ Failed [0] Nothing
        r = return $ Recoverable [1] False Nothing
        s = return $ Success True

        f' =
            \case
                False -> return $ Failed [2] Nothing
                True -> return $ Failed [3] Nothing

        r' =
            \case
                False -> return $ Recoverable [4] "false recoverable" Nothing
                True -> return $ Recoverable [5] "true recoverable" Nothing

        s' =
            \case
                False -> return $ Success "false success"
                True -> return $ Success "true success"

        t n a b e = testCase n $ e @=? (State.evalState (a `p_then` b) [])
    in
        [ t "f-f" f f' (Failed [0] Nothing)
        , t "f-r" f r' (Failed [0] Nothing)
        , t "f-s" f s' (Failed [0] Nothing)
        , t "r-f" r f' (Failed [1, 2] Nothing)
        , t "r-r" r r' (Recoverable [1, 4] "false recoverable" Nothing)
        , t "r-s" r s' (Recoverable [1] "false success" Nothing)
        , t "s-f" s f' (Failed [3] Nothing)
        , t "s-r" s r' (Recoverable [5] "true recoverable" Nothing)
        , t "s-s" s s' (Success "true success")
        ]

data ParsingTest = forall r. (Show r, Eq r) => ParsingTest String (File.File, TokenStream) r [(String, Parser Errors r)]
parsing_tests :: [ParsingTest]
parsing_tests =
    [ ParsingTest "function decl"
        (SpanHelper.make_spans_with_items [("x", Token.AlphaIdentifier ["x"]), ("=", Token.Equal), ("'c'", Token.CharLit 'c')])
        (Decl.Binding ["x"] (Expr.CharLit 'c'))
        [("decl_parse", decl_parse)]
    , ParsingTest "type signature"
        (SpanHelper.make_spans_with_items [("x", Token.AlphaIdentifier ["x"]), (":", Token.Colon), ("int", Token.AlphaIdentifier ["int"])])
        (Decl.TypeSignature ["x"] (Type.Identifier ["int"]))
        [("decl_parse", decl_parse)]
    , ParsingTest "data decl"
        (SpanHelper.make_spans_with_items
            [ ("data", Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent), ("Y", Token.AlphaIdentifier ["Y"]), ("string", Token.AlphaIdentifier ["string"]), ("newline", Token.Newline)
            , ("Z", Token.AlphaIdentifier ["Z"]), ("X", Token.AlphaIdentifier ["X"]), ("newline", Token.Newline)
            , ("dedent", Token.Dedent)
            ])
        (error "not implemented yet")
        [("decl_parse", decl_parse), ("data_parse", data_parse)]
    , ParsingTest "under decl"
        (SpanHelper.make_spans_with_items
            [ ("under", Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent), ("x", Token.AlphaIdentifier ["x"]), ("=", Token.Equal), ("2", Token.IntLit Token.Dec 2), ("newline", Token.Newline)
            , ("dedent", Token.Dedent)
            ])
        (error "not implemented yet")
        [("decl_parse", decl_parse), ("under_parse", under_parse)]
    ]

run_test :: ParsingTest -> TestTree
run_test (ParsingTest construct_name (_, construct_toks) construct_res parsers) =
    testGroup construct_name $
        map
            (\ (p_name, p) ->
            testCase (p_name ++ " parsing " ++ construct_name) $ Success construct_res @=? (State.evalState p construct_toks))
            parsers

test_parsing :: [TestTree]
test_parsing = map run_test parsing_tests

tests :: TestTree
tests = $(testGroupGenerator)
-}
