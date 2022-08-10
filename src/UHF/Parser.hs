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
type TokenPredicate = Token.Token -> Bool
type BreakingError = ParseError.ParseError
type RecoverableErrors = [ParseError.ParseError]

-- Parser {{{1
data Parser r =
    Parser { run_parser :: (TokenStream -> ParseResult (r, TokenStream)) }

instance Functor Parser where
    fmap f (Parser parser) =
        Parser $ \ toks ->
            let res = parser toks
            in (\ (a, toks') -> (f a, toks')) <$> res

instance Applicative Parser where
    pure a = Parser $ \ toks -> pure (a, toks)

    (Parser parser_f) <*> (Parser parser_v) = Parser $ \ toks ->
        -- this is actually in the ParseResult monad and not in the Parser monad
        parser_f toks >>= \ (f, toks') ->
        parser_v toks' >>= \ (v, toks'') ->
        Success (f v, toks'')

instance Monad Parser where
    (Parser parser_a) >>= b = Parser $ \ toks ->
        parser_a toks >>= \ (a, toks') ->
        run_parser (b a) toks'
-- ParseResult {{{1
data ParseResult r
    -- TODO: synchronization predicate
    = Failed RecoverableErrors BreakingError -- (Maybe TokenPredicate)
    | Recoverable RecoverableErrors r
    | Success r
    deriving (Show, Eq)

{-
instance (Eq r) => Eq (ParseResult r) where
    (Failed es1 e1 {- _ -}) == (Failed es2 e2 {- _ -}) = es1 == es2 && e1 == e2
    (Recoverable e1 r1) == (Recoverable e2 r2) = e1 == e2 && r1 == r2
    (Success r1) == (Success r2) = r1 == r2
    _ == _ = False

instance (Show r) => Show (ParseResult r) where
    show pr =
        case pr of
            (Failed es e {- s -}) -> "Failed " ++ show e -- ++ " " ++ show_sync s
            (Recoverable e r) -> "Recoverable " ++ show e ++ " " ++ show r
            (Success r) -> "Success " ++ show r
        where
            -- show_sync (Just _) = "<synchronization predicate>"
            -- show_sync Nothing = "<no synchronization predicate>"
-}

instance Functor ParseResult where
    fmap _ (Failed es e {- sync -}) = Failed es e {- sync -}
    fmap f (Recoverable es r) = Recoverable es (f r)
    fmap f (Success r) = Success (f r)

instance Applicative ParseResult where
    pure = Success

    a <*> b =
        case a of
            Failed a_es a_e {- sync -} -> Failed a_es a_e {- sync -}

            Recoverable a_es a_v ->
                case a_v <$> b of
                    Failed b_es b_e {- sync -} -> Failed (a_es ++ b_es) b_e {- sync -}
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> a_v <$> b

instance Monad ParseResult where
    a >>= b =
        case a of
            Failed a_es a_e {- sync -} -> Failed a_es a_e {- sync -}

            Recoverable a_es a_v ->
                case b a_v of
                    Failed b_es b_e {- sync -} -> Failed (a_es ++ b_es) b_e {- sync -}
                    Recoverable b_es b_v -> Recoverable (a_es ++ b_es) b_v
                    Success b_v -> Recoverable a_es b_v

            Success a_v -> b a_v
-- helpers {{{1
peek :: Parser Token.LToken
peek = Parser $ \ toks -> Success $ (head toks, toks)

is_tt :: Token.Token -> Token.Token -> Bool
is_tt a b = Data.toConstr a == Data.toConstr b

consume :: TokenPredicate -> (Token.LToken -> BreakingError) -> Parser Token.LToken
consume p er = Parser $
    \ (tok:more_toks) ->
        if p $ Location.unlocate tok
            then Success $ (tok, more_toks)
            else Failed [] $ er tok

advance :: Parser ()
advance = Parser $ \ toks -> Success ((), drop 1 toks)
-- combinators {{{1

-- sequence combinator is >>=

choice :: [Parser a] -> Parser a
choice choices = Parser $ \ toks ->
    try_choices choices [] toks
    where
        try_choices (c:cs) breaking_acc toks =
            case run_parser c toks of
                Success r -> return r
                Recoverable es r -> Recoverable es r
                Failed _ err -> try_choices cs (err:breaking_acc) toks
                -- ignore recoverable errors because they only apply if the choice could be decided
                -- e.g. a hypothetical recoverable error like "invalid function name" or something doesn't apply if it can't decide whether or not it is parsing a function or a datatype

        try_choices [] errs_acc (tok:_) = Failed [] (ParseError.NoneMatched tok errs_acc)

star :: Parser a -> Parser [a]
star a = Parser $ \ toks ->
    star' [] [] toks
    where
        star' err_acc a_acc toks =
            case run_parser a toks of
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
optional a = Parser $ \ toks ->
    case run_parser a toks of
        Success (r, toks') -> Success (Just r, toks')
        Recoverable e (r, toks') -> Recoverable e (Just r, toks')
        Failed _ _ -> Success (Nothing, toks)

-- andpred :: Parser a -> Parser ()
-- notpred :: Parser a -> Parser ()

{- TODO: synchronization (also "synchronization predicate" todo at definition of ParseResult)
sync :: TokenPredicate -> Parser ()
sync sync_p =
    parse_list
        (\case
            Token.EOF -> True
            t -> sync_p t)
        (consume (const $ Just ()) (error "unreachable")) >> return ()
m_sync :: Maybe TokenPredicate -> Parser ()
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
    consume is_alpha_iden_m (\ tok -> (nonempty_singleton $ ParseError.BadToken tok [(Token.AlphaIdentifier [], "type", Nothing)], Nothing)) `p_then` \ iden ->
    return (Success $ Type.Identifier iden)
-- exprs {{{2
expr_parse :: Parser Expr.Expr
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

data ParsingTest = forall r. (Show r, Eq r) => ParsingTest String (File.File, TokenStream) r [(String, Parser r)]
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
