{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , ParseError.ParseError
    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Parser.Parser as Parser
import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST.Decl as Decl
import qualified UHF.AST.Type as Type
import qualified UHF.AST.Expr as Expr

import qualified UHF.Util.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

-- TODO: clean up

parse :: [Token.LToken] -> Token.LToken -> ([ParseError.ParseError], [Decl.Decl])
parse toks eof_tok =
    let r_res = Parser.run_parser parse' (InfList.from_list eof_tok toks)
    in case r_res of
        Parser.Success (res, _) -> ([], res)
        Parser.Failed errs err -> (errs ++ [err], [])
        Parser.Recoverable errs (res, _) -> (errs, res)

parse' :: Parser.Parser [Decl.Decl]
parse' = Parser.star decl_parse >>= \ ds -> Parser.consume "end of file" Token.EOF >> return ds
-- decls {{{2
decl_lookahead_matches :: Parser.TokenPredicate
decl_lookahead_matches Token.Data = True
decl_lookahead_matches Token.Under = True
decl_lookahead_matches (Token.AlphaIdentifier []) = True
decl_lookahead_matches _ = False
--
decl_parse :: Parser.Parser Decl.Decl
decl_parse =
    Parser.choice
        [ data_parse
        , under_parse
        , type_sig_or_function_parse
        ]

data_parse :: Parser.Parser Decl.Decl
data_parse =
    Parser.consume "data declaration" Token.Data >>= \ data_tok ->
    Parser.return_fail [] (ParseError.NotImpl $ Location.Located (Location.just_span data_tok) "datatype declarations")

under_parse :: Parser.Parser Decl.Decl
under_parse =
    Parser.consume "under declaration" Token.Under >>= \ under_tok ->
    Parser.return_fail [] (ParseError.NotImpl $ Location.Located (Location.just_span under_tok) "'under' blocks")

type_sig_or_function_parse :: Parser.Parser Decl.Decl
type_sig_or_function_parse =
    Parser.consume "type signature or binding" Parser.alpha_iden >>= \ (Location.Located _ (Token.AlphaIdentifier name)) ->
    Parser.choice
        [ binding_parse name
        , type_signature_parse name
        ]

binding_parse :: [String] -> Parser.Parser Decl.Decl
binding_parse decl_name =
    Parser.consume "binding" Token.Equal >>= \ eq ->
    expr_parse >>= \ ex ->
    return (Decl.Binding decl_name ex)

type_signature_parse :: [String] -> Parser.Parser Decl.Decl
type_signature_parse decl_name =
    Parser.consume "type signature" Token.Colon >>= \ colon ->
    type_parse >>= \ ty ->
    return (Decl.TypeSignature decl_name ty)
-- types {{{2
type_parse :: Parser.Parser Type.Type
type_parse =
    Parser.consume "type" Parser.alpha_iden >>= \ (Location.Located _ (Token.AlphaIdentifier iden)) ->
    return (Type.Identifier iden)
-- exprs {{{2
expr_parse :: Parser.Parser Expr.Expr
expr_parse =
    Parser.peek >>= \ tok ->
    Parser.return_fail [] (ParseError.NotImpl (Location.Located (Location.just_span tok) "expressions")) -- TODO

-- tests {{{1
data ParsingTest = forall r. (Show r, Eq r) => ParsingTest String (File.File, Parser.TokenStream) r [(String, Parser.Parser r)]
make_token_stream :: [(String, Token.Token)] -> (File.File, Parser.TokenStream)
make_token_stream things =
    let (file, things') = SpanHelper.make_spans_with_items things
        l = last things'
    in (file, InfList.from_list l things')

parsing_tests :: [ParsingTest]
parsing_tests =
    [ ParsingTest "function decl"
        (make_token_stream [("x", Token.AlphaIdentifier ["x"]), ("=", Token.Equal), ("'c'", Token.CharLit 'c')])
        (Decl.Binding ["x"] (Expr.CharLit 'c'))
        [("decl_parse", decl_parse)]

    , ParsingTest "type signature"
        (make_token_stream [("x", Token.AlphaIdentifier ["x"]), (":", Token.Colon), ("int", Token.AlphaIdentifier ["int"])])
        (Decl.TypeSignature ["x"] (Type.Identifier ["int"]))
        [("decl_parse", decl_parse)]

    , ParsingTest "data decl"
        (make_token_stream
            [ ("data", Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent), ("Y", Token.AlphaIdentifier ["Y"]), ("string", Token.AlphaIdentifier ["string"]), ("newline", Token.Newline)
            , ("Z", Token.AlphaIdentifier ["Z"]), ("X", Token.AlphaIdentifier ["X"]), ("newline", Token.Newline)
            , ("dedent", Token.Dedent)
            ])
        (error "not implemented yet")
        [("decl_parse", decl_parse), ("data_parse", data_parse)]

    , ParsingTest "under decl"
        (make_token_stream
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
                testCase (p_name ++ " parsing " ++ construct_name) $ Parser.Success construct_res @=? fst <$> (Parser.run_parser p construct_toks))
            parsers

test_parsing :: [TestTree]
test_parsing = map run_test parsing_tests

tests :: TestTree
tests = $(testGroupGenerator)
