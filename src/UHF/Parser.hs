{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , ParseError.ParseError
    , tests
    ) where

import UHF.Util.Prelude

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Parser.Parser as Parser
import qualified UHF.Parser.ParseError as ParseError

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

-- TODO: clean up

parse :: [Token.LNormalToken] -> Token.LNormalToken -> ([ParseError.ParseError], [AST.Decl])
parse toks eof_tok =
    case State.runStateT parse' (toks InfList.+++ InfList.repeat eof_tok) of
        Parser.ParseResult (errs, Right (res, _)) -> (errs, res)
        Parser.ParseResult (errs, Left err) -> (errs ++ [err], [])

parse' :: Parser.Parser [AST.Decl]
parse' = Parser.star decl_parse >>= \ ds -> Parser.consume "end of file" (Token.EOF ()) >> pure ds
-- decls {{{2
decl_parse :: Parser.Parser AST.Decl
decl_parse =
    Parser.choice
        [ data_parse
        , under_parse
        , type_sig_or_function_parse
        ]

data_parse :: Parser.Parser AST.Decl
data_parse =
    Parser.consume "data declaration" (Token.SingleTypeToken Token.Data) >>= \ data_tok ->
    Parser.return_fail [] (ParseError.NotImpl $ Location.Located (Location.just_span data_tok) "datatype declarations")

under_parse :: Parser.Parser AST.Decl
under_parse =
    Parser.consume "under declaration" (Token.SingleTypeToken Token.Under) >>= \ under_tok ->
    Parser.return_fail [] (ParseError.NotImpl $ Location.Located (Location.just_span under_tok) "'under' blocks")

type_sig_or_function_parse :: Parser.Parser AST.Decl
type_sig_or_function_parse =
    Parser.consume "type signature or binding" Parser.alpha_iden >>= \ (Location.Located _ (Token.AlphaIdentifier name)) ->
    Parser.choice
        [ binding_parse name
        , type_signature_parse name
        ]

binding_parse :: [Text] -> Parser.Parser AST.Decl
binding_parse decl_name =
    Parser.consume "binding" (Token.SingleTypeToken Token.Equal) >>= \ eq ->
    expr_parse >>= \ ex ->
    pure (AST.Decl'Binding decl_name ex)

type_signature_parse :: [Text] -> Parser.Parser AST.Decl
type_signature_parse decl_name =
    Parser.consume "type signature" (Token.SingleTypeToken Token.Colon) >>= \ colon ->
    type_parse >>= \ ty ->
    pure (AST.Decl'TypeSignature decl_name ty)
-- types {{{2
type_parse :: Parser.Parser AST.Type
type_parse =
    Parser.consume "type" Parser.alpha_iden >>= \ (Location.Located _ (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Identifier iden)
-- exprs {{{2
expr_parse :: Parser.Parser AST.Expr
expr_parse =
    Parser.peek >>= \ tok ->
    Parser.return_fail [] (ParseError.NotImpl (Location.Located (Location.just_span tok) "expressions")) -- TODO

-- tests {{{1
data ParsingTest = forall r. (Show r, Eq r) => ParsingTest [Char] (File.File, Parser.TokenStream) r [([Char], Parser.Parser r)]
make_token_stream :: [(Text, Token.NormalToken)] -> (File.File, Parser.TokenStream)
make_token_stream things =
    let (file, things') = SpanHelper.make_spans_with_items things
        l = last things'
    in (file, things' InfList.+++ InfList.repeat l)

parsing_tests :: [ParsingTest]
parsing_tests =
    [ ParsingTest "function decl"
        (make_token_stream [("x", Token.AlphaIdentifier ["x"]), ("=", Token.SingleTypeToken Token.Equal), ("'c'", Token.SingleTypeToken $ Token.CharLit 'c')])
        (AST.Decl'Binding ["x"] (AST.Expr'CharLit 'c'))
        [("decl_parse", decl_parse)]

    , ParsingTest "type signature"
        (make_token_stream [("x", Token.AlphaIdentifier ["x"]), (":", Token.SingleTypeToken Token.Colon), ("int", Token.AlphaIdentifier ["int"])])
        (AST.Decl'TypeSignature ["x"] (AST.Type'Identifier ["int"]))
        [("decl_parse", decl_parse)]

    , ParsingTest "data decl"
        (make_token_stream
            [ ("data", Token.SingleTypeToken Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent ()), ("Y", Token.AlphaIdentifier ["Y"]), ("string", Token.AlphaIdentifier ["string"]), ("newline", Token.Newline Token.NLLogical)
            , ("Z", Token.AlphaIdentifier ["Z"]), ("X", Token.AlphaIdentifier ["X"]), ("newline", Token.Newline Token.NLLogical)
            , ("dedent", Token.Dedent ())
            ])
        (error "not implemented yet")
        [("decl_parse", decl_parse), ("data_parse", data_parse)]

    , ParsingTest "under decl"
        (make_token_stream
            [ ("under", Token.SingleTypeToken Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent ()), ("x", Token.AlphaIdentifier ["x"]), ("=", Token.SingleTypeToken Token.Equal), ("2", Token.SingleTypeToken $ Token.IntLit Token.Dec 2), ("newline", Token.Newline Token.NLLogical)
            , ("dedent", Token.Dedent ())
            ])
        (error "not implemented yet")
        [("decl_parse", decl_parse), ("under_parse", under_parse)]
    ]

run_test :: ParsingTest -> TestTree
run_test (ParsingTest construct_name (_, construct_toks) construct_res parsers) =
    testGroup construct_name $
        map
            (\ (p_name, p) ->
                testCase (p_name ++ " parsing " ++ construct_name) $ Parser.ParseResult ([], Right construct_res) @=? fst <$> State.runStateT p construct_toks)
            parsers

test_parsing :: [TestTree]
test_parsing = map run_test parsing_tests

tests :: TestTree
tests = $(testGroupGenerator)
