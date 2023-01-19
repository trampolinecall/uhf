module UHF.Parser.Decl(decl, tests) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as Parser
import qualified UHF.Parser.ParseError as ParseError
import qualified UHF.Parser.Type as Type
import qualified UHF.Parser.Expr as Expr
import qualified UHF.Parser.Test as Test

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty

import qualified Control.Monad.Trans.State as State

decl :: Parser.Parser AST.Decl
decl =
    Parser.choice
        [ data_
        , binding
        , type_signature
        ]

data_ :: Parser.Parser AST.Decl
data_ =
    Parser.consume "data declaration" (Token.SingleTypeToken Token.Data) >>= \ data_tok ->
    Parser.return_fail [] (ParseError.NotImpl $ Location.Located (Location.just_span data_tok) "datatype declarations") -- TODO

binding :: Parser.Parser AST.Decl
binding =
    Parser.consume "binding name" (Token.AlphaIdentifier ()) >>= \ (Location.Located _ (Token.AlphaIdentifier name)) ->
    Parser.consume "'='" (Token.SingleTypeToken Token.Equal) >>= \ eq ->
    Expr.expr >>= \ ex ->
    pure (AST.Decl'Binding name ex)

type_signature :: Parser.Parser AST.Decl
type_signature =
    Parser.consume "type signature name" (Token.AlphaIdentifier ()) >>= \ (Location.Located _ (Token.AlphaIdentifier name)) ->
    Parser.consume "':'" (Token.SingleTypeToken Token.Colon) >>= \ colon ->
    Type.type_ >>= \ ty ->
    pure (AST.Decl'TypeSignature name ty)

--- tests {{{1
tests :: [Test.ParsingTest]
tests =
    [ Test.ParsingTest "function decl"
        (Test.make_token_stream [("x", Token.AlphaIdentifier ["x"]), ("=", Token.SingleTypeToken Token.Equal), ("'c'", Token.CharLit 'c')])
        (AST.Decl'Binding ["x"] (AST.Expr'CharLit 'c'))
        [("decl", decl), ("binding", binding)]

    , Test.ParsingTest "type signature"
        (Test.make_token_stream [("x", Token.AlphaIdentifier ["x"]), (":", Token.SingleTypeToken Token.Colon), ("int", Token.AlphaIdentifier ["int"])])
        (AST.Decl'TypeSignature ["x"] (AST.Type'Identifier ["int"]))
        [("decl", decl), ("type_signature", type_signature)]

    , Test.ParsingTest "data decl"
        (Test.make_token_stream
            [ ("data", Token.SingleTypeToken Token.Data), ("X", Token.AlphaIdentifier ["X"])
            , ("indent", Token.Indent ()), ("Y", Token.AlphaIdentifier ["Y"]), ("string", Token.AlphaIdentifier ["string"]), ("newline", Token.Newline Token.NLLogical)
            , ("Z", Token.AlphaIdentifier ["Z"]), ("X", Token.AlphaIdentifier ["X"]), ("newline", Token.Newline Token.NLLogical)
            , ("dedent", Token.Dedent ())
            ])
        (error "not implemented yet")
        [("decl", decl), ("data", data_)]
    ]
