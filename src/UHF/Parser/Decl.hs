module UHF.Parser.Decl(decl, tests) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as PEG
import qualified UHF.Parser.Error as Error
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

decl :: PEG.Parser AST.Decl
decl =
    PEG.choice
        [ data_
        , binding
        ]

data_ :: PEG.Parser AST.Decl
data_ =
    PEG.consume' "data declaration" (Token.SingleTypeToken Token.Data) >>= \ data_tok ->
    PEG.other_error [Error.NotImpl $ Location.Located (Location.just_span data_tok) "datatype declarations"] >>
    pure undefined -- TODO

binding :: PEG.Parser AST.Decl
binding =
    PEG.consume' "binding name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ eq ->
    Expr.expr >>= \ ex ->
    pure (AST.Decl'Binding (Location.Located name_sp name) ex)

--- tests {{{1
tests :: [Test.ParsingTest]
tests =
    [ Test.ParsingTest "function decl"
        (Test.make_token_stream [("x", Token.AlphaIdentifier [Location.dummy_locate "x"]), ("=", Token.SingleTypeToken Token.Equal), ("'c'", Token.CharLit 'c')])
        (AST.Decl'Binding (Location.dummy_locate [Location.dummy_locate "x"]) (AST.Expr'CharLit 'c'))
        [("decl", decl), ("binding", binding)]

    , Test.ParsingTest "data decl"
        (Test.make_token_stream
            [ ("data", Token.SingleTypeToken Token.Data), ("X", Token.AlphaIdentifier [Location.dummy_locate "X"])
            , ("{", Token.SingleTypeToken Token.OBrace), ("Y", Token.AlphaIdentifier [Location.dummy_locate "Y"]), ("string", Token.AlphaIdentifier [Location.dummy_locate "string"]), (";", Token.SingleTypeToken Token.Semicolon)
            , ("Z", Token.AlphaIdentifier [Location.dummy_locate "Z"]), ("X", Token.AlphaIdentifier [Location.dummy_locate "X"]), (";", Token.SingleTypeToken Token.Semicolon)
            , ("}", Token.SingleTypeToken Token.CBrace)
            ])
        (error "not implemented yet")
        [("decl", decl), ("data", data_)]
    ]
