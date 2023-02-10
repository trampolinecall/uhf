{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , Error.BacktrackingError
    , Error.OtherError
    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as PEG
import qualified UHF.Parser.Error as Error
import qualified UHF.Parser.Test as Test

import qualified UHF.Token as Token
import qualified UHF.AST as AST

import qualified Data.InfList as InfList

import qualified UHF.IO.Location as Location

-- parse {{{1
parse :: [Token.LToken] -> Token.LToken -> ([Error.OtherError], Maybe (Location.Located [Error.BacktrackingError]), [AST.Decl])
parse toks eof_tok =
    case PEG.run_parser parse' (InfList.zip (InfList.iterate (1+) 0) (toks InfList.+++ InfList.repeat eof_tok)) of
        (other_errors, _, Just (res, _)) -> (other_errors, Nothing, res)
        (other_errors, bt_errors, _) -> (other_errors, choose_error bt_errors, [])
    where
        parse' :: PEG.Parser [AST.Decl]
        parse' = PEG.star decl >>= \ ds -> PEG.consume' "end of file" (Token.EOF ()) >> pure ds

        choose_error :: [Error.BacktrackingError] -> Maybe (Location.Located [Error.BacktrackingError])
        choose_error [] = Nothing
        choose_error errs =
            let max_ind = maximum $ map (\ (Error.BadToken ind _ _ _) -> ind) errs
                latest_errors = filter (\ (Error.BadToken ind _ _ _) -> ind == max_ind) errs
                (Error.BadToken _ (Location.Located latest_span _) _ _) = head latest_errors
            in Just $ Location.Located latest_span latest_errors
-- decl {{{1
decl :: PEG.Parser AST.Decl
decl =
    PEG.choice
        [ decl_data_
        , decl_binding
        ]

decl_data_ :: PEG.Parser AST.Decl
decl_data_ =
    PEG.consume' "data declaration" (Token.SingleTypeToken Token.Data) >>= \ data_tok ->
    PEG.other_error [Error.NotImpl $ Location.Located (Location.just_span data_tok) "datatype declarations"] >>
    pure undefined -- TODO

decl_binding :: PEG.Parser AST.Decl
decl_binding =
    PEG.consume' "binding name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ eq ->
    expr >>= \ ex ->
    pure (AST.Decl'Value (todo (Location.Located name_sp name)) ex)
-- expr {{{1
expr :: PEG.Parser AST.Expr
expr =
    PEG.choice
        [ expr_identifier
        , expr_char_lit
        , expr_string_lit
        , expr_int_lit
        , expr_float_lit
        , expr_bool_lit
        ]

expr_identifier :: PEG.Parser AST.Expr
expr_identifier =
    PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Expr'Identifier (Location.Located iden_sp iden))

expr_char_lit :: PEG.Parser AST.Expr
expr_char_lit =
    PEG.consume' "character literal" (Token.Char ()) >>= \ (Location.Located _ (Token.Char ch)) ->
    pure (AST.Expr'Char ch)

expr_string_lit :: PEG.Parser AST.Expr
expr_string_lit =
    PEG.consume' "string literal" (Token.String ()) >>= \ (Location.Located _ (Token.String s)) ->
    pure (AST.Expr'String s)

expr_int_lit :: PEG.Parser AST.Expr
expr_int_lit =
    PEG.consume' "integer literal" (Token.Int () ()) >>= \ (Location.Located _ (Token.Int _ i)) ->
    pure (AST.Expr'Int i)

expr_float_lit :: PEG.Parser AST.Expr
expr_float_lit =
    PEG.consume' "float literal" (Token.Float ()) >>= \ (Location.Located _ (Token.Float f)) ->
    pure (AST.Expr'Float f)

expr_bool_lit :: PEG.Parser AST.Expr
expr_bool_lit =
    PEG.consume' "bool literal" (Token.Bool ()) >>= \ (Location.Located _ (Token.Bool b)) ->
    pure (AST.Expr'Bool b)
-- type {{{1
type_ :: PEG.Parser AST.Type
type_ =
    PEG.consume' "type" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Identifier (Location.Located iden_sp iden))
-- tests {{{1
test_decls :: [TestTree]
test_decls = map Test.run_test $
    [ Test.ParsingTest "binding"
        (Test.make_token_stream [("x", Token.AlphaIdentifier [Location.dummy_locate "x"]), ("=", Token.SingleTypeToken Token.Equal), ("'c'", Token.Char 'c')])
        (AST.Decl'Value (todo (Location.dummy_locate [Location.dummy_locate "x"])) (AST.Expr'Char 'c'))
        [("decl", decl), ("binding", decl_binding)]

    , Test.ParsingTest "data decl"
        (Test.make_token_stream
            [ ("data", Token.SingleTypeToken Token.Data), ("X", Token.AlphaIdentifier [Location.dummy_locate "X"])
            , ("{", Token.SingleTypeToken Token.OBrace), ("Y", Token.AlphaIdentifier [Location.dummy_locate "Y"]), ("string", Token.AlphaIdentifier [Location.dummy_locate "string"]), (";", Token.SingleTypeToken Token.Semicolon)
            , ("Z", Token.AlphaIdentifier [Location.dummy_locate "Z"]), ("X", Token.AlphaIdentifier [Location.dummy_locate "X"]), (";", Token.SingleTypeToken Token.Semicolon)
            , ("}", Token.SingleTypeToken Token.CBrace)
            ])
        (error "not implemented yet")
        [("decl", decl), ("data", decl_data_)]
    ]

tests :: TestTree
tests = $(testGroupGenerator)
