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
        [ decl_data
        , decl_binding
        , decl_typesyn
        ]

decl_data :: PEG.Parser AST.Decl
decl_data =
    PEG.consume' "data declaration" (Token.SingleTypeToken Token.Data) >>= \ _ ->
    PEG.consume' "datatype name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>= \ _ ->
    PEG.star variant >>= \ variants ->
    PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ _ ->
    pure (AST.Decl'Data (Location.Located name_sp name) variants) -- TODO
    where
        variant =
            PEG.consume' "variant name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
            PEG.choice [anon_variant $ Location.Located name_sp name, named_variant $ Location.Located name_sp name]

        anon_variant name =
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ _ ->
            PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ _ ->
            PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
            pure (AST.DataVariant'Anon name field_types)

        named_variant name =
            PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>= \ _ ->
            PEG.star (
                PEG.consume' "field name" (Token.AlphaIdentifier ()) >>= \ (Location.Located field_name_sp (Token.AlphaIdentifier field_name)) ->
                PEG.consume' "':'" (Token.SingleTypeToken Token.Colon) >>= \ _ ->
                type_ >>= \ field_ty ->
                PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
                pure (Location.Located field_name_sp field_name, field_ty)
            ) >>= \ fields ->
            PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ _ ->
            PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
            pure (AST.DataVariant'Named name fields)

decl_binding :: PEG.Parser AST.Decl
decl_binding =
    pattern >>= \ target ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ _ ->
    expr >>= \ val ->
    pure (AST.Decl'Value target val)

decl_typesyn :: PEG.Parser AST.Decl
decl_typesyn =
    PEG.consume' "type synonym" (Token.SingleTypeToken Token.Type) >>= \ _ ->
    PEG.consume' "type synonym name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ _ ->
    type_ >>= \ ty ->
    pure (AST.Decl'TypeSyn (Location.Located name_sp name) ty)
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
type_ = PEG.choice [type_iden, type_tuple]

type_iden :: PEG.Parser AST.Type
type_iden =
    PEG.consume' "type" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Identifier (Location.Located iden_sp iden))

type_tuple :: PEG.Parser AST.Type
type_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ _ ->
    PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ _ ->
    pure (AST.Type'Tuple field_types)
-- pattern {{{1
pattern :: PEG.Parser AST.Pattern
pattern = PEG.choice [pattern_tuple, pattern_named, pattern_iden]

pattern_iden :: PEG.Parser AST.Pattern
pattern_iden =
    PEG.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Pattern'Identifier (Location.Located iden_sp iden))

pattern_tuple :: PEG.Parser AST.Pattern
pattern_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ _ ->
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ _ ->
    pure (AST.Pattern'Tuple fields)

pattern_named :: PEG.Parser AST.Pattern
pattern_named =
    PEG.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    PEG.consume' "'@'" (Token.SingleTypeToken Token.At) >>= \ _ ->
    pattern >>= \ more ->
    pure (AST.Pattern'Named (Location.Located iden_sp iden) more)
-- tests {{{1
test_decls :: [TestTree]
test_decls = map Test.run_test $
    let l = Location.dummy_locate
        iden1 t = [l t]
        liden1 = l . iden1
        alpha_iden1 = Token.AlphaIdentifier . iden1
        stt = Token.SingleTypeToken
    in
    [ Test.ParsingTest "binding"
        (Test.make_token_stream [(alpha_iden1 "x"), (stt Token.Equal), (Token.Char 'c')])
        (AST.Decl'Value (AST.Pattern'Identifier (liden1 "x")) (AST.Expr'Char 'c'))
        [("decl", decl), ("decl_binding", decl_binding)]

    , Test.ParsingTest "type synonym"
        (Test.make_token_stream [stt Token.Type, alpha_iden1 "Syn", stt Token.Equal, alpha_iden1 "OtherType"])
        (AST.Decl'TypeSyn (liden1 "Syn") (AST.Type'Identifier $ liden1 "OtherType"))
        [("decl", decl), ("decl_typesyn", decl_typesyn)]

    , Test.ParsingTest "data decl"
        (Test.make_token_stream
            [ (stt Token.Data), (alpha_iden1 "Thingy"), (stt Token.OBrace)

                , (alpha_iden1 "Constr1"), (stt Token.OParen)
                    , (alpha_iden1 "string"), (stt Token.Comma), (alpha_iden1 "int")
                , (stt Token.CParen), (stt Token.Semicolon)

                , (alpha_iden1 "Constr2"), (stt Token.OBrace)
                    , (alpha_iden1 "field1"), (stt Token.Colon), (alpha_iden1 "X"), (stt Token.Semicolon)
                    , (alpha_iden1 "field2"), (stt Token.Colon), (alpha_iden1 "Y"), (stt Token.Semicolon)
                , (stt Token.CBrace), (stt Token.Semicolon)

            , (stt Token.CBrace)
            ])

        (AST.Decl'Data (liden1 "Thingy")
            [ AST.DataVariant'Anon (liden1 "Constr1")
                [ AST.Type'Identifier (liden1 "string")
                , AST.Type'Identifier (liden1 "int")
                ]
            , AST.DataVariant'Named (liden1 "Constr2")
                [ (liden1 "field1", AST.Type'Identifier (liden1 "X"))
                , (liden1 "field2", AST.Type'Identifier (liden1 "Y"))
                ]
            ])
        [("decl", decl), ("decl_data", decl_data)]
    ]

tests :: TestTree
tests = $(testGroupGenerator)
