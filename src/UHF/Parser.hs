{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser
    ( parse

    , Error.BacktrackingError -- TODO: rename to just Error

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

-- TODO: write tests

-- parse {{{1
parse :: [Token.LToken] -> Token.LToken -> (Maybe (Location.Located [Error.BacktrackingError]), [AST.Decl])
parse toks eof_tok =
    case PEG.run_parser parse' (InfList.zip (InfList.iterate (1+) 0) (toks InfList.+++ InfList.repeat eof_tok)) of
        (_, Just (res, _)) -> (Nothing, res)
        (bt_errors, _) -> (choose_error bt_errors, [])
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
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
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
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
    pure (AST.Decl'Value target val)

decl_typesyn :: PEG.Parser AST.Decl
decl_typesyn =
    PEG.consume' "type synonym" (Token.SingleTypeToken Token.Type) >>= \ _ ->
    PEG.consume' "type synonym name" (Token.AlphaIdentifier ()) >>= \ (Location.Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ _ ->
    type_ >>= \ ty ->
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>= \ _ ->
    pure (AST.Decl'TypeSyn (Location.Located name_sp name) ty)
-- expr {{{1
expr :: PEG.Parser AST.Expr
expr =
    PEG.choice
        [ expr_call
        , expr_binary_ops
        ]

expr_primary :: PEG.Parser AST.Expr
expr_primary =
    PEG.choice
        [ expr_identifier
        , expr_char_lit
        , expr_string_lit
        , expr_int_lit
        , expr_float_lit
        , expr_bool_lit

        , expr_if
        , expr_case

        , expr_type_annotation

        , expr_tuple
        , expr_lambda

        , expr_let
        ]

expr_identifier :: PEG.Parser AST.Expr
expr_identifier =
    PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Expr'Identifier (Location.Located iden_sp iden))

expr_char_lit :: PEG.Parser AST.Expr
expr_char_lit =
    PEG.consume' "character literal" (Token.Char ()) >>= \ (Location.Located sp (Token.Char ch)) ->
    pure (AST.Expr'Char sp ch)

expr_string_lit :: PEG.Parser AST.Expr
expr_string_lit =
    PEG.consume' "string literal" (Token.String ()) >>= \ (Location.Located sp (Token.String s)) ->
    pure (AST.Expr'String sp s)

expr_int_lit :: PEG.Parser AST.Expr
expr_int_lit =
    PEG.consume' "integer literal" (Token.Int () ()) >>= \ (Location.Located sp (Token.Int _ i)) ->
    pure (AST.Expr'Int sp i)

expr_float_lit :: PEG.Parser AST.Expr
expr_float_lit =
    PEG.consume' "float literal" (Token.Float ()) >>= \ (Location.Located sp (Token.Float f)) ->
    pure (AST.Expr'Float sp f)

expr_bool_lit :: PEG.Parser AST.Expr
expr_bool_lit =
    PEG.consume' "bool literal" (Token.Bool ()) >>= \ (Location.Located sp (Token.Bool b)) ->
    pure (AST.Expr'Bool sp b)

expr_tuple :: PEG.Parser AST.Expr
expr_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Location.Located o_sp _) ->
    PEG.delim_star expr (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ items -> -- TODO: parenthesized expressions too
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Location.Located c_sp _) ->
    pure (AST.Expr'Tuple (o_sp `Location.join_span` c_sp) items)

expr_lambda :: PEG.Parser AST.Expr
expr_lambda =
    PEG.consume' "'\\'" (Token.SingleTypeToken Token.Backslash) >>= \ (Location.Located backslash_sp _) ->
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ _ ->
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ params ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ _ ->
    PEG.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>= \ _ ->
    expr >>= \ body ->
    pure (AST.Expr'Lambda (backslash_sp `Location.join_span` todo) params body)

expr_let :: PEG.Parser AST.Expr
expr_let =
    PEG.choice
        [ PEG.consume' "'let'" (Token.SingleTypeToken Token.Let)
        , PEG.consume' "'letrec'" (Token.SingleTypeToken Token.LetRec)
        ] >>= \ let_tok ->
    PEG.choice
        [ (:[]) <$> decl
        , PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
            PEG.star decl >>= \ decls ->
            PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
            pure decls
        ] >>= \ decls ->
    -- TODO: 'in'?
    expr >>= \ subexpr ->
    case Location.unlocate let_tok of
        Token.SingleTypeToken Token.Let -> pure $ AST.Expr'Let (Location.just_span let_tok `Location.join_span` todo) decls subexpr
        Token.SingleTypeToken Token.LetRec -> pure $ AST.Expr'LetRec (Location.just_span let_tok `Location.join_span` todo) decls subexpr
        _ -> unreachable

expr_binary_ops :: PEG.Parser AST.Expr
expr_binary_ops =
    expr_primary >>= \ first ->
    PEG.star (
        PEG.consume' "operator" (Token.SymbolIdentifier ()) >>= \ (Location.Located op_sp (Token.SymbolIdentifier op)) ->
        expr_primary >>= \ second ->
        pure (Location.Located op_sp op, second)
    ) >>= \ ops ->
    if null ops
        then pure first
        else pure (AST.Expr'BinaryOps todo first ops)

expr_call :: PEG.Parser AST.Expr
expr_call =
    expr_primary >>= \ callee ->
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ _ ->
    PEG.delim_star expr (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ args ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ _ ->
    pure (AST.Expr'Call (todo `Location.join_span` todo) callee args)

expr_if :: PEG.Parser AST.Expr
expr_if =
    PEG.consume' "'if'" (Token.SingleTypeToken Token.If) >>= \ (Location.Located if_sp _) ->
    expr >>= \ cond ->
    PEG.consume' "'then'" (Token.SingleTypeToken Token.Then) >>= \ _ ->
    expr >>= \ true_choice ->
    PEG.consume' "'else'" (Token.SingleTypeToken Token.Else) >>= \ _ ->
    expr >>= \ false_choice ->
    pure (AST.Expr'If (if_sp `Location.join_span` todo) cond true_choice false_choice)

expr_case :: PEG.Parser AST.Expr
expr_case =
    PEG.consume' "'case'" (Token.SingleTypeToken Token.Case) >>= \ _ ->
    expr >>= \ e ->
    PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    PEG.star (
        pattern >>= \ pat ->
        PEG.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>= \ _ ->
        expr >>= \ choice ->
        pure (pat, choice)
    ) >>= \ arms ->
    PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
    pure (AST.Expr'Case todo e arms)

expr_type_annotation :: PEG.Parser AST.Expr
expr_type_annotation =
    PEG.consume' "':'" (Token.SingleTypeToken Token.Colon) >>= \ (Location.Located colon_sp _) ->
    type_ >>= \ ty -> -- TODO: this probably needs a delimiter because when type applications types can go on
    expr >>= \ e ->
    pure (AST.Expr'TypeAnnotation (colon_sp `Location.join_span` todo) ty e)
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
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Location.Located o_sp _) ->
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Location.Located c_sp _) ->
    pure (AST.Pattern'Tuple (o_sp `Location.join_span` c_sp) fields)

pattern_named :: PEG.Parser AST.Pattern
pattern_named =
    PEG.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Location.Located iden_sp (Token.AlphaIdentifier iden)) ->
    PEG.consume' "'@'" (Token.SingleTypeToken Token.At) >>= \ _ ->
    pattern >>= \ more ->
    pure (AST.Pattern'Named (iden_sp `Location.join_span` todo) (Location.Located iden_sp iden) more)
-- tests {{{1
test_decls :: [TestTree]
test_decls = map Test.run_test $
    let dsp = Location.dummy_span
        l = Location.dummy_locate
        iden1 t = [l t]
        liden1 = l . iden1
        alpha_iden1 = Token.AlphaIdentifier . iden1
        stt = Token.SingleTypeToken
    in
    [ Test.ParsingTest "binding"
        (Test.make_token_stream [(alpha_iden1 "x"), (stt Token.Equal), (Token.Char 'c')])
        (AST.Decl'Value (AST.Pattern'Identifier (liden1 "x")) (AST.Expr'Char dsp 'c'))
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
