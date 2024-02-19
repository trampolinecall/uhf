{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- TODO: remove this

module UHF.Parts.Parser
    ( parse

    , Error.Error

    , tests
   ) where

import UHF.Prelude

import qualified Data.InfList as InfList

import UHF.Source.Located (Located (Located))
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.AST as AST
import qualified UHF.Data.Token as Token
import qualified UHF.Source.Located as Located
import qualified UHF.Parts.Parser.Error as Error
import qualified UHF.Parts.Parser.Parser as Parser
-- import qualified UHF.Parts.Parser.Test as Test

-- TODO: write tests
-- TODO: improve parser errors

-- parse {{{1
parse :: [Token.LToken] -> Token.LToken -> Compiler.WithDiagnostics (Located [Error.Error]) Void [AST.Decl]
parse toks eof_tok =
    case Parser.run_parser parse' (toks InfList.+++ InfList.repeat eof_tok) of
        (_, Just (res, _)) -> pure res
        (bt_errors, _) ->
            choose_error bt_errors >>
            pure []
    where
        parse' :: Parser.Parser [AST.Decl]
        parse' = Parser.star (is_tt (Token.EOF ())) decl

        -- TODO: remove duplicate clauses
        choose_error :: [Error.Error] -> Compiler.WithDiagnostics (Located [Error.Error]) Void ()
        choose_error [] = pure ()
        choose_error errs =
            let max_ind = maximum $ map (\ (Error.BadToken ind _ _ _) -> ind) errs -- TODO: make this work
                latest_errors = filter (\ (Error.BadToken ind _ _ _) -> ind == max_ind) errs
                (Error.BadToken _ (Located latest_span _) _ _) = head latest_errors
            in Compiler.tell_error (Located latest_span latest_errors) >> pure ()
-- decl {{{1
decl :: Parser.Parser AST.Decl
decl =
    peek >>= \case
        Token.SingleTypeToken Token.Data -> advance >>= decl_data
        Token.SingleTypeToken Token.TypeSyn -> advance >>= decl_typesyn
        _ -> decl_binding
        -- TODO: improve this error from 'expected a binding' to 'expected a declaration'

decl_data :: Token.LToken -> Parser.Parser AST.Decl
decl_data _ =
    Parser.consume' "datatype name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
    Parser.optional (
        Parser.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>
        Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
        Parser.delim_star
            (
                Parser.consume' "type variable name" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
                pure (Located iden_sp iden)
            )
            (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ vars ->
        Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
        pure vars
    ) >>= \ type_params ->
    Parser.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    Parser.star variant >>= \ variants ->
    Parser.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
    Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'Data (Located name_sp name) (fromMaybe [] type_params) variants)
    where
        variant =
            Parser.consume' "variant name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
            Parser.choice [anon_variant $ Located name_sp name, named_variant $ Located name_sp name]

        anon_variant name =
            Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            Parser.delim_star type_ (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
            Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
            Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
            pure (AST.DataVariant'Anon name field_types)

        named_variant name =
            Parser.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
            Parser.star (
                Parser.consume' "field name" (Token.AlphaIdentifier ()) >>= \ (Located field_name_sp (Token.AlphaIdentifier field_name)) ->
                Parser.consume' "':'" (Token.SingleTypeToken Token.Colon) >>
                type_ >>= \ field_ty ->
                Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
                pure (Located field_name_sp field_name, field_ty)
            ) >>= \ fields ->
            Parser.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
            Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
            pure (AST.DataVariant'Named name fields)

decl_binding :: Parser.Parser AST.Decl
decl_binding =
    pattern >>= \ target ->
    Parser.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ (Located eq_sp _) ->
    expr >>= \ val ->
    Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'Value target eq_sp val)

decl_typesyn :: Parser.Parser AST.Decl
decl_typesyn =
    Parser.consume' "type synonym" (Token.SingleTypeToken Token.TypeSyn) >>
    Parser.consume' "type synonym name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
    Parser.consume' "'='" (Token.SingleTypeToken Token.Equal) >>
    type_ >>= \ ty ->
    Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'TypeSyn (Located name_sp name) ty)
-- expr {{{1
-- TODO: fix precedence
expr :: Parser.Parser AST.Expr
expr = expr_binary_ops

expr_binary_ops :: Parser.Parser AST.Expr
expr_binary_ops =
    expr_call >>= \ first ->
    Parser.star (
        path_or_single_symbol_iden >>= \ op ->
        expr_call >>= \ second ->
        pure (op, second)
    ) >>= \ ops ->
    if null ops
        then pure first
        else pure (AST.Expr'BinaryOps (AST.expr_span first <> AST.expr_span (snd $ last ops)) first ops)

expr_call :: Parser.Parser AST.Expr
expr_call = expr_primary >>= calls
    where
        calls callee =
            peek >>= \ case
                Token.SingleTypeToken Token.OParen ->
                    Parser.delim_star expr (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ args ->
                    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
                    calls (AST.Expr'Call (AST.expr_span callee <> cp_sp) callee args)

                Token.SingleTypeToken Token.Hash ->
                    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
                    Parser.delim_star type_ (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ args ->
                    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
                    calls (AST.Expr'TypeApply (AST.expr_span callee <> cp_sp) callee args)

                _ -> pure callee

expr_primary :: Parser.Parser AST.Expr
expr_primary =
    Parser.choice
        [ expr_parenthesized

        , expr_identifier
        , expr_hole
        , expr_char_lit
        , expr_string_lit
        , expr_int_lit
        , expr_float_lit
        , expr_bool_lit

        , expr_if
        , expr_match

        , expr_forall

        , expr_type_annotation

        , expr_tuple
        , expr_lambda

        , expr_let
        ]

expr_identifier :: Parser.Parser AST.Expr
expr_identifier =
    path_or_single_iden >>= \ (Located sp pi) ->
    pure (AST.Expr'Identifier sp pi)

expr_hole :: Parser.Parser AST.Expr
expr_hole =
    Parser.consume' "'?'" (Token.SingleTypeToken Token.Question) >>= \ (Located question_sp _) ->
    Parser.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Expr'Hole (question_sp <> iden_sp) (Located iden_sp iden))

expr_char_lit :: Parser.Parser AST.Expr
expr_char_lit =
    Parser.consume' "character literal" (Token.Char ()) >>= \ (Located sp (Token.Char ch)) ->
    pure (AST.Expr'Char sp ch)

expr_string_lit :: Parser.Parser AST.Expr
expr_string_lit =
    Parser.consume' "string literal" (Token.String ()) >>= \ (Located sp (Token.String s)) ->
    pure (AST.Expr'String sp s)

expr_int_lit :: Parser.Parser AST.Expr
expr_int_lit =
    Parser.consume' "integer literal" (Token.Int () ()) >>= \ (Located sp (Token.Int _ i)) ->
    pure (AST.Expr'Int sp i)

expr_float_lit :: Parser.Parser AST.Expr
expr_float_lit =
    Parser.consume' "float literal" (Token.Float ()) >>= \ (Located sp (Token.Float f)) ->
    pure (AST.Expr'Float sp f)

expr_bool_lit :: Parser.Parser AST.Expr
expr_bool_lit =
    Parser.consume' "bool literal" (Token.Bool ()) >>= \ (Located sp (Token.Bool b)) ->
    pure (AST.Expr'Bool sp b)

expr_forall :: Parser.Parser AST.Expr
expr_forall =
    Parser.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>= \ (Located sp _) ->
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    Parser.delim_star
        (
            Parser.consume' "type variable name" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
            pure (Located iden_sp iden)
        )
        (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ tys ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    expr >>= \ e ->
    pure (AST.Expr'Forall (sp <> AST.expr_span e) tys e)

expr_parenthesized :: Parser.Parser AST.Expr
expr_parenthesized =
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located _ _) ->
    expr >>= \ e ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located _ _) ->
    pure e

expr_tuple :: Parser.Parser AST.Expr
expr_tuple =
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located o_sp _) ->
    Parser.delim_star expr (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ items ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located c_sp _) ->
    pure (AST.Expr'Tuple (o_sp <> c_sp) items)

expr_lambda :: Parser.Parser AST.Expr
expr_lambda =
    Parser.consume' "'\\'" (Token.SingleTypeToken Token.Backslash) >>= \ (Located backslash_sp _) ->
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    Parser.delim_star pattern (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ params ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    Parser.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
    expr >>= \ body ->
    pure (AST.Expr'Lambda (backslash_sp <> AST.expr_span body) params body)

expr_let :: Parser.Parser AST.Expr
expr_let =
    Parser.choice
        [ Parser.consume' "'let'" (Token.SingleTypeToken Token.Let)
        , Parser.consume' "'letrec'" (Token.SingleTypeToken Token.LetRec)
        ] >>= \ let_tok ->
    Parser.choice
        [ (:[]) <$> decl
        , Parser.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
            Parser.star decl >>= \ decls ->
            Parser.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
            pure decls
        ] >>= \ decls ->
    -- TODO: 'in'?
    expr >>= \ subexpr ->
    case Located.unlocate let_tok of
        Token.SingleTypeToken Token.Let -> pure $ AST.Expr'Let (Located.just_span let_tok <> AST.expr_span subexpr) decls subexpr
        Token.SingleTypeToken Token.LetRec -> pure $ AST.Expr'LetRec (Located.just_span let_tok <> AST.expr_span subexpr) decls subexpr
        _ -> unreachable

expr_if :: Parser.Parser AST.Expr
expr_if =
    Parser.consume' "'if'" (Token.SingleTypeToken Token.If) >>= \ (Located if_sp _) ->
    expr >>= \ cond ->
    Parser.consume' "'then'" (Token.SingleTypeToken Token.Then) >>
    expr >>= \ true_choice ->
    Parser.consume' "'else'" (Token.SingleTypeToken Token.Else) >>
    expr >>= \ false_choice ->
    pure (AST.Expr'If (if_sp <> AST.expr_span false_choice) if_sp cond true_choice false_choice)

expr_match :: Parser.Parser AST.Expr
expr_match =
    Parser.consume' "match" (Token.SingleTypeToken Token.Match) >>= \ (Located match_sp _) ->
    expr >>= \ e ->
    Parser.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    Parser.star (
        pattern >>= \ pat ->
        Parser.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
        expr >>= \ choice ->
        Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
        pure (pat, choice)
    ) >>= \ arms ->
    Parser.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ (Located cb_sp _) ->
    pure (AST.Expr'Match (match_sp <> cb_sp) match_sp e arms)

expr_type_annotation :: Parser.Parser AST.Expr
expr_type_annotation =
    Parser.consume' "':'" (Token.SingleTypeToken Token.Colon) >>= \ (Located colon_sp _) ->
    type_ >>= \ ty ->
    Parser.consume' "':'" (Token.SingleTypeToken Token.Colon) >>
    expr >>= \ e ->
    pure (AST.Expr'TypeAnnotation (colon_sp <> AST.expr_span e) ty e)
-- type {{{1
type_ :: Parser.Parser AST.Type
type_ = type_forall

type_forall :: Parser.Parser AST.Type
type_forall = Parser.choice [forall_, type_function]
    where
        forall_ =
            Parser.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>= \ (Located hash_sp _) ->
            Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            Parser.delim_star (Parser.consume' "type variable" (Token.AlphaIdentifier ()) >>= \ (Located sp (Token.AlphaIdentifier a)) -> pure (Located sp a)) (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ vars ->
            Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
            type_forall >>= \ subty ->
            pure (AST.Type'Forall (hash_sp <> AST.type_span subty) vars subty)

type_function :: Parser.Parser AST.Type
type_function = type_apply >>= m_function
    where
        m_function first = Parser.choice [function first, pure first]
        function arg =
            Parser.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
            type_function >>= \ res ->
            pure (AST.Type'Function (AST.type_span arg <> AST.type_span res) arg res)

type_apply :: Parser.Parser AST.Type
type_apply = type_primary >>= m_applys
    where
        m_applys base = Parser.choice [applys base, get base, pure base]
        applys base =
            Parser.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>
            Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            Parser.delim_star type_ (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ tys ->
            Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
            m_applys (AST.Type'Apply (AST.type_span base <> cp_sp) base tys)
        get base =
            Parser.consume' "'::'" (Token.SingleTypeToken Token.DoubleColon) >>
            Parser.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located next_sp (Token.AlphaIdentifier next)) ->
            m_applys (AST.Type'Get (AST.type_span base <> next_sp) base (Located next_sp next))

type_primary :: Parser.Parser AST.Type
type_primary = Parser.choice [type_parenthesized, type_refer, type_wild, type_hole, type_tuple]

type_refer :: Parser.Parser AST.Type
type_refer =
    Parser.consume' "type" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Refer (Located iden_sp iden))

type_wild :: Parser.Parser AST.Type
type_wild =
    Parser.consume' "'_'" (Token.SingleTypeToken Token.Underscore) >>= \ (Located sp _) ->
    pure (AST.Type'Wild sp)

type_hole :: Parser.Parser AST.Type
type_hole =
    Parser.consume' "'?'" (Token.SingleTypeToken Token.Question) >>= \ (Located question_sp _) ->
    Parser.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Hole (question_sp <> iden_sp) (Located iden_sp iden))

type_parenthesized :: Parser.Parser AST.Type
type_parenthesized =
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    type_ >>= \ ty ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    pure ty

type_tuple :: Parser.Parser AST.Type
type_tuple =
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located op_sp _) ->
    Parser.delim_star type_ (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
    pure (AST.Type'Tuple (op_sp <> cp_sp) field_types)
-- pattern {{{1
pattern :: Parser.Parser AST.Pattern
pattern = Parser.choice [pattern_tuple, pattern_named, pattern_anon_variant, pattern_named_variant, pattern_iden, pattern_wild]

pattern_iden :: Parser.Parser AST.Pattern
pattern_iden =
    Parser.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Pattern'Identifier (Located iden_sp iden))

pattern_anon_variant :: Parser.Parser AST.Pattern
pattern_anon_variant =
    path_or_single_iden >>= \ (Located variant_sp variant) ->
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    Parser.delim_star pattern (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
    pure (AST.Pattern'AnonADTVariant (variant_sp <> cp_sp) variant fields)

pattern_named_variant :: Parser.Parser AST.Pattern
pattern_named_variant =
    path_or_single_iden >>= \ (Located variant_sp variant) ->
    Parser.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    Parser.star (
        Parser.consume' "field name" (Token.AlphaIdentifier ()) >>= \ (Located field_name_sp (Token.AlphaIdentifier field_name)) ->
        Parser.consume' "'='" (Token.SingleTypeToken Token.Equal) >>
        pattern >>= \ field_ty ->
        Parser.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
        pure (Located field_name_sp field_name, field_ty)
    ) >>= \ fields ->
    Parser.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ (Located cp_sp _) ->
    pure (AST.Pattern'NamedADTVariant (variant_sp <> cp_sp) variant fields)

pattern_wild :: Parser.Parser AST.Pattern
pattern_wild =
    Parser.consume' "pattern" (Token.SingleTypeToken Token.Underscore) >>= \ (Located sp _) ->
    pure (AST.Pattern'Wildcard sp)

pattern_tuple :: Parser.Parser AST.Pattern
pattern_tuple =
    Parser.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located o_sp _) ->
    Parser.delim_star pattern (Parser.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    Parser.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located c_sp _) ->
    pure (AST.Pattern'Tuple (o_sp <> c_sp) fields)

pattern_named :: Parser.Parser AST.Pattern
pattern_named =
    Parser.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    Parser.consume' "'@'" (Token.SingleTypeToken Token.At) >>= \ (Located at_sp _) ->
    pattern >>= \ more ->
    pure (AST.Pattern'Named (iden_sp <> AST.pattern_span more) (Located iden_sp iden) at_sp more)
-- utilities {{{1
path_or_single_iden :: Parser.Parser (Located AST.PathOrSingleIden)
path_or_single_iden = Parser.choice [path, single_iden]
    where
        -- a hack to make paths work: let the type parse the path and then afterwards, pick out the topmost Get portion and turn that into a AST.PathOrSingleIden'Path node
        path = do
            ty <- type_
            case ty of
                AST.Type'Get whole_span ty (Located next_sp next) ->
                    pure (Located whole_span (AST.PathOrSingleIden'Path ty (Located next_sp next)))
                _ -> Parser.fail (Error.NotAPath (AST.type_span ty))
        single_iden =
            Parser.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
            pure (Located iden_sp (AST.PathOrSingleIden'Single (Located iden_sp iden)))

path_or_single_symbol_iden :: Parser.Parser (Located AST.PathOrSingleIden)
path_or_single_symbol_iden = Parser.choice [path, single_iden]
    where
        -- this does not need the hack from above because ::<symbol identifier> wont be consumed by type_
        path =
            type_ >>= \ ty ->
            Parser.consume' "'::'" (Token.SingleTypeToken Token.DoubleColon) >>
            Parser.consume' "symbol identifier" (Token.SymbolIdentifier ()) >>= \ (Located next_sp (Token.SymbolIdentifier next)) ->
            pure (Located (AST.type_span ty <> next_sp) (AST.PathOrSingleIden'Path ty (Located next_sp next)))
        single_iden =
            Parser.consume' "operator" (Token.SymbolIdentifier ()) >>= \ (Located iden_sp (Token.SymbolIdentifier iden)) ->
            pure (Located iden_sp (AST.PathOrSingleIden'Single (Located iden_sp iden)))

-- tests {{{1
{- TODO
-- test_decls :: [TestTree]
-- test_decls = map Test.run_test $
    let dsp = Span.dummy
        l = Located.dummy_locate
        iden1 t = [l t]
        liden1 = l . iden1
        alpha_iden1 = Token.AlphaIdentifier . iden1
        stt = Token.SingleTypeToken
    in
    [ Test.ParsingTest "binding"
        (Test.make_token_stream [(alpha_iden1 "x"), (stt Token.Equal), (Token.Char 'c')])
        (AST.Decl'Value (AST.Pattern'Identifier (liden1 "x")) dsp (AST.Expr'Char dsp 'c'))
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
-}

tests :: TestTree
tests = $(testGroupGenerator)
