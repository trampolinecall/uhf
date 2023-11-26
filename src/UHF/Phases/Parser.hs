{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- TODO: remove this

module UHF.Phases.Parser
    ( parse

    , Error.Error

    , tests
   ) where

import UHF.Util.Prelude

import qualified UHF.Phases.Parser.PEG as PEG
import qualified UHF.Phases.Parser.Error as Error
-- import qualified UHF.Phases.Parser.Test as Test

import qualified UHF.Data.Token as Token
import qualified UHF.Data.AST as AST

import qualified Data.InfList as InfList

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located (Located))

import qualified UHF.Compiler as Compiler

-- TODO: write tests
-- TODO: improve parser errors

-- parse {{{1
parse :: [Token.LToken] -> Token.LToken -> Compiler.WithDiagnostics (Located [Error.Error]) Void [AST.Decl]
parse toks eof_tok =
    case PEG.run_parser parse' (InfList.zip (InfList.iterate (1+) 0) (toks InfList.+++ InfList.repeat eof_tok)) of
        (_, Just (res, _)) -> pure res
        (bt_errors, _) ->
            choose_error bt_errors >>
            pure []
    where
        parse' :: PEG.Parser [AST.Decl]
        parse' = PEG.star decl >>= \ ds -> PEG.consume' "end of file" (Token.EOF ()) >> pure ds

        -- TODO: remove duplicate clauses
        choose_error :: [Error.Error] -> Compiler.WithDiagnostics (Located [Error.Error]) Void ()
        choose_error [] = pure ()
        choose_error errs =
            let max_ind = maximum $ map (\ (Error.BadToken ind _ _ _) -> ind) errs
                latest_errors = filter (\ (Error.BadToken ind _ _ _) -> ind == max_ind) errs
                (Error.BadToken _ (Located latest_span _) _ _) = head latest_errors
            in Compiler.tell_error (Located latest_span latest_errors) >> pure ()
-- decl {{{1
decl :: PEG.Parser AST.Decl
decl =
    PEG.choice
        [ decl_data
        , decl_typesyn
        , decl_binding
        ]

decl_data :: PEG.Parser AST.Decl
decl_data =
    PEG.consume' "data declaration" (Token.SingleTypeToken Token.Data) >>
    PEG.consume' "datatype name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.optional (
        PEG.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>
        PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
        PEG.delim_star
            (
                PEG.consume' "type variable name" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
                pure (Located iden_sp iden)
            )
            (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ vars ->
        PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
        pure vars
    ) >>= \ type_params ->
    PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    PEG.star variant >>= \ variants ->
    PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'Data (Located name_sp name) (fromMaybe [] type_params) variants)
    where
        variant =
            PEG.consume' "variant name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
            PEG.choice [anon_variant $ Located name_sp name, named_variant $ Located name_sp name]

        anon_variant name =
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
            PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
            pure (AST.DataVariant'Anon name field_types)

        named_variant name =
            PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
            PEG.star (
                PEG.consume' "field name" (Token.AlphaIdentifier ()) >>= \ (Located field_name_sp (Token.AlphaIdentifier field_name)) ->
                PEG.consume' "':'" (Token.SingleTypeToken Token.Colon) >>
                type_ >>= \ field_ty ->
                PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
                pure (Located field_name_sp field_name, field_ty)
            ) >>= \ fields ->
            PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>
            PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
            pure (AST.DataVariant'Named name fields)

decl_binding :: PEG.Parser AST.Decl
decl_binding =
    pattern >>= \ target ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>= \ (Located eq_sp _) ->
    expr >>= \ val ->
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'Value target eq_sp val)

decl_typesyn :: PEG.Parser AST.Decl
decl_typesyn =
    PEG.consume' "type synonym" (Token.SingleTypeToken Token.TypeSyn) >>
    PEG.consume' "type synonym name" (Token.AlphaIdentifier ()) >>= \ (Located name_sp (Token.AlphaIdentifier name)) ->
    PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>
    type_ >>= \ ty ->
    PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
    pure (AST.Decl'TypeSyn (Located name_sp name) ty)
-- expr {{{1
-- TODO: fix precedence
expr :: PEG.Parser AST.Expr
expr = expr_binary_ops

expr_binary_ops :: PEG.Parser AST.Expr
expr_binary_ops =
    expr_call >>= \ first ->
    PEG.star (
        path_or_single_symbol_iden >>= \ op ->
        expr_call >>= \ second ->
        pure (op, second)
    ) >>= \ ops ->
    if null ops
        then pure first
        else pure (AST.Expr'BinaryOps (AST.expr_span first <> AST.expr_span (snd $ last ops)) first ops)

expr_call :: PEG.Parser AST.Expr
expr_call = expr_primary >>= calls
    where
        calls callee =
            PEG.choice [normal_call callee, type_application callee, pure callee]

        normal_call callee =
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            PEG.delim_star expr (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ args ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
            calls (AST.Expr'Call (AST.expr_span callee <> cp_sp) callee args)

        type_application callee =
            PEG.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ args ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
            calls (AST.Expr'TypeApply (AST.expr_span callee <> cp_sp) callee args)

expr_primary :: PEG.Parser AST.Expr
expr_primary =
    PEG.choice
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

expr_identifier :: PEG.Parser AST.Expr
expr_identifier =
    path_or_single_iden >>= \ (Located sp pi) ->
    pure (AST.Expr'Identifier sp pi)

expr_hole :: PEG.Parser AST.Expr
expr_hole =
    PEG.consume' "'?'" (Token.SingleTypeToken Token.Question) >>= \ (Located question_sp _) ->
    PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Expr'Hole (question_sp <> iden_sp) (Located iden_sp iden))

expr_char_lit :: PEG.Parser AST.Expr
expr_char_lit =
    PEG.consume' "character literal" (Token.Char ()) >>= \ (Located sp (Token.Char ch)) ->
    pure (AST.Expr'Char sp ch)

expr_string_lit :: PEG.Parser AST.Expr
expr_string_lit =
    PEG.consume' "string literal" (Token.String ()) >>= \ (Located sp (Token.String s)) ->
    pure (AST.Expr'String sp s)

expr_int_lit :: PEG.Parser AST.Expr
expr_int_lit =
    PEG.consume' "integer literal" (Token.Int () ()) >>= \ (Located sp (Token.Int _ i)) ->
    pure (AST.Expr'Int sp i)

expr_float_lit :: PEG.Parser AST.Expr
expr_float_lit =
    PEG.consume' "float literal" (Token.Float ()) >>= \ (Located sp (Token.Float f)) ->
    pure (AST.Expr'Float sp f)

expr_bool_lit :: PEG.Parser AST.Expr
expr_bool_lit =
    PEG.consume' "bool literal" (Token.Bool ()) >>= \ (Located sp (Token.Bool b)) ->
    pure (AST.Expr'Bool sp b)

expr_forall :: PEG.Parser AST.Expr
expr_forall =
    PEG.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>= \ (Located sp _) ->
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    PEG.delim_star
        (
            PEG.consume' "type variable name" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
            pure (Located iden_sp iden)
        )
        (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ tys ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    expr >>= \ e ->
    pure (AST.Expr'Forall (sp <> AST.expr_span e) tys e)

expr_parenthesized :: PEG.Parser AST.Expr
expr_parenthesized =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located _ _) ->
    expr >>= \ e ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located _ _) ->
    pure e

expr_tuple :: PEG.Parser AST.Expr
expr_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located o_sp _) ->
    PEG.delim_star expr (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ items ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located c_sp _) ->
    pure (AST.Expr'Tuple (o_sp <> c_sp) items)

expr_lambda :: PEG.Parser AST.Expr
expr_lambda =
    PEG.consume' "'\\'" (Token.SingleTypeToken Token.Backslash) >>= \ (Located backslash_sp _) ->
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ params ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    PEG.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
    expr >>= \ body ->
    pure (AST.Expr'Lambda (backslash_sp <> AST.expr_span body) params body)

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
    case Located.unlocate let_tok of
        Token.SingleTypeToken Token.Let -> pure $ AST.Expr'Let (Located.just_span let_tok <> AST.expr_span subexpr) decls subexpr
        Token.SingleTypeToken Token.LetRec -> pure $ AST.Expr'LetRec (Located.just_span let_tok <> AST.expr_span subexpr) decls subexpr
        _ -> unreachable

expr_if :: PEG.Parser AST.Expr
expr_if =
    PEG.consume' "'if'" (Token.SingleTypeToken Token.If) >>= \ (Located if_sp _) ->
    expr >>= \ cond ->
    PEG.consume' "'then'" (Token.SingleTypeToken Token.Then) >>
    expr >>= \ true_choice ->
    PEG.consume' "'else'" (Token.SingleTypeToken Token.Else) >>
    expr >>= \ false_choice ->
    pure (AST.Expr'If (if_sp <> AST.expr_span false_choice) if_sp cond true_choice false_choice)

expr_match :: PEG.Parser AST.Expr
expr_match =
    PEG.consume' "match" (Token.SingleTypeToken Token.Match) >>= \ (Located match_sp _) ->
    expr >>= \ e ->
    PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    PEG.star (
        pattern >>= \ pat ->
        PEG.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
        expr >>= \ choice ->
        PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
        pure (pat, choice)
    ) >>= \ arms ->
    PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ (Located cb_sp _) ->
    pure (AST.Expr'Match (match_sp <> cb_sp) match_sp e arms)

expr_type_annotation :: PEG.Parser AST.Expr
expr_type_annotation =
    PEG.consume' "':'" (Token.SingleTypeToken Token.Colon) >>= \ (Located colon_sp _) ->
    type_ >>= \ ty ->
    PEG.consume' "':'" (Token.SingleTypeToken Token.Colon) >>
    expr >>= \ e ->
    pure (AST.Expr'TypeAnnotation (colon_sp <> AST.expr_span e) ty e)
-- type {{{1
type_ :: PEG.Parser AST.Type
type_ = type_forall

type_forall :: PEG.Parser AST.Type
type_forall = PEG.choice [forall, type_function]
    where
        forall =
            PEG.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>= \ (Located hash_sp _) ->
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            PEG.delim_star (PEG.consume' "type variable" (Token.AlphaIdentifier ()) >>= \ (Located sp (Token.AlphaIdentifier a)) -> pure (Located sp a)) (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ vars ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
            type_forall >>= \ subty ->
            pure (AST.Type'Forall (hash_sp <> AST.type_span subty) vars subty)

type_function :: PEG.Parser AST.Type
type_function = type_apply >>= m_function
    where
        m_function first = PEG.choice [function first, pure first]
        function arg =
            PEG.consume' "'->'" (Token.SingleTypeToken Token.Arrow) >>
            type_function >>= \ res ->
            pure (AST.Type'Function (AST.type_span arg <> AST.type_span res) arg res)

type_apply :: PEG.Parser AST.Type
type_apply = type_primary >>= m_applys
    where
        m_applys base = PEG.choice [applys base, get base, pure base]
        applys base =
            PEG.consume' "'#'" (Token.SingleTypeToken Token.Hash) >>
            PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
            PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ tys ->
            PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
            m_applys (AST.Type'Apply (AST.type_span base <> cp_sp) base tys)
        get base =
            PEG.consume' "'::'" (Token.SingleTypeToken Token.DoubleColon) >>= \ _ ->
            PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located next_sp (Token.AlphaIdentifier next)) ->
            m_applys (AST.Type'Get (AST.type_span base <> next_sp) base (Located next_sp next))

type_primary :: PEG.Parser AST.Type
type_primary = PEG.choice [type_parenthesized, type_refer, type_wild, type_hole, type_tuple]

type_refer :: PEG.Parser AST.Type
type_refer =
    PEG.consume' "type" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Refer (Located iden_sp iden))

type_wild :: PEG.Parser AST.Type
type_wild =
    PEG.consume' "'_'" (Token.SingleTypeToken Token.Underscore) >>= \ (Located sp _) ->
    pure (AST.Type'Wild sp)

type_hole :: PEG.Parser AST.Type
type_hole =
    PEG.consume' "'?'" (Token.SingleTypeToken Token.Question) >>= \ (Located question_sp _) ->
    PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Type'Hole (question_sp <> iden_sp) (Located iden_sp iden))

type_parenthesized :: PEG.Parser AST.Type
type_parenthesized =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    type_ >>= \ ty ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>
    pure ty

type_tuple :: PEG.Parser AST.Type
type_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located op_sp _) ->
    PEG.delim_star type_ (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ field_types ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
    pure (AST.Type'Tuple (op_sp <> cp_sp) field_types)
-- pattern {{{1
pattern :: PEG.Parser AST.Pattern
pattern = PEG.choice [pattern_tuple, pattern_named, pattern_anon_variant, pattern_named_variant, pattern_iden, pattern_wild]

pattern_iden :: PEG.Parser AST.Pattern
pattern_iden =
    PEG.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    pure (AST.Pattern'Identifier (Located iden_sp iden))

pattern_anon_variant :: PEG.Parser AST.Pattern
pattern_anon_variant =
    path_or_single_iden >>= \ (Located variant_sp variant) ->
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located cp_sp _) ->
    pure (AST.Pattern'AnonADTVariant (variant_sp <> cp_sp) variant fields)

pattern_named_variant :: PEG.Parser AST.Pattern
pattern_named_variant =
    path_or_single_iden >>= \ (Located variant_sp variant) ->
    PEG.consume' "'{'" (Token.SingleTypeToken Token.OBrace) >>
    PEG.star (
        PEG.consume' "field name" (Token.AlphaIdentifier ()) >>= \ (Located field_name_sp (Token.AlphaIdentifier field_name)) ->
        PEG.consume' "'='" (Token.SingleTypeToken Token.Equal) >>
        pattern >>= \ field_ty ->
        PEG.consume' "';'" (Token.SingleTypeToken Token.Semicolon) >>
        pure (Located field_name_sp field_name, field_ty)
    ) >>= \ fields ->
    PEG.consume' "'}'" (Token.SingleTypeToken Token.CBrace) >>= \ (Located cp_sp _) ->
    pure (AST.Pattern'NamedADTVariant (variant_sp <> cp_sp) variant fields)

pattern_wild :: PEG.Parser AST.Pattern
pattern_wild =
    PEG.consume' "pattern" (Token.SingleTypeToken Token.Underscore) >>= \ (Located sp _) ->
    pure (AST.Pattern'Wildcard sp)

pattern_tuple :: PEG.Parser AST.Pattern
pattern_tuple =
    PEG.consume' "'('" (Token.SingleTypeToken Token.OParen) >>= \ (Located o_sp _) ->
    PEG.delim_star pattern (PEG.consume' "','" (Token.SingleTypeToken Token.Comma)) >>= \ fields ->
    PEG.consume' "')'" (Token.SingleTypeToken Token.CParen) >>= \ (Located c_sp _) ->
    pure (AST.Pattern'Tuple (o_sp <> c_sp) fields)

pattern_named :: PEG.Parser AST.Pattern
pattern_named =
    PEG.consume' "pattern" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
    PEG.consume' "'@'" (Token.SingleTypeToken Token.At) >>= \ (Located at_sp _) ->
    pattern >>= \ more ->
    pure (AST.Pattern'Named (iden_sp <> AST.pattern_span more) (Located iden_sp iden) at_sp more)
-- utilities {{{1
path_or_single_iden :: PEG.Parser (Located AST.PathOrSingleIden)
path_or_single_iden = PEG.choice [path, single_iden]
    where
        path =
            type_ >>= \ ty ->
            PEG.consume' "'::'" (Token.SingleTypeToken Token.DoubleColon) >>= \ _ ->
            PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located next_sp (Token.AlphaIdentifier next)) ->
            pure (Located (AST.type_span ty <> next_sp) (AST.PathOrSingleIden'Path ty (Located next_sp next)))
        single_iden =
            PEG.consume' "identifier" (Token.AlphaIdentifier ()) >>= \ (Located iden_sp (Token.AlphaIdentifier iden)) ->
            pure (Located iden_sp (AST.PathOrSingleIden'Single (Located iden_sp iden)))

path_or_single_symbol_iden :: PEG.Parser (Located AST.PathOrSingleIden)
path_or_single_symbol_iden = PEG.choice [path, single_iden]
    where
        path =
            type_ >>= \ ty ->
            PEG.consume' "'::'" (Token.SingleTypeToken Token.DoubleColon) >>= \ _ ->
            PEG.consume' "symbol identifier" (Token.SymbolIdentifier ()) >>= \ (Located next_sp (Token.SymbolIdentifier next)) ->
            pure (Located (AST.type_span ty <> next_sp) (AST.PathOrSingleIden'Path ty (Located next_sp next)))
        single_iden =
            PEG.consume' "operator" (Token.SymbolIdentifier ()) >>= \ (Located iden_sp (Token.SymbolIdentifier iden)) ->
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
