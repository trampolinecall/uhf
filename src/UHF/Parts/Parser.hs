{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file -fshow-error-context #-}

module UHF.Parts.Parser
    ( parse
    , Error.Error
    ) where

import UHF.Prelude

import qualified Data.InfList as InfList

import Data.Maybe (maybeToList)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.AST as AST
import UHF.Data.Token (BaseToken (..), SingleTypeToken (..))
import qualified UHF.Data.Token as Token
import qualified UHF.Parts.Parser.Error as Error
import qualified UHF.Parts.Parser.Generate as Generate
import UHF.Parts.Parser.Grammar
import UHF.Source.Located (Located (Located))
import qualified UHF.Source.Located as Located

-- TODO: improve parser errors

$( let unwrap_right :: Show a => Either a b -> b
       unwrap_right (Left a) = error $ "unwrap_right on Left " ++ show a
       unwrap_right (Right a) = a
   in Generate.make_parse_fn
        "parse'"
        [t|[AST.Decl]|]
        ( Generate.generate_table $ unwrap_right $ make_grammar $ do
            let (.) :: (ToSymbol a, ToSymbol b) => a -> b -> [Symbol]
                (.) = prod_join
                infixr 6 .

                empty :: [Symbol]
                empty = []

                p |> m = (to_symbol p, m)
                infix 4 |>

                -- TODO: rename to star list
                list :: Nonterminal -> GrammarMonad Nonterminal
                list Augment = error "cannot make list of augment"
                list thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    thing_list <- nt ("list of " <> name) [t|[$thing_ty]|]

                    thing_list --> thing_list . thing |> [|\l t -> l ++ [t]|]
                    thing_list --> empty |> [|[]|]

                    pure thing_list

                list_min_1 :: Nonterminal -> GrammarMonad Nonterminal
                list_min_1 Augment = error "cannot make list of augment"
                list_min_1 thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    thing_list <- nt ("list of " <> name) [t|[$thing_ty]|]

                    thing_list --> thing_list . thing |> [|\l t -> l ++ [t]|]
                    thing_list --> thing |> [|\t -> [t]|]

                    pure thing_list

                optional :: Nonterminal -> GrammarMonad Nonterminal
                optional Augment = error "cannot make optional augment"
                optional thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    opt <- nt ("optional " <> name) [t|Maybe $thing_ty|]

                    opt --> thing |> [|Just|]
                    opt --> empty |> [|Nothing|]

                    pure opt

                sep_list_allow_trailing :: ToSymbol sep => sep -> Nonterminal -> GrammarMonad Nonterminal
                sep_list_allow_trailing _ Augment = error "cannot make separated list of augment"
                sep_list_allow_trailing sep thing@(Nonterminal name) = do
                    thing_ty <- get_nt_ty thing

                    list <- nt ("delimited list of " <> name) [t|[$thing_ty]|]
                    helper_list <- nt ("delimited list of " <> name <> " helper") [t|[$thing_ty]|]

                    list --> helper_list |> [|identity|]
                    list --> helper_list . sep |> [|\l _ -> l|]
                    list --> empty |> [|[]|]

                    helper_list --> helper_list . sep . thing |> [|\l _ t -> l ++ [t]|]
                    helper_list --> thing |> [|\x -> [x]|]

                    pure list

            -- TODO: rename all nonterminals
            decl <- nt "decl" [t|AST.Decl|]
            decl_list <- list decl >>= toplevel

            decl_data <- nt "decl_data" [t|AST.Decl|]
            data_variant <- nt "data_variant" [t|AST.DataVariant|]
            data_variant_list <- list data_variant

            decl_typesyn <- nt "decl_typesyn" [t|AST.Decl|]
            decl_binding <- nt "decl_binding" [t|AST.Decl|]

            type_ <- nt "type" [t|AST.Type|]
            type_forall <- nt "forall type" [t|AST.Type|]
            type_function <- nt "function type" [t|AST.Type|]
            type_apply_or_get <- nt "apply or get type" [t|AST.Type|]
            type_primary <- nt "primary type" [t|AST.Type|]

            expr <- nt "expr" [t|AST.Expr|]
            expr_toplevel <- nt "toplevel expr" [t|AST.Expr|]
            expr_forall <- nt "forall expr" [t|AST.Expr|]
            expr_let <- nt "let expr" [t|AST.Expr|]
            expr_if <- nt "if expression" [t|AST.Expr|]
            expr_type_annotation <- nt "type annotation expression" [t|AST.Expr|]
            expr_lambda <- nt "lambda expression" [t|AST.Expr|]
            expr_keyword_call <- nt "keyword call expression" [t|AST.Expr|]
            expr_binary_ops <- nt "binary ops expression" [t|AST.Expr|]
            expr_call <- nt "call expression" [t|AST.Expr|]
            expr_primary <- nt "primary expression" [t|AST.Expr|]
            expr_identifier <- nt "identifier expression" [t|AST.Expr|]
            expr_hole <- nt "hole expression" [t|AST.Expr|]
            expr_literal <- nt "literal expression" [t|AST.Expr|]
            expr_match <- nt "match expression" [t|AST.Expr|]
            expr_tuple <- nt "tuple expression" [t|AST.Expr|]

            operator <- nt "operator" [t|AST.PathOrSingleIden|]

            pattern <- nt "pattern" [t|AST.Pattern|]
            pattern_anon_variant <- nt "anonymous variant pattern" [t|AST.Pattern|]
            pattern_iden <- nt "identifier pattern" [t|AST.Pattern|]
            pattern_named <- nt "named pattern" [t|AST.Pattern|] -- TODO: rename this to at-pattern?
            pattern_named_variant <- nt "named variant pattern" [t|AST.Pattern|]
            pattern_primary <- nt "primary pattern" [t|AST.Pattern|]
            pattern_toplevel <- nt "toplevel pattern" [t|AST.Pattern|]
            pattern_tuple <- nt "tuple pattern" [t|AST.Pattern|]
            pattern_wild <- nt "wild pattern" [t|AST.Pattern|]

            aiden <- nt "alpha iden" [t|AST.Identifier|]

            type_param_list <- nt "type parameter list" [t|[AST.Identifier]|] -- TODO: rename this?
            comma_sep_expr_list <- sep_list_allow_trailing Comma expr
            comma_sep_type_list <- sep_list_allow_trailing Comma type_
            comma_sep_pattern_list <- sep_list_allow_trailing Comma pattern

            comma_sep_expr_list_at_least_one_comma <- nt "comma separated expression list with at least one comma" [t|[AST.Expr]|]
            comma_sep_type_list_at_least_one_comma <- nt "comma separated type list with at least one comma" [t|[AST.Type]|]
            comma_sep_pattern_list_at_least_one_comma <- nt "comma separated pattern list with at least one comma" [t|[AST.Pattern]|]

            alpha_iden_path <- nt "alpha identifier path" [t|AST.PathOrSingleIden|] -- TODO: change this
            kw_iden_path <- nt "keyword identifier path" [t|AST.PathOrSingleIden|] -- TODO: also probably change this
            symbol_iden_path <- nt "symbol identifier path" [t|AST.PathOrSingleIden|] -- TODO: also probably change this
            kw_iden_paths <- nt "keyword identifer paths" [t|[AST.PathOrSingleIden]|]
            optional_kw_iden_paths <- nt "optional keyword identifier paths" [t|Maybe [AST.PathOrSingleIden]|]
            keyword_call_args <- nt "keyword call args" [t|[Either AST.PathOrSingleIden AST.Expr]|] -- TODO: this is a bad name and also reorganize this and also probably dont use Either
            pattern_named_list_at_least_once <- nt "named pattern list" [t|[AST.Pattern]|]

            decl --> decl_data |> [|identity|]
            decl --> decl_typesyn |> [|identity|]
            decl --> decl_binding |> [|identity|]

            -- TODO: optional type params
            decl_data
                --> (Data . aiden . type_param_list . OBrace . data_variant_list . CBrace . Semicolon)
                |> [|\_ name typarams _ variants _ _ -> AST.Decl'Data name typarams variants|]

            -- TODO: fields
            -- TODO: redesign fields to better match function application syntax
            do
                anon_field <- nt "data declaration anonymous field" [t|AST.Type|]
                field_list <- sep_list_allow_trailing Comma anon_field

                anon_field --> type_ |> [|identity|]
                data_variant --> (aiden . OParen . field_list . CParen . Semicolon) |> [|\name _ fields _ _ -> AST.DataVariant'Anon name fields|]

            do
                named_field <- nt "data declaration named field" [t|(AST.Identifier, AST.Type)|]
                field_list <- sep_list_allow_trailing Comma named_field

                named_field --> aiden . Colon . type_ |> [|\a _ t -> (a, t)|]
                data_variant --> (aiden . OBrace . field_list . CBrace . Semicolon) |> [|\name _ fields _ _ -> AST.DataVariant'Named name fields|]

            decl_typesyn --> (TypeSyn . aiden . Equal . type_ . Semicolon) |> [|\_ name _ ty _ -> AST.Decl'TypeSyn name ty|]

            decl_binding --> (pattern . Equal . expr . Semicolon) |> [|\p (Located eq _) e _ -> AST.Decl'Value p eq e|]

            expr --> expr_toplevel |> [|identity|]

            expr_toplevel --> expr_forall |> [|identity|]
            expr_toplevel --> expr_let |> [|identity|]
            expr_toplevel --> expr_if |> [|identity|]
            expr_toplevel --> expr_type_annotation |> [|identity|]
            expr_toplevel --> expr_lambda |> [|identity|]
            expr_toplevel --> expr_keyword_call |> [|identity|]

            expr_forall --> Hash . aiden . expr_toplevel |> [|\(Located h_sp _) tv e -> AST.Expr'Forall (h_sp <> AST.expr_span e) [tv] e|] -- TODO: remove the list around tv
            expr_let --> Let . OBrace . decl_list . CBrace . expr_toplevel |> [|\(Located ls _) _ ds _ e -> AST.Expr'Let (ls <> AST.expr_span e) ds e|]
            expr_let --> Let . decl . expr_toplevel |> [|\(Located ls _) d e -> AST.Expr'Let (ls <> AST.expr_span e) [d] e|]
            expr_let --> LetRec . OBrace . decl_list . CBrace . expr_toplevel |> [|\(Located ls _) _ ds _ e -> AST.Expr'LetRec (ls <> AST.expr_span e) ds e|]
            expr_let --> LetRec . decl . expr_toplevel |> [|\(Located ls _) d e -> AST.Expr'LetRec (ls <> AST.expr_span e) [d] e|]
            expr_type_annotation --> Colon . type_ . Colon . expr_toplevel |> [|\(Located cs _) t _ e -> AST.Expr'TypeAnnotation (cs <> AST.expr_span e) t e|]
            expr_if
                --> (If . expr_toplevel . Then . expr_toplevel . Else . expr_toplevel)
                |> [|\(Located i_sp _) c _ t _ f -> AST.Expr'If (i_sp <> AST.expr_span f) i_sp c t f|]
            expr_lambda
                --> (Backslash . pattern_named_list_at_least_once . Arrow . expr_toplevel)
                |> [|\(Located bs_sp _) pats _ e -> AST.Expr'Lambda (bs_sp <> AST.expr_span e) pats e|]

            -- TODO: reorganize these things
            -- TODO: these are probably wrong
            expr_keyword_call
                --> (kw_iden_paths . keyword_call_args . optional_kw_iden_paths)
                |> [|\first_paths args more_path -> todo|]
            expr_keyword_call --> expr_binary_ops |> [|identity|]
            keyword_call_args
                --> (keyword_call_args . kw_iden_paths . expr_binary_ops)
                |> [|\other_args next_path arg -> todo|]
            keyword_call_args --> expr_binary_ops |> [|todo|]
            kw_iden_paths --> (kw_iden_path . kw_iden_paths) |> [|\a b -> a : b|]
            kw_iden_paths --> kw_iden_path |> [|\a -> [a]|]
            optional_kw_iden_paths --> kw_iden_paths |> [|Just|]
            optional_kw_iden_paths --> empty |> [|Nothing|]

            -- TODO: this does not work correctly
            expr_binary_ops --> expr_call |> [|identity|]
            expr_binary_ops
                --> (expr_call . operator . expr_binary_ops)
                |> [|\operand operator more -> AST.Expr'BinaryOps (AST.expr_span operand <> AST.expr_span more) todo todo|]
            operator
                --> S'T (SymbolIdentifier ())
                |> [|
                    \case
                        (Located sp (SymbolIdentifier n)) -> AST.PathOrSingleIden'Single $ Located sp n
                        _ -> unreachable
                    |]
            operator --> symbol_iden_path |> [|todo|]
            -- TODO: operator --> Backtick . alpha_iden_path . Backtick |> [|todo|]

            expr_call --> expr_primary |> [|identity|]
            expr_call --> (expr_call . expr_primary) |> [|\c a -> AST.Expr'Call (AST.expr_span c <> AST.expr_span a) c [a]|] -- TODO: remove the list around a
            expr_call --> (expr_call . Hash . type_primary) |> [|\c _ t -> AST.Expr'TypeApply (AST.expr_span c <> AST.type_span t) c [t]|] -- TODO: remove the list around t
            expr_primary --> expr_identifier |> [|identity|]
            expr_primary --> expr_hole |> [|identity|]
            expr_primary --> expr_literal |> [|identity|]
            expr_primary --> expr_match |> [|identity|]
            expr_primary --> expr_tuple |> [|identity|]
            expr_primary --> OParen . expr_toplevel . CParen |> [|\_ e _ -> e|] -- TODO: change span of this?
            expr_identifier --> alpha_iden_path |> [|AST.Expr'Identifier todo|]
            expr_hole --> (Question . aiden) |> [|\(Located q_span _) i@(Located i_span _) -> AST.Expr'Hole (q_span <> i_span) i|]
            expr_literal --> S'T (Char ()) |> [|\(Located sp (Token.Char c)) -> AST.Expr'Char sp c|]
            expr_literal --> S'T (String ()) |> [|\(Located sp (Token.String s)) -> AST.Expr'String sp s|]
            expr_literal --> S'T (Int () ()) |> [|\(Located sp (Token.Int _ i)) -> AST.Expr'Int sp i|]
            expr_literal --> S'T (Float ()) |> [|\(Located sp (Token.Float f)) -> AST.Expr'Float sp f|]
            expr_literal --> S'T (Bool ()) |> [|\(Located sp (Token.Bool b)) -> AST.Expr'Bool sp b|]
            expr_tuple
                --> (OParen . comma_sep_expr_list_at_least_one_comma . CParen)
                |> [|\(Located o_sp _) parts (Located c_sp _) -> AST.Expr'Tuple (o_sp <> c_sp) parts|]
            do
                match_arm <- nt "match arm" [t|(AST.Pattern, AST.Expr)|]
                match_arm_list <- list match_arm
                match_arm --> (pattern . Arrow . expr . Semicolon) |> [|\p _ e _ -> (p, e)|]
                expr_match
                    --> (Match . expr . OBrace . match_arm_list . CBrace)
                    |> [|\(Located m_sp _) scr _ arms (Located c_sp _) -> AST.Expr'Match (m_sp <> c_sp) m_sp scr arms|]

            type_ --> type_forall |> [|identity|]
            type_forall --> type_function |> [|identity|]
            type_forall --> Hash . aiden . type_forall |> [|\(Located h_sp _) iden t -> AST.Type'Forall (h_sp <> AST.type_span t) [iden] t|] -- TODO: remove this list around iden
            type_function --> type_apply_or_get . Arrow . type_function |> [|\a _ b -> AST.Type'Function (AST.type_span a <> AST.type_span b) a b|]
            type_function --> type_apply_or_get |> [|identity|]
            type_apply_or_get --> type_primary |> [|identity|]
            type_apply_or_get --> type_apply_or_get . Hash . type_primary |> [|\a _ b -> AST.Type'Apply (AST.type_span a <> AST.type_span b) a [b]|] -- TODO: remove the list around b
            type_apply_or_get --> type_apply_or_get . DoubleColon . aiden |> [|\t _ i@(Located i_sp _) -> AST.Type'Get (AST.type_span t <> i_sp) t i|]
            type_primary --> OParen . type_ . CParen |> [|\_ t _ -> t|] -- TODO: change this span?
            type_primary --> aiden |> [|AST.Type'Refer|]
            type_primary --> Underscore |> [|\(Located sp _) -> AST.Type'Wild sp|]
            type_primary --> Question . aiden |> [|\(Located q_sp _) i@(Located i_sp _) -> AST.Type'Hole (q_sp <> i_sp) i|]
            type_primary
                --> OParen
                . comma_sep_type_list_at_least_one_comma
                . CParen
                |> [|\(Located o_sp _) parts (Located c_sp _) -> AST.Type'Tuple (o_sp <> c_sp) parts|]

            pattern --> pattern_toplevel |> [|identity|]

            pattern_toplevel --> pattern_anon_variant |> [|identity|]
            pattern_toplevel --> pattern_named_variant |> [|identity|]
            pattern_toplevel --> pattern_named |> [|identity|]

            pattern_anon_variant --> (alpha_iden_path . pattern_named_list_at_least_once) |> [|\v p -> AST.Pattern'AnonADTVariant todo v p|]
            do
                field_pattern <- nt "named variant pattern field" [t|(AST.Identifier, AST.Pattern)|]
                field_pattern --> (aiden . Equal . pattern) |> [|\i _ p -> (i, p)|]
                field_list <- sep_list_allow_trailing Comma field_pattern

                pattern_named_variant
                    --> (alpha_iden_path . OBrace . field_list . CBrace)
                    |> [|\v _ p (Located cb_sp _) -> AST.Pattern'NamedADTVariant (todo <> cb_sp) v p|]

            pattern_named
                --> (aiden . At . pattern_named)
                |> [|\i@(Located i_sp _) (Located a_sp _) n -> AST.Pattern'Named (i_sp <> AST.pattern_span n) i a_sp n|]
            pattern_named --> pattern_primary |> [|identity|]

            pattern_primary --> pattern_iden |> [|identity|]
            pattern_primary --> pattern_wild |> [|identity|]
            pattern_primary --> (OParen . pattern . CParen) |> [|\_ a _ -> a|] -- TODO: change this span?
            pattern_primary --> pattern_tuple |> [|identity|]
            pattern_iden --> aiden |> [|AST.Pattern'Identifier|]
            pattern_wild --> Underscore |> [|\(Located sp _) -> AST.Pattern'Wildcard sp|]
            pattern_tuple
                --> (OParen . comma_sep_pattern_list_at_least_one_comma . CParen)
                |> [|\(Located o_sp _) parts (Located c_sp _) -> AST.Pattern'Tuple (o_sp <> c_sp) parts|]

            -- TODO: clean this up
            alpha_iden_path --> aiden |> [|AST.PathOrSingleIden'Single|]
            -- alpha_iden_path --> Root |> [|_|] TODO
            alpha_iden_path --> (alpha_iden_path . DoubleColon . aiden) |> [|todo|]
            alpha_iden_path --> (alpha_iden_path . DoubleColon . Hash . type_primary) |> [|todo|]
            alpha_iden_path --> (OBrack . OBrack . type_ . CBrack . CBrack . DoubleColon . aiden) |> [|todo|]

            kw_iden_path --> (Caret . alpha_iden_path . DoubleColon . S'T (KeywordIdentifier ())) |> [|todo|]
            symbol_iden_path --> (Backtick . alpha_iden_path . DoubleColon . S'T (SymbolIdentifier ())) |> [|todo|]

            type_param_list --> (type_param_list . Hash . aiden) |> [|\last _ i -> last ++ [i]|]
            type_param_list --> empty |> [|[]|]

            pattern_named_list_at_least_once --> (pattern_named_list_at_least_once . pattern_named) |> [|\ps p -> ps ++ [p]|]
            pattern_named_list_at_least_once --> pattern_named |> [|\p -> [p]|]

            comma_sep_expr_list_at_least_one_comma --> (expr . Comma . comma_sep_expr_list) |> [|\e _ more -> e : more|]
            comma_sep_type_list_at_least_one_comma --> (type_ . Comma . comma_sep_type_list) |> [|\t _ more -> t : more|]
            comma_sep_pattern_list_at_least_one_comma --> (pattern . Comma . comma_sep_pattern_list) |> [|\p _ more -> p : more|]

            aiden
                --> S'T (AlphaIdentifier ())
                |> [|
                    \case
                        (Located sp (AlphaIdentifier n)) -> Located sp n
                        _ -> unreachable
                    |]

            pure ()
        )
 )

parse :: [Token.LToken] -> Token.LToken -> Compiler.WithDiagnostics (Error.Error) Void [AST.Decl]
parse toks eof_tok =
    case parse' (toks InfList.+++ InfList.repeat eof_tok) of
        Right ast -> pure (trace_show_id ast)
        Left err -> do
            _ <- Compiler.tell_error err
            pure []
