{-# LANGUAGE ExistentialQuantification #-}

module UHF.Parser.Test (ParsingTest(..), make_token_stream, run_test) where

import UHF.Util.Prelude

import qualified UHF.Parser.PEG as PEG

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Token as Token

import qualified Data.InfList as InfList

import qualified Data.Text as Text

data ParsingTest = forall r. (Show r, Eq r) => ParsingTest [Char] (File.File, PEG.TokenStream) r [([Char], PEG.Parser r)]

make_token_stream :: [Token.Token] -> (File.File, PEG.TokenStream)
make_token_stream things =
    let (file, things') = SpanHelper.make_spans_with_items $ map (\ t -> (token_to_text t, t)) things
        l = last things'
    in (file, InfList.zip (InfList.iterate (1+) 0) (things' InfList.+++ InfList.repeat l))

    where
        -- TODO: is this necessary? or can this just be replaced with all empty strings
        token_to_text :: Token.Token -> Text
        token_to_text (Token.SingleTypeToken Token.OParen) = "("
        token_to_text (Token.SingleTypeToken Token.CParen) = ")"
        token_to_text (Token.SingleTypeToken Token.OBrack) = "["
        token_to_text (Token.SingleTypeToken Token.CBrack) = "]"
        token_to_text (Token.SingleTypeToken Token.Comma) = ","
        token_to_text (Token.SingleTypeToken Token.Equal) = "="
        token_to_text (Token.SingleTypeToken Token.Colon) = ":"
        token_to_text (Token.SingleTypeToken Token.Arrow) = "->"
        token_to_text (Token.SingleTypeToken Token.At) = "@"

        token_to_text (Token.SingleTypeToken Token.DoubleColon) = "::"

        token_to_text (Token.SingleTypeToken Token.Root) = "root"
        token_to_text (Token.SingleTypeToken Token.Let) = "let"
        token_to_text (Token.SingleTypeToken Token.LetRec) = "letrec"
        token_to_text (Token.SingleTypeToken Token.Type) = "type"
        token_to_text (Token.SingleTypeToken Token.Data) = "data"
        token_to_text (Token.SingleTypeToken Token.Under) = "under"
        token_to_text (Token.SingleTypeToken Token.If) = "if"
        token_to_text (Token.SingleTypeToken Token.Else) = "else"
        token_to_text (Token.SingleTypeToken Token.Case) = "case"

        token_to_text (Token.SingleTypeToken Token.OBrace) = "{"
        token_to_text (Token.SingleTypeToken Token.CBrace) = "}"
        token_to_text (Token.SingleTypeToken Token.Semicolon) = ";"

        token_to_text (Token.Char c) = "'" `Text.snoc` c `Text.append` "'"
        token_to_text (Token.String s) = "\"" <> s <> "\""
        token_to_text (Token.Int _ i) = show i
        token_to_text (Token.Float f) = show f
        token_to_text (Token.Bool True) = "true"
        token_to_text (Token.Bool False) = "false"

        token_to_text (Token.SymbolIdentifier parts) = Text.intercalate "::" (map Location.unlocate parts)
        token_to_text (Token.AlphaIdentifier parts) = Text.intercalate "::" (map Location.unlocate parts)

        token_to_text (Token.EOF _) = "end of file"

{- TODO:
check_parser :: [Error.Error] -> r -> [Token.Token] -> Parser r -> PEG.TokenStream -> IO ()
check_parser expected_recoverable_errors expected_result parser toks =
    let PEG.ParseResult (result_recoverable_errors, result) = runStateT parser toks
    in result_recoverable_errors @?= expected_recoverable_errors >>
    case (result, expected_result) of
        (Right (got_result, got_toks'), (expected_result, expected_toks'))

        _ -> assertFailure "checking parser failed: constructors for result do not match"
-}

run_test :: ParsingTest -> TestTree
run_test (ParsingTest construct_name (_, construct_toks) construct_res parsers) =
    testGroup construct_name $
        map
            (\ (p_name, p) ->
                testCase (p_name ++ " parsing " ++ construct_name) $ ([], [], Just construct_res) @=? PEG.eval_parser p construct_toks)
            parsers
