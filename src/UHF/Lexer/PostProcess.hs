{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UHF.Lexer.PostProcess
    ( group_identifiers

    , tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Lexer.LexError as LexError

import qualified UHF.Token as Token
import qualified UHF.Token.Raw as Token.Raw
import qualified UHF.IO.Location as Location

import qualified Safe

group_identifiers :: ([LexError.LexError], [Location.Located Token.Raw.Token], Token.LToken) -> ([LexError.LexError], [Location.Located Token.Token], Token.LToken)
group_identifiers (errs, toks, eof_tok) =
    let (errs', grouped) = group_identifiers' toks
    in (errs ++ errs', grouped, eof_tok)

group_identifiers' :: [Location.Located Token.Raw.Token] -> ([LexError.LexError], [Location.Located Token.Token])

group_identifiers' ((Location.Located start_sp (Token.Raw.AlphaIdentifier start_iden)):more) =
    let find_iden ((Location.Located _ Token.Raw.DoubleColon) : (Location.Located sp (Token.Raw.AlphaIdentifier iden)) : m) =
            let (more_iden, is_symbol, m') = find_iden m
            in ((iden, sp):more_iden, is_symbol, m')

        find_iden ((Location.Located _ Token.Raw.DoubleColon) : (Location.Located sp (Token.Raw.SymbolIdentifier iden)) : m) =
            ([(iden, sp)], True, m)

        find_iden t =
            ([], False, t)

        (iden_found, is_symbol_identifier, more') = find_iden more
        iden_names = start_iden : map fst iden_found
        iden_sp = start_sp `Location.join_span` (Safe.lastDef start_sp $ map snd iden_found)

        iden_tok =
            if is_symbol_identifier
                then Token.SymbolIdentifier iden_names
                else Token.AlphaIdentifier iden_names

        (errs, more'_grouped) = group_identifiers' more'

    in (errs, Location.Located iden_sp iden_tok : more'_grouped)

group_identifiers' (other:more) =
    let (errs', more') = group_identifiers' more
    in case convert_raw_token other of
        Right converted -> (errs', converted : more')
        Left err -> (err : errs', more')

group_identifiers' [] = ([], [])

convert_raw_token :: Location.Located Token.Raw.Token -> Either LexError.LexError (Location.Located Token.Token)
convert_raw_token (Location.Located sp Token.Raw.OParen) = Right $ Location.Located sp Token.OParen
convert_raw_token (Location.Located sp Token.Raw.CParen) = Right $ Location.Located sp Token.CParen
convert_raw_token (Location.Located sp Token.Raw.OBrack) = Right $ Location.Located sp Token.OBrack
convert_raw_token (Location.Located sp Token.Raw.CBrack) = Right $ Location.Located sp Token.CBrack
convert_raw_token (Location.Located sp Token.Raw.Comma) = Right $ Location.Located sp Token.Comma
convert_raw_token (Location.Located sp Token.Raw.Equal) = Right $ Location.Located sp Token.Equal
convert_raw_token (Location.Located sp Token.Raw.Colon) = Right $ Location.Located sp Token.Colon
convert_raw_token (Location.Located sp Token.Raw.DoubleColon) = Left $ LexError.InvalidDoubleColon sp
convert_raw_token (Location.Located sp Token.Raw.Arrow) = Right $ Location.Located sp Token.Arrow
convert_raw_token (Location.Located sp Token.Raw.Root) = Right $ Location.Located sp Token.Root
convert_raw_token (Location.Located sp Token.Raw.Let) = Right $ Location.Located sp Token.Let
convert_raw_token (Location.Located sp Token.Raw.Data) = Right $ Location.Located sp Token.Data
convert_raw_token (Location.Located sp Token.Raw.Under) = Right $ Location.Located sp Token.Under
convert_raw_token (Location.Located sp Token.Raw.If) = Right $ Location.Located sp Token.If
convert_raw_token (Location.Located sp Token.Raw.Else) = Right $ Location.Located sp Token.Else
convert_raw_token (Location.Located sp Token.Raw.Case) = Right $ Location.Located sp Token.Case
convert_raw_token (Location.Located sp (Token.Raw.SymbolIdentifier i)) = Right $ Location.Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Location.Located _ (Token.Raw.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Location.Located sp (Token.Raw.CharLit val)) = Right $ Location.Located sp $ Token.CharLit val
convert_raw_token (Location.Located sp (Token.Raw.StringLit val)) = Right $ Location.Located sp $ Token.StringLit val
convert_raw_token (Location.Located sp (Token.Raw.IntLit base val)) = Right $ Location.Located sp $ Token.IntLit base val
convert_raw_token (Location.Located sp (Token.Raw.FloatLit val)) = Right $ Location.Located sp $ Token.FloatLit $ Token.Decimal val
convert_raw_token (Location.Located sp (Token.Raw.BoolLit val)) = Right $ Location.Located sp $ Token.BoolLit val
convert_raw_token (Location.Located sp Token.Raw.OBrace) = Right $ Location.Located sp Token.OBrace
convert_raw_token (Location.Located sp Token.Raw.CBrace) = Right $ Location.Located sp Token.CBrace
convert_raw_token (Location.Located sp Token.Raw.Semicolon) = Right $ Location.Located sp Token.Semicolon
convert_raw_token (Location.Located sp Token.Raw.Indent) = Right $ Location.Located sp Token.Indent
convert_raw_token (Location.Located sp Token.Raw.Dedent) = Right $ Location.Located sp Token.Dedent
convert_raw_token (Location.Located sp Token.Raw.Newline) = Right $ Location.Located sp Token.Newline

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    let (_, [paren_sp, a_sp, dcolon_sp, b_sp, eof_sp]) = SpanHelper.make_spans ["(", "a", "::", "b", "eof"]
        eof_tok = Location.Located eof_sp Token.EOF
    in ([], [Location.Located paren_sp Token.OParen, Location.Located (a_sp `Location.join_span` b_sp) (Token.AlphaIdentifier ["a", "b"])], eof_tok)
    @=?
    group_identifiers
        ( []
        , [ Location.Located paren_sp Token.Raw.OParen
          , Location.Located a_sp (Token.Raw.AlphaIdentifier "a")
          , Location.Located dcolon_sp Token.Raw.DoubleColon
          , Location.Located b_sp (Token.Raw.AlphaIdentifier "b")
          ]
        , eof_tok
        )

case_group_identifiers'_single_alpha :: Assertion
case_group_identifiers'_single_alpha =
    let (_, [sp]) = SpanHelper.make_spans ["a"]
    in ([], [Location.Located sp (Token.AlphaIdentifier ["a"])])
    @=?
    group_identifiers' [Location.Located sp (Token.Raw.AlphaIdentifier "a")]

case_group_identifiers'_multiple_alpha :: Assertion
case_group_identifiers'_multiple_alpha =
    let (_, [a, dc1, b, dc2, c]) = SpanHelper.make_spans ["a", "::", "b", "::", "c"]
    in ([], [Location.Located (a `Location.join_span` c) (Token.AlphaIdentifier ["a", "b", "c"])])
    @=?
    group_identifiers'
        [ Location.Located a $ Token.Raw.AlphaIdentifier "a"
        , Location.Located dc1 Token.Raw.DoubleColon
        , Location.Located b $ Token.Raw.AlphaIdentifier "b"
        , Location.Located dc2 Token.Raw.DoubleColon
        , Location.Located c $ Token.Raw.AlphaIdentifier "c"
        ]

case_group_identifiers'_single_symbol :: Assertion
case_group_identifiers'_single_symbol =
    let (_, [sp]) = SpanHelper.make_spans ["*"]
    in ([], [Location.Located sp $ Token.SymbolIdentifier ["*"]])
    @=?
    group_identifiers' [Location.Located sp $ Token.Raw.SymbolIdentifier "*"]

case_group_identifiers'_multiple_symbol :: Assertion
case_group_identifiers'_multiple_symbol =
    let (_, [a, dc1, b, dc2, star]) = SpanHelper.make_spans ["a", "::", "b", "::", "*"]
    in ([], [Location.Located (a `Location.join_span` star) $ Token.SymbolIdentifier ["a", "b", "*"]])
    @=?
    group_identifiers'
        [ Location.Located a $ Token.Raw.AlphaIdentifier "a"
        , Location.Located dc1 Token.Raw.DoubleColon
        , Location.Located b $ Token.Raw.AlphaIdentifier "b"
        , Location.Located dc2 Token.Raw.DoubleColon
        , Location.Located star $ Token.Raw.SymbolIdentifier "*"
        ]

case_group_identifiers'_symbol_start :: Assertion
case_group_identifiers'_symbol_start =
    let (_, [star, dc1, amper, dc2, dollar]) = SpanHelper.make_spans ["*", "::", "$", "::", "$"]
    in ([LexError.InvalidDoubleColon dc1, LexError.InvalidDoubleColon dc2], [Location.Located star $ Token.SymbolIdentifier ["*"], Location.Located amper $ Token.SymbolIdentifier ["&"], Location.Located dollar $ Token.SymbolIdentifier ["$"]])
    @=?
    group_identifiers'
        [ Location.Located star $ Token.Raw.SymbolIdentifier "*"
        , Location.Located dc1 Token.Raw.DoubleColon
        , Location.Located amper $ Token.Raw.SymbolIdentifier "&"
        , Location.Located dc2 Token.Raw.DoubleColon
        , Location.Located dollar $ Token.Raw.SymbolIdentifier "$"
        ]

tests :: TestTree
tests = $(testGroupGenerator)
