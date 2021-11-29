{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UHF.Lexer.PostProcess
    ( group_identifiers

    , tests
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified UHF.Test.SpanHelper as SpanHelper

import qualified UHF.Lexer.MainLexer as MainLexer

import qualified UHF.Token as Token
import qualified UHF.RawToken as RawToken
import qualified UHF.IO.Location as Location

import qualified Safe

group_identifiers :: ([MainLexer.LexError], [Location.Located RawToken.Token]) -> ([MainLexer.LexError], [Location.Located Token.Token])
group_identifiers (errs, toks) =
    let (errs', grouped) = group_identifiers' toks
    in (errs ++ errs', grouped)

group_identifiers' :: [Location.Located RawToken.Token] -> ([MainLexer.LexError], [Location.Located Token.Token])

group_identifiers' ((Location.Located start_sp (RawToken.AlphaIdentifier start_iden)):more) =
    let find_iden ((Location.Located _ RawToken.DoubleColon) : (Location.Located sp (RawToken.AlphaIdentifier iden)) : m) =
            let (more_iden, is_symbol, m') = find_iden m
            in ((iden, sp):more_iden, is_symbol, m')

        find_iden ((Location.Located _ RawToken.DoubleColon) : (Location.Located sp (RawToken.SymbolIdentifier iden)) : m) =
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

convert_raw_token :: Location.Located RawToken.Token -> Either MainLexer.LexError (Location.Located Token.Token)
convert_raw_token (Location.Located sp RawToken.OParen) = Right $ Location.Located sp Token.OParen
convert_raw_token (Location.Located sp RawToken.CParen) = Right $ Location.Located sp Token.CParen
convert_raw_token (Location.Located sp RawToken.OBrack) = Right $ Location.Located sp Token.OBrack
convert_raw_token (Location.Located sp RawToken.CBrack) = Right $ Location.Located sp Token.CBrack
convert_raw_token (Location.Located sp RawToken.Comma) = Right $ Location.Located sp Token.Comma
convert_raw_token (Location.Located sp RawToken.Equal) = Right $ Location.Located sp Token.Equal
convert_raw_token (Location.Located sp RawToken.DoubleColon) = Left $ MainLexer.InvalidDoubleColon sp
convert_raw_token (Location.Located sp RawToken.Root) = Right $ Location.Located sp Token.Root
convert_raw_token (Location.Located sp RawToken.Let) = Right $ Location.Located sp Token.Let
convert_raw_token (Location.Located sp RawToken.Data) = Right $ Location.Located sp Token.Data
convert_raw_token (Location.Located sp RawToken.Under) = Right $ Location.Located sp Token.Under
convert_raw_token (Location.Located sp RawToken.If) = Right $ Location.Located sp Token.If
convert_raw_token (Location.Located sp RawToken.Else) = Right $ Location.Located sp Token.Else
convert_raw_token (Location.Located sp RawToken.Case) = Right $ Location.Located sp Token.Case
convert_raw_token (Location.Located sp (RawToken.SymbolIdentifier i)) = Right $ Location.Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Location.Located _ (RawToken.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Location.Located sp (RawToken.CharLit val)) = Right $ Location.Located sp $ Token.CharLit val
convert_raw_token (Location.Located sp (RawToken.StringLit val)) = Right $ Location.Located sp $ Token.StringLit val
convert_raw_token (Location.Located sp (RawToken.IntLit base val)) = Right $ Location.Located sp $ Token.IntLit base val
convert_raw_token (Location.Located sp (RawToken.FloatLit val)) = Right $ Location.Located sp $ Token.FloatLit val
convert_raw_token (Location.Located sp (RawToken.BoolLit val)) = Right $ Location.Located sp $ Token.BoolLit val
convert_raw_token (Location.Located sp RawToken.OBrace) = Right $ Location.Located sp Token.OBrace
convert_raw_token (Location.Located sp RawToken.CBrace) = Right $ Location.Located sp Token.CBrace
convert_raw_token (Location.Located sp RawToken.Semicolon) = Right $ Location.Located sp Token.Semicolon
convert_raw_token (Location.Located sp RawToken.Indent) = Right $ Location.Located sp Token.Indent
convert_raw_token (Location.Located sp RawToken.Dedent) = Right $ Location.Located sp Token.Dedent
convert_raw_token (Location.Located sp RawToken.Newline) = Right $ Location.Located sp Token.Newline

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    let (_, [paren_sp, a_sp, dcolon_sp, b_sp]) = SpanHelper.make_spans ["(", "a", "::", "b"]
    in ([], [Location.Located paren_sp Token.OParen, Location.Located (a_sp `Location.join_span` b_sp) (Token.AlphaIdentifier ["a", "b"])])
    @=?
    group_identifiers
        ( []
        , [ Location.Located paren_sp RawToken.OParen
          , Location.Located a_sp (RawToken.AlphaIdentifier "a")
          , Location.Located dcolon_sp RawToken.DoubleColon
          , Location.Located b_sp (RawToken.AlphaIdentifier "b")
          ]
        )

case_group_identifiers'_single_alpha :: Assertion
case_group_identifiers'_single_alpha =
    let (_, [sp]) = SpanHelper.make_spans ["a"]
    in ([], [Location.Located sp (Token.AlphaIdentifier ["a"])])
    @=?
    group_identifiers' [Location.Located sp (RawToken.AlphaIdentifier "a")]

case_group_identifiers'_multiple_alpha :: Assertion
case_group_identifiers'_multiple_alpha =
    let (_, [a, dc1, b, dc2, c]) = SpanHelper.make_spans ["a", "::", "b", "::", "c"]
    in ([], [Location.Located (a `Location.join_span` c) (Token.AlphaIdentifier ["a", "b", "c"])])
    @=?
    group_identifiers'
        [ Location.Located a $ RawToken.AlphaIdentifier "a"
        , Location.Located dc1 RawToken.DoubleColon
        , Location.Located b $ RawToken.AlphaIdentifier "b"
        , Location.Located dc2 RawToken.DoubleColon
        , Location.Located c $ RawToken.AlphaIdentifier "c"
        ]

case_group_identifiers'_single_symbol :: Assertion
case_group_identifiers'_single_symbol =
    let (_, [sp]) = SpanHelper.make_spans ["*"]
    in ([], [Location.Located sp $ Token.SymbolIdentifier ["*"]])
    @=?
    group_identifiers' [Location.Located sp $ RawToken.SymbolIdentifier "*"]

case_group_identifiers'_multiple_symbol :: Assertion
case_group_identifiers'_multiple_symbol =
    let (_, [a, dc1, b, dc2, star]) = SpanHelper.make_spans ["a", "::", "b", "::", "*"]
    in ([], [Location.Located (a `Location.join_span` star) $ Token.SymbolIdentifier ["a", "b", "*"]])
    @=?
    group_identifiers'
        [ Location.Located a $ RawToken.AlphaIdentifier "a"
        , Location.Located dc1 RawToken.DoubleColon
        , Location.Located b $ RawToken.AlphaIdentifier "b"
        , Location.Located dc2 RawToken.DoubleColon
        , Location.Located star $ RawToken.SymbolIdentifier "*"
        ]

case_group_identifiers'_symbol_start :: Assertion
case_group_identifiers'_symbol_start =
    let (_, [star, dc1, amper, dc2, dollar]) = SpanHelper.make_spans ["*", "::", "$", "::", "$"]
    in ([MainLexer.InvalidDoubleColon dc1, MainLexer.InvalidDoubleColon dc2], [Location.Located star $ Token.SymbolIdentifier ["*"], Location.Located amper $ Token.SymbolIdentifier ["&"], Location.Located dollar $ Token.SymbolIdentifier ["$"]])
    @=?
    group_identifiers'
        [ Location.Located star $ RawToken.SymbolIdentifier "*"
        , Location.Located dc1 RawToken.DoubleColon
        , Location.Located amper $ RawToken.SymbolIdentifier "&"
        , Location.Located dc2 RawToken.DoubleColon
        , Location.Located dollar $ RawToken.SymbolIdentifier "$"
        ]

tests :: TestTree
tests = $(testGroupGenerator)
