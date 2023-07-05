{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- TODO: remove this

module UHF.Phases.Lexer.IdentifierGrouper
    ( group_identifiers

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Data.Token as Token
import UHF.IO.Located (Located (..))

group_identifiers :: [Token.LInternalToken] -> [Token.LToken]
group_identifiers ((Located start_sp (Token.AlphaIdentifier start_iden)):more) =
    let (iden_found, is_symbol_identifier, more') = find_iden more
        iden_names = start_iden : map fst iden_found
        iden_sp = start_sp <> lastDef start_sp (map snd iden_found)

        iden_tok =
            if is_symbol_identifier
                then Token.SymbolIdentifier iden_names
                else Token.AlphaIdentifier iden_names

        more'_grouped  = group_identifiers more'
    in Located iden_sp iden_tok : more'_grouped
    where
        find_iden ((Located _ (Token.SingleTypeToken Token.DoubleColon)) : (Located sp (Token.AlphaIdentifier iden)) : m) =
            let (more_iden, is_symbol, m') = find_iden m
            in ((iden, sp):more_iden, is_symbol, m')

        find_iden ((Located _ (Token.SingleTypeToken Token.DoubleColon)) : (Located sp (Token.SymbolIdentifier iden)) : m) =
            ([(iden, sp)], True, m)

        find_iden t =
            ([], False, t)

group_identifiers (other:more) =
    let more' = group_identifiers more
    in convert_raw_token other : more'

group_identifiers [] = []

convert_raw_token :: Token.LInternalToken -> Token.LToken
convert_raw_token (Located sp (Token.SingleTypeToken t)) = Located sp (Token.SingleTypeToken t)
convert_raw_token (Located sp (Token.Char ch)) = Located sp (Token.Char ch)
convert_raw_token (Located sp (Token.String str)) = Located sp (Token.String str)
convert_raw_token (Located sp (Token.Int b n)) = Located sp (Token.Int b n)
convert_raw_token (Located sp (Token.Float f)) = Located sp (Token.Float f)
convert_raw_token (Located sp (Token.Bool b)) = Located sp (Token.Bool b)
convert_raw_token (Located sp (Token.SymbolIdentifier i)) = Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Located _ (Token.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Located _ (Token.EOF eof)) = absurd eof

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    SpanHelper.make_spans ["(", "a", "::", "b", "eof"] >>= \ (_, [paren_sp, a_sp, dcolon_sp, b_sp, _]) ->
    [Located paren_sp (Token.SingleTypeToken Token.OParen), Located (a_sp <> b_sp) (Token.AlphaIdentifier [Located a_sp "a", Located b_sp "b"])]
    @=?
    group_identifiers
        [ Located paren_sp (Token.SingleTypeToken Token.OParen)
        , Located a_sp (Token.AlphaIdentifier (Located a_sp "a"))
        , Located dcolon_sp (Token.SingleTypeToken Token.DoubleColon)
        , Located b_sp (Token.AlphaIdentifier (Located b_sp "b"))
        ]

case_group_identifiers_single_alpha :: Assertion
case_group_identifiers_single_alpha =
    SpanHelper.make_spans ["a"] >>= \ (_, [sp]) ->
    [Located sp (Token.AlphaIdentifier [Located sp "a"])]
    @=?
    group_identifiers [Located sp (Token.AlphaIdentifier (Located sp "a"))]

case_group_identifiers_multiple_alpha :: Assertion
case_group_identifiers_multiple_alpha =
    SpanHelper.make_spans ["a", "::", "b", "::", "c"] >>= \ (_, [a, dc1, b, dc2, c]) ->
    [Located (a <> c) (Token.AlphaIdentifier [Located a "a", Located b "b", Located c "c"])]
    @=?
    group_identifiers
        [ Located a $ Token.AlphaIdentifier (Located a "a")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located b $ Token.AlphaIdentifier (Located b "b")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located c $ Token.AlphaIdentifier (Located c "c")
        ]

case_group_identifiers_single_symbol :: Assertion
case_group_identifiers_single_symbol =
    SpanHelper.make_spans ["*"] >>= \ (_, [sp]) ->
    [Located sp $ Token.SymbolIdentifier [Located sp "*"]]
    @=?
    group_identifiers [Located sp $ Token.SymbolIdentifier (Located sp "*")]

case_group_identifiers_multiple_symbol :: Assertion
case_group_identifiers_multiple_symbol =
    SpanHelper.make_spans ["a", "::", "b", "::", "*"] >>= \ (_, [a, dc1, b, dc2, star]) ->
    [Located (a <> star) $ Token.SymbolIdentifier [Located a "a", Located b "b", Located star "*"]]
    @=?
    group_identifiers
        [ Located a $ Token.AlphaIdentifier (Located a "a")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located b $ Token.AlphaIdentifier (Located b "b")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located star $ Token.SymbolIdentifier (Located star "*")
        ]

case_group_identifiers_symbol_start :: Assertion
case_group_identifiers_symbol_start =
    SpanHelper.make_spans ["*", "::", "$", "::", "$"] >>= \ (_, [star, dc1, amper, dc2, dollar]) ->
    [Located star $ Token.SymbolIdentifier [Located star "*"], Located dc1 (Token.SingleTypeToken Token.DoubleColon), Located amper $ Token.SymbolIdentifier [Located amper "&"], Located dc2 (Token.SingleTypeToken Token.DoubleColon), Located dollar $ Token.SymbolIdentifier [Located dollar "$"]]
    @=?
    group_identifiers
        [ Located star $ Token.SymbolIdentifier (Located star "*")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located amper $ Token.SymbolIdentifier (Located amper "&")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located dollar $ Token.SymbolIdentifier (Located dollar "$")
        ]

tests :: TestTree
tests = $(testGroupGenerator)
