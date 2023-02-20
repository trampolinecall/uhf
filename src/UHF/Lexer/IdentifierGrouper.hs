module UHF.Lexer.IdentifierGrouper
    ( group_identifiers

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Lexer.LexError as LexError

import qualified UHF.Token as Token
import qualified UHF.IO.Span as Span
import UHF.IO.Located (Located (..))

group_identifiers :: [Token.LInternalToken] -> Writer [LexError.LexError] [Token.LToken]
group_identifiers ((Located start_sp (Token.AlphaIdentifier start_iden)):more) =
    let find_iden ((Located _ (Token.SingleTypeToken Token.DoubleColon)) : (Located sp (Token.AlphaIdentifier iden)) : m) =
            let (more_iden, is_symbol, m') = find_iden m
            in ((iden, sp):more_iden, is_symbol, m')

        find_iden ((Located _ (Token.SingleTypeToken Token.DoubleColon)) : (Located sp (Token.SymbolIdentifier iden)) : m) =
            ([(iden, sp)], True, m)

        find_iden t =
            ([], False, t)

        (iden_found, is_symbol_identifier, more') = find_iden more
        iden_names = start_iden : map fst iden_found
        iden_sp = start_sp `Span.join` lastDef start_sp (map snd iden_found)

        iden_tok =
            if is_symbol_identifier
                then Token.SymbolIdentifier iden_names
                else Token.AlphaIdentifier iden_names

    in group_identifiers more' >>= \ more'_grouped ->
    pure (Located iden_sp iden_tok : more'_grouped)

group_identifiers (other:more) =
    group_identifiers more >>= \ more' ->
    case convert_raw_token other of
        Right converted -> pure $ converted : more'
        Left err -> tell [err] >> pure more'

group_identifiers [] = pure []

convert_raw_token :: Token.LInternalToken -> Either LexError.LexError Token.LToken
convert_raw_token (Located sp (Token.SingleTypeToken t)) = Right $ Located sp (Token.SingleTypeToken t)
convert_raw_token (Located sp (Token.Char ch)) = Right $ Located sp (Token.Char ch)
convert_raw_token (Located sp (Token.String str)) = Right $ Located sp (Token.String str)
convert_raw_token (Located sp (Token.Int b n)) = Right $ Located sp (Token.Int b n)
convert_raw_token (Located sp (Token.Float f)) = Right $ Located sp (Token.Float f)
convert_raw_token (Located sp (Token.Bool b)) = Right $ Located sp (Token.Bool b)
convert_raw_token (Located sp (Token.SymbolIdentifier i)) = Right $ Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Located _ (Token.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Located _ (Token.EOF eof)) = absurd eof

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    let (_, [paren_sp, a_sp, dcolon_sp, b_sp, _]) = SpanHelper.make_spans ["(", "a", "::", "b", "eof"]
    in ([Located paren_sp (Token.SingleTypeToken Token.OParen), Located (a_sp `Span.join` b_sp) (Token.AlphaIdentifier [Located a_sp "a", Located b_sp "b"])], [])
    @=?
    runWriter (group_identifiers
        [ Located paren_sp (Token.SingleTypeToken Token.OParen)
        , Located a_sp (Token.AlphaIdentifier (Located a_sp "a"))
        , Located dcolon_sp (Token.SingleTypeToken Token.DoubleColon)
        , Located b_sp (Token.AlphaIdentifier (Located b_sp "b"))
        ])

case_group_identifiers_single_alpha :: Assertion
case_group_identifiers_single_alpha =
    let (_, [sp]) = SpanHelper.make_spans ["a"]
    in ([Located sp (Token.AlphaIdentifier [Located sp "a"])], [])
    @=?
    runWriter (group_identifiers [Located sp (Token.AlphaIdentifier (Located sp "a"))])

case_group_identifiers_multiple_alpha :: Assertion
case_group_identifiers_multiple_alpha =
    let (_, [a, dc1, b, dc2, c]) = SpanHelper.make_spans ["a", "::", "b", "::", "c"]
    in ([Located (a `Span.join` c) (Token.AlphaIdentifier [Located a "a", Located b "b", Located c "c"])], [])
    @=?
    runWriter (group_identifiers
        [ Located a $ Token.AlphaIdentifier (Located a "a")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located b $ Token.AlphaIdentifier (Located b "b")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located c $ Token.AlphaIdentifier (Located c "c")
        ])

case_group_identifiers_single_symbol :: Assertion
case_group_identifiers_single_symbol =
    let (_, [sp]) = SpanHelper.make_spans ["*"]
    in ([Located sp $ Token.SymbolIdentifier [Located sp "*"]], [])
    @=?
    runWriter (group_identifiers [Located sp $ Token.SymbolIdentifier (Located sp "*")])

case_group_identifiers_multiple_symbol :: Assertion
case_group_identifiers_multiple_symbol =
    let (_, [a, dc1, b, dc2, star]) = SpanHelper.make_spans ["a", "::", "b", "::", "*"]
    in ([Located (a `Span.join` star) $ Token.SymbolIdentifier [Located a "a", Located b "b", Located star "*"]], [])
    @=?
    runWriter (group_identifiers
        [ Located a $ Token.AlphaIdentifier (Located a "a")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located b $ Token.AlphaIdentifier (Located b "b")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located star $ Token.SymbolIdentifier (Located star "*")
        ])

case_group_identifiers_symbol_start :: Assertion
case_group_identifiers_symbol_start =
    let (_, [star, dc1, amper, dc2, dollar]) = SpanHelper.make_spans ["*", "::", "$", "::", "$"]
    in ([Located star $ Token.SymbolIdentifier [Located star "*"], Located amper $ Token.SymbolIdentifier [Located amper "&"], Located dollar $ Token.SymbolIdentifier [Located dollar "$"]], [LexError.InvalidDoubleColon dc1, LexError.InvalidDoubleColon dc2])
    @=?
    runWriter (group_identifiers
        [ Located star $ Token.SymbolIdentifier (Located star "*")
        , Located dc1 (Token.SingleTypeToken Token.DoubleColon)
        , Located amper $ Token.SymbolIdentifier (Located amper "&")
        , Located dc2 (Token.SingleTypeToken Token.DoubleColon)
        , Located dollar $ Token.SymbolIdentifier (Located dollar "$")
        ])

tests :: TestTree
tests = $(testGroupGenerator)
