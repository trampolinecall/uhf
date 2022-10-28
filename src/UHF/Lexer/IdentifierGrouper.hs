module UHF.Lexer.IdentifierGrouper
    ( group_identifiers

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Lexer.LexError as LexError

import qualified UHF.Token as Token
import qualified UHF.IO.Location as Location

group_identifiers :: [Token.LTokenWithIndentation] -> ([LexError.LexError], [Token.LNormalToken])
group_identifiers ((Location.Located start_sp (Token.AlphaIdentifier start_iden)):more) =
    let find_iden ((Location.Located _ (Token.DoubleColon _)) : (Location.Located sp (Token.AlphaIdentifier iden)) : m) =
            let (more_iden, is_symbol, m') = find_iden m
            in ((iden, sp):more_iden, is_symbol, m')

        find_iden ((Location.Located _ (Token.DoubleColon _)) : (Location.Located sp (Token.SymbolIdentifier iden)) : m) =
            ([(iden, sp)], True, m)

        find_iden t =
            ([], False, t)

        (iden_found, is_symbol_identifier, more') = find_iden more
        iden_names = start_iden : map fst iden_found
        iden_sp = start_sp `Location.join_span` lastDef start_sp (map snd iden_found)

        iden_tok =
            if is_symbol_identifier
                then Token.SymbolIdentifier iden_names
                else Token.AlphaIdentifier iden_names

        (errs, more'_grouped) = group_identifiers more'

    in (errs, Location.Located iden_sp iden_tok : more'_grouped)

group_identifiers (other:more) =
    let (errs', more') = group_identifiers more
    in case convert_raw_token other of
        Right converted -> (errs', converted : more')
        Left err -> (err : errs', more')

group_identifiers [] = ([], [])

convert_raw_token :: Token.LTokenWithIndentation -> Either LexError.LexError Token.LNormalToken
convert_raw_token (Location.Located sp (Token.SingleTypeToken t)) = Right $ Location.Located sp (Token.SingleTypeToken t)
convert_raw_token (Location.Located sp (Token.DoubleColon _)) = Left $ LexError.InvalidDoubleColon sp
convert_raw_token (Location.Located sp (Token.SymbolIdentifier i)) = Right $ Location.Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Location.Located _ (Token.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Location.Located sp Token.OBrace) = Right $ Location.Located sp Token.OBrace
convert_raw_token (Location.Located sp Token.CBrace) = Right $ Location.Located sp Token.CBrace
convert_raw_token (Location.Located sp Token.Semicolon) = Right $ Location.Located sp Token.Semicolon
convert_raw_token (Location.Located sp (Token.Indent i)) = Right $ Location.Located sp $ Token.Indent i
convert_raw_token (Location.Located sp (Token.Dedent i)) = Right $ Location.Located sp $ Token.Dedent i
convert_raw_token (Location.Located sp (Token.Newline nl)) = Right $ Location.Located sp $ Token.Newline nl
convert_raw_token (Location.Located _ (Token.Backslash v)) = absurd v
convert_raw_token (Location.Located _ (Token.EOF eof)) = absurd eof

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    let (_, [paren_sp, a_sp, dcolon_sp, b_sp, _]) = SpanHelper.make_spans ["(", "a", "::", "b", "eof"]
    in ([], [Location.Located paren_sp (Token.SingleTypeToken Token.OParen), Location.Located (a_sp `Location.join_span` b_sp) (Token.AlphaIdentifier ["a", "b"])])
    @=?
    group_identifiers
        [ Location.Located paren_sp (Token.SingleTypeToken Token.OParen)
        , Location.Located a_sp (Token.AlphaIdentifier "a")
        , Location.Located dcolon_sp (Token.DoubleColon ())
        , Location.Located b_sp (Token.AlphaIdentifier "b")
        ]

case_group_identifiers_single_alpha :: Assertion
case_group_identifiers_single_alpha =
    let (_, [sp]) = SpanHelper.make_spans ["a"]
    in ([], [Location.Located sp (Token.AlphaIdentifier ["a"])])
    @=?
    group_identifiers [Location.Located sp (Token.AlphaIdentifier "a")]

case_group_identifiers_multiple_alpha :: Assertion
case_group_identifiers_multiple_alpha =
    let (_, [a, dc1, b, dc2, c]) = SpanHelper.make_spans ["a", "::", "b", "::", "c"]
    in ([], [Location.Located (a `Location.join_span` c) (Token.AlphaIdentifier ["a", "b", "c"])])
    @=?
    group_identifiers
        [ Location.Located a $ Token.AlphaIdentifier "a"
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located b $ Token.AlphaIdentifier "b"
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located c $ Token.AlphaIdentifier "c"
        ]

case_group_identifiers_single_symbol :: Assertion
case_group_identifiers_single_symbol =
    let (_, [sp]) = SpanHelper.make_spans ["*"]
    in ([], [Location.Located sp $ Token.SymbolIdentifier ["*"]])
    @=?
    group_identifiers [Location.Located sp $ Token.SymbolIdentifier "*"]

case_group_identifiers_multiple_symbol :: Assertion
case_group_identifiers_multiple_symbol =
    let (_, [a, dc1, b, dc2, star]) = SpanHelper.make_spans ["a", "::", "b", "::", "*"]
    in ([], [Location.Located (a `Location.join_span` star) $ Token.SymbolIdentifier ["a", "b", "*"]])
    @=?
    group_identifiers
        [ Location.Located a $ Token.AlphaIdentifier "a"
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located b $ Token.AlphaIdentifier "b"
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located star $ Token.SymbolIdentifier "*"
        ]

case_group_identifiers_symbol_start :: Assertion
case_group_identifiers_symbol_start =
    let (_, [star, dc1, amper, dc2, dollar]) = SpanHelper.make_spans ["*", "::", "$", "::", "$"]
    in ([LexError.InvalidDoubleColon dc1, LexError.InvalidDoubleColon dc2], [Location.Located star $ Token.SymbolIdentifier ["*"], Location.Located amper $ Token.SymbolIdentifier ["&"], Location.Located dollar $ Token.SymbolIdentifier ["$"]])
    @=?
    group_identifiers
        [ Location.Located star $ Token.SymbolIdentifier "*"
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located amper $ Token.SymbolIdentifier "&"
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located dollar $ Token.SymbolIdentifier "$"
        ]

tests :: TestTree
tests = $(testGroupGenerator)
