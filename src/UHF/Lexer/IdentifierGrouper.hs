module UHF.Lexer.IdentifierGrouper
    ( group_identifiers

    , tests
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location.SpanHelper as SpanHelper

import qualified UHF.Lexer.LexError as LexError

import qualified UHF.Token as Token
import qualified UHF.IO.Location as Location

group_identifiers :: [Token.LRawToken] -> Writer [LexError.LexError] [Token.LToken]
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

    in group_identifiers more' >>= \ more'_grouped ->
    pure (Location.Located iden_sp iden_tok : more'_grouped)

group_identifiers (other:more) =
    group_identifiers more >>= \ more' ->
    case convert_raw_token other of
        Right converted -> pure $ converted : more'
        Left err -> tell [err] >> pure more'

group_identifiers [] = pure []

convert_raw_token :: Token.LRawToken -> Either LexError.LexError Token.LToken
convert_raw_token (Location.Located sp (Token.SingleTypeToken t)) = Right $ Location.Located sp (Token.SingleTypeToken t)
convert_raw_token (Location.Located sp (Token.CharLit ch)) = Right $ Location.Located sp (Token.CharLit ch)
convert_raw_token (Location.Located sp (Token.StringLit str)) = Right $ Location.Located sp (Token.StringLit str)
convert_raw_token (Location.Located sp (Token.IntLit b n)) = Right $ Location.Located sp (Token.IntLit b n)
convert_raw_token (Location.Located sp (Token.FloatLit f)) = Right $ Location.Located sp (Token.FloatLit f)
convert_raw_token (Location.Located sp (Token.BoolLit b)) = Right $ Location.Located sp (Token.BoolLit b)
convert_raw_token (Location.Located sp (Token.DoubleColon _)) = Left $ LexError.InvalidDoubleColon sp
convert_raw_token (Location.Located sp (Token.SymbolIdentifier i)) = Right $ Location.Located sp $ Token.SymbolIdentifier [i]
convert_raw_token (Location.Located _ (Token.AlphaIdentifier _)) = error "cannot convert raw alpha identiifer to alpha identifier"
convert_raw_token (Location.Located _ (Token.EOF eof)) = absurd eof

-- tests {{{1
case_group_identifiers :: Assertion
case_group_identifiers =
    let (_, [paren_sp, a_sp, dcolon_sp, b_sp, _]) = SpanHelper.make_spans ["(", "a", "::", "b", "eof"]
    in ([Location.Located paren_sp (Token.SingleTypeToken Token.OParen), Location.Located (a_sp `Location.join_span` b_sp) (Token.AlphaIdentifier [Location.Located a_sp "a", Location.Located b_sp "b"])], [])
    @=?
    runWriter (group_identifiers
        [ Location.Located paren_sp (Token.SingleTypeToken Token.OParen)
        , Location.Located a_sp (Token.AlphaIdentifier (Location.Located a_sp "a"))
        , Location.Located dcolon_sp (Token.DoubleColon ())
        , Location.Located b_sp (Token.AlphaIdentifier (Location.Located b_sp "b"))
        ])

case_group_identifiers_single_alpha :: Assertion
case_group_identifiers_single_alpha =
    let (_, [sp]) = SpanHelper.make_spans ["a"]
    in ([Location.Located sp (Token.AlphaIdentifier [Location.Located sp "a"])], [])
    @=?
    runWriter (group_identifiers [Location.Located sp (Token.AlphaIdentifier (Location.Located sp "a"))])

case_group_identifiers_multiple_alpha :: Assertion
case_group_identifiers_multiple_alpha =
    let (_, [a, dc1, b, dc2, c]) = SpanHelper.make_spans ["a", "::", "b", "::", "c"]
    in ([Location.Located (a `Location.join_span` c) (Token.AlphaIdentifier [Location.Located a "a", Location.Located b "b", Location.Located c "c"])], [])
    @=?
    runWriter (group_identifiers
        [ Location.Located a $ Token.AlphaIdentifier (Location.Located a "a")
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located b $ Token.AlphaIdentifier (Location.Located b "b")
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located c $ Token.AlphaIdentifier (Location.Located c "c")
        ])

case_group_identifiers_single_symbol :: Assertion
case_group_identifiers_single_symbol =
    let (_, [sp]) = SpanHelper.make_spans ["*"]
    in ([Location.Located sp $ Token.SymbolIdentifier [Location.Located sp "*"]], [])
    @=?
    runWriter (group_identifiers [Location.Located sp $ Token.SymbolIdentifier (Location.Located sp "*")])

case_group_identifiers_multiple_symbol :: Assertion
case_group_identifiers_multiple_symbol =
    let (_, [a, dc1, b, dc2, star]) = SpanHelper.make_spans ["a", "::", "b", "::", "*"]
    in ([Location.Located (a `Location.join_span` star) $ Token.SymbolIdentifier [Location.Located a "a", Location.Located b "b", Location.Located star "*"]], [])
    @=?
    runWriter (group_identifiers
        [ Location.Located a $ Token.AlphaIdentifier (Location.Located a "a")
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located b $ Token.AlphaIdentifier (Location.Located b "b")
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located star $ Token.SymbolIdentifier (Location.Located star "*")
        ])

case_group_identifiers_symbol_start :: Assertion
case_group_identifiers_symbol_start =
    let (_, [star, dc1, amper, dc2, dollar]) = SpanHelper.make_spans ["*", "::", "$", "::", "$"]
    in ([Location.Located star $ Token.SymbolIdentifier [Location.Located star "*"], Location.Located amper $ Token.SymbolIdentifier [Location.Located amper "&"], Location.Located dollar $ Token.SymbolIdentifier [Location.Located dollar "$"]], [LexError.InvalidDoubleColon dc1, LexError.InvalidDoubleColon dc2])
    @=?
    runWriter (group_identifiers
        [ Location.Located star $ Token.SymbolIdentifier (Location.Located star "*")
        , Location.Located dc1 (Token.DoubleColon ())
        , Location.Located amper $ Token.SymbolIdentifier (Location.Located amper "&")
        , Location.Located dc2 (Token.DoubleColon ())
        , Location.Located dollar $ Token.SymbolIdentifier (Location.Located dollar "$")
        ])

tests :: TestTree
tests = $(testGroupGenerator)
