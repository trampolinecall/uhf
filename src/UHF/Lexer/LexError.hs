module UHF.Lexer.LexError
    ( LexError(..)
    ) where

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Text as Text

data LexError
    = BadChar Char Location.Span
    | UnclosedMComment Location.Span
    | UnclosedStrLit Location.Span
    | UnclosedCharLit Location.Span
    | MulticharCharLit Location.Span
    | InvalidIntBase Char Location.Span
    | InvalidIntDigit Char Location.Span
    | NonDecimalFloat Location.Span
    | MissingDigits Location.Span
    -- TODO: add 4 fields: new indent level, before indent level, two closest indentation levels
    | BadDedent Location.Span
    | InvalidDoubleColon Location.Span
    deriving (Eq, Show)

instance Diagnostic.IsDiagnostic LexError where
    to_diagnostic (BadChar ch sp) = Diagnostic.Diagnostic Codes.bad_char (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "bad character '" `Text.append` Text.singleton ch `Text.append` "'"]]]

    to_diagnostic (UnclosedMComment sp) = Diagnostic.Diagnostic Codes.unclosed_multiline_comment (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "unclosed multiline comment"]]]

    to_diagnostic (UnclosedStrLit sp) = Diagnostic.Diagnostic Codes.unclosed_string_lit (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "unclosed string literal"]]]

    to_diagnostic (UnclosedCharLit sp) = Diagnostic.Diagnostic Codes.unclosed_char_lit (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "unclosed character literal"]]]

    to_diagnostic (MulticharCharLit sp) = Diagnostic.Diagnostic Codes.invalid_char_lit (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "character literal not exactly 1 character long"]]]

    to_diagnostic (InvalidIntBase ch sp) = Diagnostic.Diagnostic Codes.invalid_int_base (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "invalid base '" `Text.append` Text.singleton ch `Text.append` "'"]]]

    to_diagnostic (InvalidIntDigit ch sp) = Diagnostic.Diagnostic Codes.invalid_int_digit (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error $ "invalid digit '" `Text.append` Text.singleton ch `Text.append` "'"]]]

    to_diagnostic (NonDecimalFloat sp) = Diagnostic.Diagnostic Codes.non_decimal_float (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "floating point literals must be in decimal"]]]

    to_diagnostic (MissingDigits sp) = Diagnostic.Diagnostic Codes.missing_digits (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "missing digits from number literal"]]]

    to_diagnostic (BadDedent sp) = Diagnostic.Diagnostic Codes.bad_dedent (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "dedent to unknown level"]]]

    to_diagnostic (InvalidDoubleColon sp) = Diagnostic.Diagnostic Codes.invalid_double_colon (Just sp)
        [Underlines.underlines [sp `Underlines.primary` [Underlines.error "double colon is not allowed outside of paths"]]]
