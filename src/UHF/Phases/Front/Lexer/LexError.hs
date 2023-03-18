module UHF.Phases.Front.Lexer.LexError
    ( Error(..)
    ) where

import UHF.Util.Prelude

import UHF.IO.Span (Span)
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes

import qualified Data.Text as Text

data Error
    = BadChar Char Span
    | UnclosedMComment Span
    | UnclosedStrLit Span
    | UnclosedCharLit Span
    | MulticharCharLit Span
    | InvalidIntBase Char Span
    | InvalidIntDigit Char Span
    | NonDecimalFloat Span
    | MissingDigits Span
    | InvalidDoubleColon Span
    deriving Show

instance Diagnostic.ToError Error where
    to_error (BadChar ch sp) = Diagnostic.Error Codes.bad_char (Just sp) ("bad character '" <> Text.singleton ch <> "'") [] []

    to_error (UnclosedMComment sp) = Diagnostic.Error Codes.unclosed_multiline_comment (Just sp) "unclosed multiline comment" [] []

    to_error (UnclosedStrLit sp) = Diagnostic.Error Codes.unclosed_string_lit (Just sp) "unclosed string literal" [] []

    to_error (UnclosedCharLit sp) = Diagnostic.Error Codes.unclosed_char_lit (Just sp) "unclosed character literal" [] []

    to_error (MulticharCharLit sp) = Diagnostic.Error Codes.invalid_char_lit (Just sp) "character literal not exactly 1 character long" [] []

    to_error (InvalidIntBase ch sp) = Diagnostic.Error Codes.invalid_int_base (Just sp) ("invalid base '" <> Text.singleton ch <> "'") [] []

    to_error (InvalidIntDigit ch sp) = Diagnostic.Error Codes.invalid_int_digit (Just sp) ("invalid digit '" <> Text.singleton ch <> "'") [] []

    to_error (NonDecimalFloat sp) = Diagnostic.Error Codes.non_decimal_float (Just sp) "floating point literals must be in decimal" [] []

    to_error (MissingDigits sp) = Diagnostic.Error Codes.missing_digits (Just sp) "missing digits from number literal" [] []

    to_error (InvalidDoubleColon sp) = Diagnostic.Error Codes.invalid_double_colon (Just sp) "double colon is not allowed outside of paths" [] []
