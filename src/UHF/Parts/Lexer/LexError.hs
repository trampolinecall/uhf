module UHF.Parts.Lexer.LexError
    ( Error(..)
    ) where

import UHF.Prelude

import UHF.Source.Span (Span)
import qualified Data.Text as Text
import qualified UHF.Diagnostic as Diagnostic

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
    to_error (BadChar ch sp) = Diagnostic.Error (Just sp) ("bad character '" <> Text.singleton ch <> "'") [] []

    to_error (UnclosedMComment sp) = Diagnostic.Error (Just sp) "unclosed multiline comment" [] []

    to_error (UnclosedStrLit sp) = Diagnostic.Error (Just sp) "unclosed string literal" [] []

    to_error (UnclosedCharLit sp) = Diagnostic.Error (Just sp) "unclosed character literal" [] []

    to_error (MulticharCharLit sp) = Diagnostic.Error (Just sp) "character literal not exactly 1 character long" [] []

    to_error (InvalidIntBase ch sp) = Diagnostic.Error (Just sp) ("invalid base '" <> Text.singleton ch <> "'") [] []

    to_error (InvalidIntDigit ch sp) = Diagnostic.Error (Just sp) ("invalid digit '" <> Text.singleton ch <> "'") [] []

    to_error (NonDecimalFloat sp) = Diagnostic.Error (Just sp) "floating point literals must be in decimal" [] []

    to_error (MissingDigits sp) = Diagnostic.Error (Just sp) "missing digits from number literal" [] []

    to_error (InvalidDoubleColon sp) = Diagnostic.Error (Just sp) "double colon is not allowed outside of paths" [] []
