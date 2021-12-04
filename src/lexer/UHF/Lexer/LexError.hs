module UHF.Lexer.LexError
    ( LexError(..)
    ) where

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Diagnostic.Codes

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
    to_diagnostic = undefined -- TODO
