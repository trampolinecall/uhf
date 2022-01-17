module UHF.Parser.ParseError
    ( ParseError(..)
    ) where

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Text as Text

data ParseError
    = ParseError
    deriving (Eq, Show)

instance Diagnostic.IsDiagnostic ParseError where
