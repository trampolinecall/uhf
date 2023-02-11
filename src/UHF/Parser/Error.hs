{-# LANGUAGE FlexibleInstances #-}

module UHF.Parser.Error
    ( BacktrackingError(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Token as Token

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

data BacktrackingError
    = BadToken Int Token.LToken Token.TokenType Text
    -- | NoneMatched Token.LToken [Error]
    deriving (Eq, Show)

instance Diagnostic.IsError (Location.Located [BacktrackingError]) where
    to_error (Location.Located sp bits) = Diagnostic.Error Codes.parse_error $
            Diagnostic.DiagnosticContents
                (Just sp)
                [ Underlines.underlines $
                    map
                        (\ (BadToken _ tok expectation construct) ->
                            Location.just_span tok `Underlines.primary`
                                [ Underlines.error $ Literal construct <> " expects " <> format expectation <> " but got " <> format (Location.unlocate tok) ])
                        bits -- TODO: make this better
                ]
