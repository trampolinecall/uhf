{-# LANGUAGE FlexibleInstances #-}

module UHF.Parser.Error
    ( BacktrackingError(..)
    , OtherError(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Token as Token

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

data BacktrackingError
    = BadToken Token.LToken Token.TokenType Text
    -- | NoneMatched Token.LToken [Error]
    deriving (Eq, Show)

data OtherError
    = NotImpl (Location.Located Text)
    deriving (Eq, Show)

instance Diagnostic.IsDiagnostic [BacktrackingError] where
    -- TODO
    {-
    to_diagnostic (BadToken tok expectation construct) =
        let sp = Location.just_span tok
        in Diagnostic.Diagnostic Codes.bad_token (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary`
                    [ Underlines.error $ "bad " <> format (Location.unlocate tok)
                    , Underlines.note $ Literal construct <> " expects " <> format expectation
                    ]
                ]
            ]

    to_diagnostic (NoneMatched (Location.Located sp tok) errs) =
        Diagnostic.Diagnostic Codes.none_matched (Just sp) $
            Underlines.underlines
                [ sp `Underlines.primary`
                    [ Underlines.error "no parser matched tokens" ]
                ]
            :
            -- TODO: make this less janky
            concatMap ((\ (Diagnostic.Diagnostic _ _ sections) -> sections) . Diagnostic.to_diagnostic) errs
    -}

instance Diagnostic.IsDiagnostic OtherError where
    to_diagnostic (NotImpl construct) =
        Diagnostic.Diagnostic Codes.not_implemented (Just $ Location.just_span construct)
            [ Underlines.underlines
                [Location.just_span construct `Underlines.primary` [Underlines.error $ Literal (Location.unlocate construct) <> " are not implemented yet"]]
            ]
