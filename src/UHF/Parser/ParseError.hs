module UHF.Parser.ParseError
    ( ParseError(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Token as Token

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

data ParseError
    = BadToken Token.LNormalToken Token.TokenType Text
    | NoneMatched Token.LNormalToken [ParseError]
    | NotImpl (Location.Located Text)
    deriving (Eq, Show)

instance Diagnostic.IsDiagnostic ParseError where
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

    to_diagnostic (NotImpl construct) =
        Diagnostic.Diagnostic Codes.not_implemented (Just $ Location.just_span construct)
            [ Underlines.underlines
                [Location.just_span construct `Underlines.primary` [Underlines.error $ Literal (Location.unlocate construct) <> " are not implemented yet"]]
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
