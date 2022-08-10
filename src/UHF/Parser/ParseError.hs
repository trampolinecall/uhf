module UHF.Parser.ParseError
    ( ParseError(..)
    ) where

import qualified UHF.Token as Token

import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.Diagnostic.Sections.Underlines as Underlines

import qualified Data.Text as Text

data ParseError
    = BadToken Token.LToken Token.Token String (Maybe String)
    | NoneMatched Token.LToken [ParseError]
    | ExpectedAtLeastOne Token.LToken [ParseError]
    | NotImpl (Location.Located String)
    deriving (Eq, Show)

instance Diagnostic.IsDiagnostic ParseError where
    to_diagnostic (BadToken tok expectation construct m_component) =
        let sp = Location.just_span tok
        in Diagnostic.Diagnostic Codes.bad_token (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary`
                    [ Underlines.error $ Text.pack $ "bad " ++ Token.format_tok (Location.unlocate tok)
                    , Underlines.note $ Text.pack $ construct ++ " expects " ++ Token.format_tok expectation ++
                        (case m_component of
                            Just c -> " as " ++ c
                            Nothing -> "")
                    ]
                ]
            ]

    to_diagnostic (NotImpl construct) =
        Diagnostic.Diagnostic Codes.not_implemented (Just $ Location.just_span construct)
            [ Underlines.underlines
                [Location.just_span construct `Underlines.primary` [Underlines.error $ Text.pack $ Location.unlocate construct ++ " are not implemented yet"]]
            ]
