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
    = BadToken Int Token.LToken Token.TokenType Text
    -- | NoneMatched Token.LToken [Error]
    deriving (Eq, Show)

data OtherError
    = NotImpl (Location.Located Text)
    | BindToPath (Location.Located [Location.Located Text])
    deriving (Eq, Show)

instance Diagnostic.IsError (Location.Located [BacktrackingError]) where
    -- TODO
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

instance Diagnostic.IsError OtherError where
    to_error (NotImpl construct) = Diagnostic.Error Codes.not_implemented $
        Diagnostic.DiagnosticContents
            (Just $ Location.just_span construct)
            [ Underlines.underlines
                [Location.just_span construct `Underlines.primary` [Underlines.error $ Literal (Location.unlocate construct) <> " are not implemented yet"]]
            ]

    to_error (BindToPath (Location.Located sp _)) = Diagnostic.Error Codes.binding_lhs_path $
        Diagnostic.DiagnosticContents
            (Just sp)
            [ Underlines.underlines
                [sp `Underlines.primary` [Underlines.error "path in left-hand side of binding"]]
            ]
