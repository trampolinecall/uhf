{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Parts.Parser.Error
    ( Error (..)
    ) where

import UHF.Prelude

import qualified Data.Text as Text
import qualified UHF.Data.Token as Token
import qualified UHF.Diagnostic as Diagnostic
import UHF.Source.EqIgnoringSpans
import UHF.Source.Located (Located (Located))
import qualified UHF.Source.Located as Located
import UHF.Source.Span (Span)

data Error = BadToken [Token.TokenType] Token.LToken
    deriving (Generic, EqIgnoringSpans, Show, Eq)

instance Diagnostic.ToError Error where
    to_error (BadToken expected got) =
        let sp = Located.just_span got
            msg = "expected one of " <> Text.intercalate ", " (map format expected) <> " but got " <> format (Located.unlocate got)
        in Diagnostic.Error (Just sp) ("parse error: " <> msg) [sp `Diagnostic.msg_error_at` msg] [] -- TODO: make message better
