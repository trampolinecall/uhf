{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module UHF.Phases.Parser.Error
    ( Error(..)
    ) where

import UHF.Util.Prelude

import UHF.IO.EqIgnoringSpans
import UHF.IO.Located (Located (Located))
import qualified UHF.Data.Token as Token
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.IO.Located as Located

data Error
    = BadToken Int Token.LToken Token.TokenType Text
    -- | NoneMatched Token.LToken [Error]
    deriving (Generic, EqIgnoringSpans, Show, Eq)

instance Diagnostic.ToError (Located [Error]) where
    to_error (Located sp bits) =
        Diagnostic.Error
            (Just sp)
            "parse error" -- TODO
            (map
                (\ (BadToken _ tok expectation construct) ->
                    Located.just_span tok `Diagnostic.msg_error_at` convert_str (construct <> " expects " <> format expectation <> " but got " <> format (Located.unlocate tok)))
                bits) -- TODO: make this better
            []
