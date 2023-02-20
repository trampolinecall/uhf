{-# LANGUAGE FlexibleInstances #-}

module UHF.Parser.Error
    ( Error(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Token as Token

import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Codes as Codes
import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located (Located))

data Error
    = BadToken Int Token.LToken Token.TokenType Text
    -- | NoneMatched Token.LToken [Error]
    deriving (Eq, Show)

instance Diagnostic.ToError (Located [Error]) where
    to_error (Located sp bits) =
        Diagnostic.Error
            Codes.parse_error
            (Just sp)
            "parse error" -- TODO
            (map
                (\ (BadToken _ tok expectation construct) ->
                    Located.just_span tok `Diagnostic.msg_error` convert_str (construct <> " expects " <> format expectation <> " but got " <> format (Located.unlocate tok)))
                bits) -- TODO: make this better
            []
