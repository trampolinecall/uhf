{-# LANGUAGE DeriveLift #-}

module UHF.Diagnostic.Codes.Code
    ( Error(..)
    , Warning(..)
    ) where

import UHF.Util.Prelude

import qualified Language.Haskell.TH.Syntax as Syntax

newtype Error = Error { error_code_desc :: Maybe (Text, Text) }
    deriving Syntax.Lift

newtype Warning = Warning { warning_code_desc :: Maybe (Text, Text) }
    deriving Syntax.Lift
