{-# LANGUAGE DeriveLift #-}

module UHF.Diagnostic.Code
    ( Code(..)
    , Type(..)
    ) where

import qualified Data.Text as Text
import qualified Language.Haskell.TH.Syntax as Syntax

data Type = Error | Warning | DebugMessage | InternalError deriving Syntax.Lift
data Code =
    Code
    { code_type :: Type
    , code_code_desc :: Maybe (Text.Text, Text.Text)
    }
    deriving Syntax.Lift
