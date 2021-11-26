module UHF.Diagnostic.Code
    ( Code(..)
    , Type(..)
    ) where

import qualified Data.Text as Text

data Type = Error | Warning | DebugMessage | InternalError
data Code =
    Code
    { code_type :: Type
    , code_code_desc :: Maybe (Text.Text, Text.Text)
    }
