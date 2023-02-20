{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Diagnostic.Diagnostic
    ( Error(..)
    , Warning(..)
    , DebugMessage(..)
    , InternalError(..)

    , ToError(..)
    , IsWarning(..)

    , MessagesSection
    , Message
    , MessageType(..)
    , msg_error, msg_warning, msg_note, msg_hint

    , OtherSection(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.Codes.Code as Code

import UHF.IO.Location (Span)

data Error = Error Code.Error (Maybe Span) Text MessagesSection [OtherSection]
data Warning = Warning Code.Warning (Maybe Span) Text MessagesSection [OtherSection]
data DebugMessage = DebugMessage (Maybe Span) Text MessagesSection [OtherSection]
data InternalError = InternalError (Maybe Span) Text MessagesSection [OtherSection]

class ToError e where to_error :: e -> Error
class IsWarning w where to_warning :: w -> Warning

type MessagesSection = [Message]
type Message = (Span, MessageType, Maybe Text)
data MessageType = MsgError | MsgWarning | MsgNote | MsgHint deriving (Show, Eq)

msg_error, msg_warning, msg_note, msg_hint :: Span -> Text -> Message
msg_error s m = (s, MsgError, Just m)
msg_warning s m = (s, MsgWarning, Just m)
msg_note s m = (s, MsgNote, Just m)
msg_hint s m = (s, MsgHint, Just m)

data OtherSection
    = Section'Messages MessagesSection
