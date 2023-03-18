{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module UHF.Diagnostic.Diagnostic
    ( Error(..)
    , Warning(..)
    , DebugMessage(..)
    , InternalError(..)

    , ToError(..)
    , ToWarning(..)

    , MessagesSection
    , Message
    , MessageType(..)
    , msg_error_at, msg_warning_at, msg_note_at, msg_hint_at
    , msg_error, msg_warning, msg_note, msg_hint

    , OtherSection(..)
    ) where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.Codes.Code as Code

import UHF.IO.Span (Span)

data Error = Error Code.Error (Maybe Span) Text MessagesSection [OtherSection]
data Warning = Warning Code.Warning (Maybe Span) Text MessagesSection [OtherSection]
data DebugMessage = DebugMessage (Maybe Span) Text MessagesSection [OtherSection]
data InternalError = InternalError (Maybe Span) Text MessagesSection [OtherSection]

class ToError e where to_error :: e -> Error
class ToWarning w where to_warning :: w -> Warning

instance ToError Error where to_error = identity
instance ToWarning Warning where to_warning = identity
instance ToError Void where to_error = absurd
instance ToWarning Void where to_warning = absurd

type MessagesSection = [Message]
type Message = (Maybe Span, MessageType, Maybe Text)
data MessageType = MsgError | MsgWarning | MsgNote | MsgHint deriving (Show, Eq)

msg_error_at, msg_warning_at, msg_note_at, msg_hint_at :: Span -> Text -> Message
msg_error_at s m = (Just s, MsgError, Just m)
msg_warning_at s m = (Just s, MsgWarning, Just m)
msg_note_at s m = (Just s, MsgNote, Just m)
msg_hint_at s m = (Just s, MsgHint, Just m)
msg_error, msg_warning, msg_note, msg_hint :: Text -> Message
msg_error m = (Nothing, MsgError, Just m)
msg_warning m = (Nothing, MsgWarning, Just m)
msg_note m = (Nothing, MsgNote, Just m)
msg_hint m = (Nothing, MsgHint, Just m)

newtype OtherSection
    = Section'Messages MessagesSection
