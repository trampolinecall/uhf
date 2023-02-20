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
    , msg_error_at, msg_warning_at, msg_note_at, msg_hint_at
    , msg_error_noloc, msg_warning_noloc, msg_note_noloc, msg_hint_noloc

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
class IsWarning w where to_warning :: w -> Warning

type MessagesSection = [Message]
type Message = (Maybe Span, MessageType, Maybe Text)
data MessageType = MsgError | MsgWarning | MsgNote | MsgHint deriving (Show, Eq)

msg_error_at, msg_warning_at, msg_note_at, msg_hint_at :: Span -> Text -> Message
msg_error_at s m = (Just s, MsgError, Just m)
msg_warning_at s m = (Just s, MsgWarning, Just m)
msg_note_at s m = (Just s, MsgNote, Just m)
msg_hint_at s m = (Just s, MsgHint, Just m)
msg_error_noloc, msg_warning_noloc, msg_note_noloc, msg_hint_noloc :: Text -> Message
msg_error_noloc m = (Nothing, MsgError, Just m)
msg_warning_noloc m = (Nothing, MsgWarning, Just m)
msg_note_noloc m = (Nothing, MsgNote, Just m)
msg_hint_noloc m = (Nothing, MsgHint, Just m)

data OtherSection
    = Section'Messages MessagesSection
