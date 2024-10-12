{-# LANGUAGE FlexibleInstances #-}

module UHF.Diagnostic.Styles.JSON (JSON (..)) where

import UHF.Prelude

import UHF.Source.File (File)
import UHF.Source.Span (Span)
import qualified Text.JSON as JSON
import qualified UHF.Diagnostic.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Report as Report
import qualified UHF.Source.File as File
import qualified UHF.Source.Span as Span

data JSON = JSON

instance Report.DiagnosticStyle JSON where
    report_error          JSON _ handle e =
        let (Diagnostic.Error         sp main_message messages sections) = Diagnostic.to_error e
        in hPutStrLn handle $ JSON.showJSValue (diagnostic_info_to_json sp main_message messages sections) ""
    report_warning        JSON _ handle w =
        let (Diagnostic.Warning       sp main_message messages sections) = Diagnostic.to_warning w
        in hPutStrLn handle $ JSON.showJSValue (diagnostic_info_to_json sp main_message messages sections) ""
    report_debug_message  JSON _ handle d =
        let (Diagnostic.DebugMessage  sp main_message messages sections) = d
        in hPutStrLn handle $ JSON.showJSValue (diagnostic_info_to_json sp main_message messages sections) ""
    report_internal_error JSON _ handle i =
        let (Diagnostic.InternalError sp main_message messages sections) = i
        in hPutStrLn handle $ JSON.showJSValue (diagnostic_info_to_json sp main_message messages sections) ""

class ToJSON a where
    to_json :: a -> JSON.JSValue

instance ToJSON Text where
    to_json t = JSON.JSString $ JSON.toJSString $ convert_str t
instance ToJSON a => ToJSON (Maybe a) where
    to_json (Just a) =
        JSON.JSObject $ JSON.toJSObject [("maybe", JSON.JSBool True), ("data", to_json a)]
    to_json Nothing =
        JSON.JSObject $ JSON.toJSObject [("maybe", JSON.JSBool False)]

instance ToJSON File where
    to_json f =
        JSON.JSObject $ JSON.toJSObject [("path", JSON.showJSON $ File.path f)]
instance ToJSON Span where
    to_json sp =
        JSON.JSObject $ JSON.toJSObject
            [ ("file", to_json $ Span.file sp)
            , ("start", JSON.showJSON $ Span.start_ind sp)
            , ("end", JSON.showJSON $ Span.end_ind sp)
            ]

instance ToJSON Diagnostic.MessageType where
    to_json Diagnostic.MsgError = to_json ("error" :: Text)
    to_json Diagnostic.MsgWarning = to_json ("warning" :: Text)
    to_json Diagnostic.MsgNote = to_json ("note" :: Text)
    to_json Diagnostic.MsgHint = to_json ("hint" :: Text)
instance ToJSON Diagnostic.Message where
    to_json (sp, ty, message) =
        JSON.JSObject $ JSON.toJSObject
            [ ("sp", to_json sp)
            , ("type", to_json ty)
            , ("message", to_json message)
            ]
instance ToJSON Diagnostic.Section where
    to_json (Diagnostic.Section'Messages m) =
        JSON.JSObject $ JSON.toJSObject
            [ ("type", to_json ("messages" :: Text))
            , ("data", JSON.JSArray $ map to_json m)
            ]

diagnostic_info_to_json :: Maybe Span -> Text -> Diagnostic.MessagesSection -> [Diagnostic.Section] -> JSON.JSValue
diagnostic_info_to_json sp main_message messages sections =
    JSON.JSObject $ JSON.toJSObject
        [ ("sp", to_json sp)
        , ("main_message", to_json main_message)
        , ("messages", JSON.JSArray $ map to_json messages)
        , ("sections", JSON.JSArray $ map to_json sections)
        ]
