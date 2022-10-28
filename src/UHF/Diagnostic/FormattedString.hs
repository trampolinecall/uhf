module UHF.Diagnostic.FormattedString
    ( FormattedString(..)
    , ColorsNeeded(..)
    , color_text

    , UHF.Diagnostic.FormattedString.length

    , render_formatted_string
    , flatten_no_sgr
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text
import qualified System.Console.ANSI as ANSI
import qualified System.IO as IO

data ColorsNeeded = Colors | NoColors | AutoDetect

color_text :: [ANSI.SGR] -> Text -> FormattedString
color_text sgr = Colored sgr . Literal

render_formatted_string :: IO.Handle -> ColorsNeeded -> FormattedString -> IO ()
render_formatted_string handle c_needed fs =
    case c_needed of
        Colors -> pure True
        NoColors -> pure False
        AutoDetect -> ANSI.hSupportsANSI handle
    >>= \ c_needed' ->
    render_formatted_string' handle c_needed' [] fs


render_formatted_string' :: IO.Handle -> Bool -> [ANSI.SGR] -> FormattedString -> IO ()
render_formatted_string' handle c_needed old_sgrs (Colored sgrs text) =
    ANSI.setSGR [] >> ANSI.hSetSGR handle old_sgrs >> ANSI.setSGR sgrs >>
    render_formatted_string' handle c_needed (old_sgrs ++ sgrs) text >>
    ANSI.setSGR [] >> ANSI.hSetSGR handle old_sgrs

render_formatted_string' handle c_needed old_srgs (Join a b) = render_formatted_string' handle c_needed old_srgs a >> render_formatted_string' handle c_needed old_srgs b
render_formatted_string' handle _ _ (Literal t) = hPutStr handle t

length :: FormattedString -> Int
length = Text.length . flatten_no_sgr

flatten_no_sgr :: FormattedString -> Text
flatten_no_sgr (Colored _ a) = flatten_no_sgr a
flatten_no_sgr (Join a b) = flatten_no_sgr a <> flatten_no_sgr b
flatten_no_sgr (Literal t) = t
