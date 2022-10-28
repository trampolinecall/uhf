module UHF.Diagnostic.FormattedString
    ( FormattedString(..)
    , ColorsNeeded(..)
    , color_text

    , render_formatted_string
    , flatten_no_sgr

    , tests
    ) where

import UHF.Util.Prelude

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Diagnostic.Colors as Colors

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Tuple as Tuple
import qualified Data.Maybe as Maybe
import qualified System.Console.ANSI as ANSI
import qualified Data.List as List
import qualified System.IO as IO

import qualified Data.String (IsString(..))

data FormattedString
    = Colored [ANSI.SGR] FormattedString
    | Join FormattedString FormattedString
    | Literal Text
    deriving (Show, Eq)

instance Data.String.IsString FormattedString where
    fromString = Literal . Text.pack

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
render_formatted_string' handle c_needed _ (Literal t) = hPutStr handle t

flatten_no_sgr :: FormattedString -> Text
flatten_no_sgr (Colored _ a) = flatten_no_sgr a
flatten_no_sgr (Join a b) = flatten_no_sgr a <> flatten_no_sgr b
flatten_no_sgr (Literal t) = t
