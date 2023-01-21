module UHF.FormattedString
    ( FormattedString(..)
    , ColorsNeeded(..)
    , color_text

    , UHF.FormattedString.length

    , render_formatted_string
    , flatten_no_sgr
    ) where

import Prelude -- use normal prelude because UHF.Prelude reexports this

import Data.String (IsString(..))
import qualified System.Console.ANSI as ANSI
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.IO as IO

data FormattedString
    = Colored [ANSI.SGR] FormattedString
    | Join FormattedString FormattedString
    | Literal Text.Text
    deriving (Show, Eq)

instance IsString FormattedString where
    fromString = Literal . Text.pack
instance Semigroup FormattedString where
    (<>) = Join

data ColorsNeeded = Colors | NoColors | AutoDetect

color_text :: [ANSI.SGR] -> Text.Text -> FormattedString
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
    -- TODO: actually following c_needed
    ANSI.hSetSGR handle [] >> ANSI.hSetSGR handle old_sgrs >> ANSI.hSetSGR handle sgrs >>
    render_formatted_string' handle c_needed (old_sgrs ++ sgrs) text >>
    ANSI.hSetSGR handle [] >> ANSI.hSetSGR handle old_sgrs

render_formatted_string' handle c_needed old_srgs (Join a b) = render_formatted_string' handle c_needed old_srgs a >> render_formatted_string' handle c_needed old_srgs b
render_formatted_string' handle _ _ (Literal t) = Text.IO.hPutStr handle t

length :: FormattedString -> Int
length = Text.length . flatten_no_sgr

flatten_no_sgr :: FormattedString -> Text.Text
flatten_no_sgr (Colored _ a) = flatten_no_sgr a
flatten_no_sgr (Join a b) = flatten_no_sgr a <> flatten_no_sgr b
flatten_no_sgr (Literal t) = t
