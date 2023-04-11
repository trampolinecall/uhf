module UHF.IO.FormattedString
    ( FormattedString(..)
    , ColorsNeeded(..)
    , color_text

    , render
    , flatten_no_sgr
    ) where

import Prelude -- use normal prelude because UHF.Prelude reexports this

import Data.String (IsString(..))
import qualified System.Console.ANSI as ANSI
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.IO as IO

data ColorsNeeded = Colors | NoColors | AutoDetect

data FormattedString
    = Colored [ANSI.SGR] FormattedString
    | Join FormattedString FormattedString
    | Literal Text.Text
    deriving (Show, Eq)

instance IsString FormattedString where
    fromString = Literal . Text.pack
instance Semigroup FormattedString where
    (<>) = Join
instance Monoid FormattedString where
    mempty = Literal ""

color_text :: [ANSI.SGR] -> Text.Text -> FormattedString
color_text sgr = Colored sgr . Literal

render :: IO.Handle -> ColorsNeeded -> FormattedString -> IO ()
render handle c_needed fs =
    case c_needed of
        Colors -> pure True
        NoColors -> pure False
        AutoDetect -> ANSI.hSupportsANSI handle
    >>= \ c_needed' ->
    render' handle c_needed' [] fs

render' :: IO.Handle -> Bool -> [ANSI.SGR] -> FormattedString -> IO ()
render' handle c_needed old_sgrs (Colored sgrs text) =
    maybe_set_sgr c_needed handle [] >> maybe_set_sgr c_needed handle old_sgrs >> maybe_set_sgr c_needed handle sgrs >>
    render' handle c_needed (old_sgrs ++ sgrs) text >>
    maybe_set_sgr c_needed handle [] >> maybe_set_sgr c_needed handle old_sgrs

render' handle c_needed old_srgs (Join a b) = render' handle c_needed old_srgs a >> render' handle c_needed old_srgs b
render' handle _ _ (Literal t) = Text.IO.hPutStr handle t

maybe_set_sgr :: Bool -> IO.Handle -> [ANSI.SGR] -> IO ()
maybe_set_sgr False _ _ = pure ()
maybe_set_sgr True handle sgrs = ANSI.hSetSGR handle sgrs

flatten_no_sgr :: FormattedString -> Text.Text
flatten_no_sgr (Colored _ a) = flatten_no_sgr a
flatten_no_sgr (Join a b) = flatten_no_sgr a <> flatten_no_sgr b
flatten_no_sgr (Literal t) = t
