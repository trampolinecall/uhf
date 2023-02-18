{-# LANGUAGE OverloadedStrings #-}
module UHF.IO.Location.SpanHelper
    ( make_spans
    , make_spans'
    , make_spans_with_items
    , make_spans_with_items'
    , make_spans_with_show_items
    , make_spans_with_show_items'
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.Location as Location

import qualified Data.Text as Text

make_spans :: [Text] -> (Location.File, [Location.Span])
make_spans = make_spans' "" " "

make_spans' :: FilePath -> Text -> [Text] -> (Location.File, [Location.Span])
make_spans' fname intercalation strs =
    let combined = Text.intercalate intercalation strs
        file = Location.new_file fname combined

        (_, spans) =
            mapAccumL
                (\ start s ->
                    (Location.seek (Text.length s + Text.length intercalation) start, Location.new_span start 0 (Text.length s))
                )
                (Location.new_location file)
                strs

    in (file, spans)

make_spans_with_items :: [(Text, a)] -> (Location.File, [Location.Located a])
make_spans_with_items = make_spans_with_items' "" " "

make_spans_with_items' :: FilePath -> Text -> [(Text, a)] -> (Location.File, [Location.Located a])
make_spans_with_items' fname intercalation strs =
    let (f, sps) = make_spans' fname intercalation (fst <$> strs)
    in (f, zipWith Location.Located sps (snd <$> strs))

make_spans_with_show_items :: Show a => [a] -> (Location.File, [Location.Located a])
make_spans_with_show_items = make_spans_with_show_items' "" " "

make_spans_with_show_items' :: Show a => FilePath -> Text -> [a] -> (Location.File, [Location.Located a])
make_spans_with_show_items' fname intercalation things =
    let (f, sps) = make_spans' fname intercalation (show <$> things)
    in (f, zipWith Location.Located sps things)
