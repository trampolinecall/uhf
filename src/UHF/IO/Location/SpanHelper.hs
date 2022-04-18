{-# LANGUAGE OverloadedStrings #-}
module UHF.IO.Location.SpanHelper
    ( make_spans
    , make_spans'
    , make_spans_with_items
    , make_spans_with_items'
    ) where

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified Data.List as List

make_spans :: [String] -> (File.File, [Location.Span])
make_spans = make_spans' "<generated span file>" " "

make_spans' :: String -> String -> [String] -> (File.File, [Location.Span])
make_spans' fname intercalation strs =
    let combined = Text.pack $ List.intercalate intercalation strs
        file = File.File fname combined

        (_, spans) =
            List.mapAccumL
                (\ start s ->
                    (Location.seek (length s + length intercalation) start, Location.new_span start 0 (length s))
                )
                (Location.new_location file)
                strs

    in (file, spans)

make_spans_with_items :: [(String, a)] -> (File.File, [Location.Located a])
make_spans_with_items = make_spans_with_items' "<generated span file>" " "

make_spans_with_items' :: String -> String -> [(String, a)] -> (File.File, [Location.Located a])
make_spans_with_items' fname intercalation strs =
    let (f, sps) = make_spans' fname intercalation (fst <$> strs)
    in (f, zipWith Location.Located sps (snd <$> strs))
