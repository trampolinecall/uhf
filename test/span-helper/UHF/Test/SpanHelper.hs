{-# LANGUAGE OverloadedStrings #-}
module UHF.Test.SpanHelper
    ( make_spans
    , make_spans'
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
