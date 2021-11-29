module UHF.Test.SpanHelper
    ( make_spans
    ) where

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location

import qualified Data.Text as Text
import qualified Data.List as List

make_spans :: [String] -> (File.File, [Location.Span])
make_spans strs =
    let combined = List.intercalate " " strs
        (_, indices) = List.mapAccumL (\ start s -> let end = start + length s in (end, (start, end))) 0 strs

        file = File.File "<generated span file>" (Text.pack combined)
        loc i = Location.Location file i 1 (i + 1)
        sp (start, end) = Location.Span (loc start) (loc $ end - 1) (loc end)

    in (file, map sp indices)
