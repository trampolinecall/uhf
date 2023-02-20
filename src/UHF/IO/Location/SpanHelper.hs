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

import qualified UHF.IO.File as File
import UHF.IO.File (File)
import qualified UHF.IO.Location as Location
import qualified UHF.IO.Span as Span
import UHF.IO.Span (Span)
import UHF.IO.Located (Located (..))

import qualified Data.Text as Text

make_spans :: [Text] -> (File, [Span])
make_spans = make_spans' "" " "

make_spans' :: FilePath -> Text -> [Text] -> (File, [Span])
make_spans' fname intercalation strs =
    let combined = Text.intercalate intercalation strs
        file = File.new fname combined

        (_, spans) =
            mapAccumL
                (\ start s ->
                    (Location.seek (Text.length s + Text.length intercalation) start, Span.new start 0 (Text.length s))
                )
                (Location.new file)
                strs

    in (file, spans)

make_spans_with_items :: [(Text, a)] -> (File, [Located a])
make_spans_with_items = make_spans_with_items' "" " "

make_spans_with_items' :: FilePath -> Text -> [(Text, a)] -> (File, [Located a])
make_spans_with_items' fname intercalation strs =
    let (f, sps) = make_spans' fname intercalation (fst <$> strs)
    in (f, zipWith Located sps (snd <$> strs))

make_spans_with_show_items :: Show a => [a] -> (File, [Located a])
make_spans_with_show_items = make_spans_with_show_items' "" " "

make_spans_with_show_items' :: Show a => FilePath -> Text -> [a] -> (File, [Located a])
make_spans_with_show_items' fname intercalation things =
    let (f, sps) = make_spans' fname intercalation (show <$> things)
    in (f, zipWith Located sps things)
