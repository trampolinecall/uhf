{-# LANGUAGE OverloadedStrings #-}
module UHF.Source.Location.SpanHelper
    ( make_spans
    , make_spans'
    , make_spans_with_items
    , make_spans_with_items'
    , make_spans_with_show_items
    , make_spans_with_show_items'
    ) where

import UHF.Prelude

import qualified Data.Text as Text

import UHF.Source.File (File)
import UHF.Source.Located (Located (..))
import UHF.Source.Span (Span)
import qualified UHF.Source.File as File
import qualified UHF.Source.Location as Location
import qualified UHF.Source.Span as Span

make_spans :: [Text] -> IO (File, [Span])
make_spans = make_spans' "" " "

make_spans' :: FilePath -> Text -> [Text] -> IO (File, [Span])
make_spans' fname intercalation strs =
    let combined = Text.intercalate intercalation strs
    in File.new fname combined >>= \ file ->
    let (_, spans) =
            mapAccumL
                (\ start s ->
                    (Location.seek (Text.length s + Text.length intercalation) start, Span.new start 0 (Text.length s))
                )
                (Location.new file)
                strs

    in pure (file, spans)

make_spans_with_items :: [(Text, a)] -> IO (File, [Located a])
make_spans_with_items = make_spans_with_items' "" " "

make_spans_with_items' :: FilePath -> Text -> [(Text, a)] -> IO (File, [Located a])
make_spans_with_items' fname intercalation strs =
    make_spans' fname intercalation (fst <$> strs) >>= \ (f, sps) ->
    pure (f, zipWith Located sps (snd <$> strs))

make_spans_with_show_items :: Show a => [a] -> IO (File, [Located a])
make_spans_with_show_items = make_spans_with_show_items' "" " "

make_spans_with_show_items' :: Show a => FilePath -> Text -> [a] -> IO (File, [Located a])
make_spans_with_show_items' fname intercalation things =
    make_spans' fname intercalation (show <$> things) >>= \ (f, sps) ->
    pure (f, zipWith Located sps things)
