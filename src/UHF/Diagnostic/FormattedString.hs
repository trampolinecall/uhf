module UHF.Diagnostic.FormattedString
    ( FormattedString

    , ColorsNeeded(..)

    , make_formatted_string
    , render_formatted_string
    , compare_formatted_string

    , formatted_string_contents_and_formats

    , tests
    ) where

import UHF.Util.Prelude


import qualified UHF.Diagnostic.Colors as Colors

import qualified Data.Text as Text
import qualified Data.Tuple as Tuple
import qualified Data.Maybe as Maybe
import qualified System.Console.ANSI as ANSI

data FormattedString = FormattedString Text [([ANSI.SGR], Int)] deriving (Eq, Show)

data ColorsNeeded = Colors | NoColors | AutoDetect

make_formatted_string :: [([ANSI.SGR], Text)] -> FormattedString
make_formatted_string str =
    let (_, formats) =
            mapAccumL
                (\ start (sgrs, t) ->
                    let len = Text.length t
                        end = start + len
                    in if len == 0
                        then (end, [])
                        else (end, [(sgrs, len), ([ANSI.Reset], 0)])
                )
                0
                str

    in FormattedString (Text.concat $ map snd str) (concat formats)

render_formatted_string :: Handle -> ColorsNeeded -> FormattedString -> IO ()
render_formatted_string handle c_needed (FormattedString str formats) =
    case c_needed of
        Colors -> pure True
        NoColors -> pure False
        AutoDetect -> ANSI.hSupportsANSI handle
    >>= \ c_needed' ->

    let (_, puts) =
            mapAccumL
                (\ remaining (sgrs, len) ->
                    let (cur, remaining') = Text.splitAt len remaining
                    in (remaining',
                        (if c_needed' then ANSI.hSetSGR handle sgrs else pure ()) >>
                        hPutStr handle cur)
                )
                str
                formats

    in sequence_ puts

formatted_string_contents_and_formats :: [(Char, [ANSI.SGR])] -> FormattedString -> (Text, Text)
formatted_string_contents_and_formats bindings (FormattedString text formats) =
    let reverse_bindings = ([], ' ') : map Tuple.swap bindings
    in ( text
       , Text.pack $ concatMap
           (\ (sgrs, amt) ->
               if amt == 0
                   then ""
                   else Maybe.fromJust (lookup sgrs reverse_bindings) : replicate (amt - 1) '-')
           formats
       )
-- for testing
compare_formatted_string :: [(Char, [ANSI.SGR])] -> Text -> Text -> FormattedString -> Bool
compare_formatted_string bindings text sgrs =
    let bindings' = (' ', []) : bindings
        group_sgrs ((ch, sgr_binding):more) =
            let (grabbed_chrs, more') = span ((=='-') . snd) more
                sgr =
                    case lookup sgr_binding bindings' of
                        Just x -> x
                        Nothing -> error $ "undefined sgr character: '" ++ [sgr_binding] ++ "'"

            in ((sgr, Text.pack $ ch : map fst grabbed_chrs) : group_sgrs more')

        group_sgrs [] = []

    in (make_formatted_string (group_sgrs $ Text.zip text sgrs) ==)

-- tests {{{1
case_compare_formatted_string :: Assertion
case_compare_formatted_string =
    assertBool "compare_formatted_string failed" $
        compare_formatted_string
            [ ('a', [Colors.fg_bred])
            , ('b', [Colors.fg_bgreen])
            , ('c', [Colors.fg_bblue])
            ]
            "abcdefghijklmnop"
            "a--- --b -c---  "
            (make_formatted_string [([Colors.fg_bred], "abcd"), ([], "efg"), ([Colors.fg_bgreen], "h"), ([], "ij"), ([Colors.fg_bblue], "klmn"), ([], "o"), ([], "p")])

case_make_formatted_string :: Assertion
case_make_formatted_string =
    FormattedString "text a b cdef"
        [([Colors.bold], 5), ([Colors.reset], 0), ([Colors.bg_red], 2), ([Colors.reset], 0), ([Colors.fg_bgreen], 2), ([Colors.reset], 0), ([Colors.fg_blue], 1), ([Colors.reset], 0), ([], 3), ([Colors.reset], 0)]
            @=?
    make_formatted_string
        [([Colors.bold], "text "), ([Colors.bg_red], "a "), ([Colors.fg_bgreen], "b "), ([Colors.fg_blue], "c"), ([], "def")]

tests :: TestTree
tests = $(testGroupGenerator)
