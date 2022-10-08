module UHF.Diagnostic.FormattedString
    ( FormattedString(..)
    , ColorsNeeded(..)

    , render_formatted_string

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

data FormattedString
    = Colored [ANSI.SGR] FormattedString
    | Join FormattedString FormattedString
    | Literal Text

data ColorsNeeded = Colors | NoColors | AutoDetect

render_formatted_string :: IO.Handle -> ColorsNeeded -> FormattedString -> IO ()
render_formatted_string handle c_needed fs =
    case c_needed of
        Colors -> pure True
        NoColors -> pure False
        AutoDetect -> ANSI.hSupportsANSI handle
    >>= \ c_needed' ->

    render_formatted_string' handle c_needed' [] fs

{-
    let (_, puts) =
            List.mapAccumL
                (\ remaining (sgrs, len) ->
                    let (cur, remaining') = Text.splitAt len remaining
                    in (remaining',
                        (if c_needed' then ANSI.hSetSGR handle sgrs else pure ()) >>
                        Text.IO.hPutStr handle cur)
                )
                str
                formats

    in sequence_ puts
    -}

render_formatted_string' :: IO.Handle -> Bool -> [ANSI.SGR] -> FormattedString -> IO ()
render_formatted_string' handle c_needed old_sgrs (Colored sgrs text) =
    ANSI.setSGR [] >> ANSI.hSetSGR handle old_sgrs >> ANSI.setSGR sgrs >>
    render_formatted_string' handle c_needed (old_sgrs ++ sgrs) text >>
    ANSI.setSGR [] >> ANSI.hSetSGR handle old_sgrs

render_formatted_string' handle c_needed old_srgs (Join a b) = render_formatted_string' handle c_needed old_srgs a >> render_formatted_string' handle c_needed old_srgs b
render_formatted_string' handle c_needed _ (Literal t) = hPutStr handle t
{-
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
-}

-- tests {{{1
{-
-- case_compare_formatted_string :: Assertion
-- case_compare_formatted_string =
    assertBool "compare_formatted_string failed" $
        compare_formatted_string
            [ ('a', [Colors.fg_bred])
            , ('b', [Colors.fg_bgreen])
            , ('c', [Colors.fg_bblue])
            ]
            "abcdefghijklmnop"
            "a--- --b -c---  "
            (make_formatted_string [([Colors.fg_bred], "abcd"), ([], "efg"), ([Colors.fg_bgreen], "h"), ([], "ij"), ([Colors.fg_bblue], "klmn"), ([], "o"), ([], "p")])
-}

tests :: TestTree
tests = $(testGroupGenerator)
