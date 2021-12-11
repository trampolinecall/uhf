{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module UHF.Diagnostic.FormattedString
    ( FormattedString

    , ColorsNeeded(..)

    , make_formatted_string
    , render_formatted_string

    , tests
    ) where

import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty

import qualified UHF.Diagnostic.Colors as Colors

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Console.ANSI as ANSI
import qualified Data.List as List
import qualified System.IO as IO

data FormattedString = FormattedString Text.Text [([ANSI.SGR], Int)] deriving (Eq, Show)

data ColorsNeeded = Colors | NoColors | AutoDetect

make_formatted_string :: [([ANSI.SGR], Text.Text)] -> FormattedString
make_formatted_string str =
    let (_, formats) =
            List.mapAccumL
                (\ start (sgrs, t) ->
                    let len = Text.length t
                        end = start + len
                    in (end, [(sgrs, len), ([ANSI.Reset], 0)])
                )
                0
                str

    in FormattedString (Text.concat $ map snd str) (concat formats)

render_formatted_string :: IO.Handle -> ColorsNeeded -> FormattedString -> IO ()
render_formatted_string handle c_needed (FormattedString str formats) =
    case c_needed of
        Colors -> return True
        NoColors -> return False
        AutoDetect -> ANSI.hSupportsANSI handle
    >>= \ c_needed' ->

    let (_, puts) =
            List.mapAccumL
                (\ remaining (sgrs, len) ->
                    let (cur, remaining') = Text.splitAt len remaining
                    in (remaining',
                        (if c_needed' then ANSI.setSGR sgrs else return ()) >>
                        Text.IO.putStr cur)
                )
                str
                formats

    in sequence_ puts

-- for testing
compare_formatted_string :: [(Char, [ANSI.SGR])] -> String -> String -> FormattedString -> Bool
compare_formatted_string bindings text sgrs =
    let group_sgrs textsgrs@((_, ' '):_) =
            let (grabbed_chrs, more) = span ((==' ') . snd) textsgrs
            in (([], Text.pack $ map fst grabbed_chrs) : group_sgrs more)

        group_sgrs ((ch, sgr_binding):more) =
            let (grabbed_chrs, more') = span ((=='-') . snd) more
                sgr =
                    case lookup sgr_binding bindings of
                        Just x -> x
                        Nothing -> error $ "undefined sgr character: '" ++ [sgr_binding] ++ "'"

            in ((sgr, Text.pack $ ch : map fst grabbed_chrs) : group_sgrs more')

        group_sgrs [] = []

    in (make_formatted_string (group_sgrs $ zip text sgrs) ==)

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
            "a---   b  c---  "
            (make_formatted_string [([Colors.fg_bred], "abcd"), ([], "efg"), ([Colors.fg_bgreen], "h"), ([], "ij"), ([Colors.fg_bblue], "klmn"), ([], "op")])

case_make_formatted_string :: Assertion
case_make_formatted_string =
    FormattedString "text a b cdef"
        [([Colors.bold], 5), ([Colors.reset], 0), ([Colors.bg_red], 2), ([Colors.reset], 0), ([Colors.fg_bgreen], 2), ([Colors.reset], 0), ([Colors.fg_blue], 1), ([Colors.reset], 0), ([], 3), ([Colors.reset], 0)]
            @=?
    make_formatted_string
        [([Colors.bold], "text "), ([Colors.bg_red], "a "), ([Colors.fg_bgreen], "b "), ([Colors.fg_blue], "c"), ([], "def")]

tests :: TestTree
tests = $(testGroupGenerator)
