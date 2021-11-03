{-# LANGUAGE OverloadedStrings #-}

module UHF.Lexer
    ( lex
    , tests
    ) where

import Test.HUnit
import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified Data.Text as Text

data Lexer
    = Lexer
      { file :: File.File
      , location :: Int
      , line :: Int
      , col :: Int
      , indent_stack :: [IndentFrame]
      }
      deriving (Eq, Show)

data IndentFrame
    = IndentationSensitive Int
    | IndentationInsensitive
    deriving (Eq, Show)

l_contents :: Lexer -> Text.Text
l_contents = File.contents . file

remaining :: Lexer -> Text.Text
remaining l = Text.drop (location l) (l_contents l)

passed :: Lexer -> Text.Text
passed l = Text.take (location l) (l_contents l)

rev_passed :: Lexer -> Text.Text
rev_passed = Text.reverse . passed

new_lexer :: File.File -> Lexer
new_lexer f = Lexer f 0 1 1 [IndentationSensitive 0]

lexer_location :: Lexer -> Location.Location
lexer_location lexer = Location.Location (file lexer) (line lexer) (col lexer)

lexer_span :: Lexer -> Int -> Int -> Location.Span
lexer_span lexer start len =
    let start_lexer = lexer `seek` start
        before_end_lexer = start_lexer `seek` (len - 1)
        end_lexer = start_lexer `seek` len
    in Location.Span (lexer_location start_lexer) (lexer_location before_end_lexer) (lexer_location end_lexer)

seek :: Lexer -> Int -> Lexer
seek lexer 0 = lexer
seek lexer n =
    let
        num_nl
            | n < 0 = Text.count "\n" $ Text.take (-n) (Text.drop (location lexer + n) (l_contents lexer))
            | otherwise = Text.count "\n" $ Text.take n (Text.drop (location lexer) (l_contents lexer))

        line'
            | n < 0 = line lexer - num_nl
            | otherwise = line lexer + num_nl

        col'
            | num_nl == 0 = col lexer + n
            | otherwise =
                1 + Text.length (Text.takeWhile (/='\n') $ Text.reverse $ Text.take (location lexer + n) (l_contents lexer))

    in lexer
       { location = location lexer + n
       , line = line'
       , col = col'
       }

tests :: Test
tests = test
    [ "l_contents" ~:
        "abcdefghijkl" ~=? l_contents (Lexer (File.File "filename" "abcdefghijkl") 0 1 1 [])

    , "remaining" ~:
        "fghijkl" ~=? remaining (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "passed" ~:
        "abcde" ~=? passed (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "rev_passed" ~:
        "edcba" ~=? rev_passed (Lexer (File.File "filename" "abcdefghijkl") 5 1 1 [])

    , "new_lexer" ~:
        let f = File.File "a" "abc"
        in Lexer f 0 1 1 [IndentationSensitive 0] ~=? new_lexer f

    , "lexer_location" ~:
        let f = File.File "a" "abc"
        in Location.Location f 2 4 ~=? lexer_location (Lexer f 0 2 4 [])

    , "lexer_span" ~:
        let f = File.File "a" "abcdef"
        in Location.Span (Location.Location f 1 1) (Location.Location f 1 2) (Location.Location f 1 3) ~=? lexer_span (new_lexer f) 0 2

    , "seek" ~:
        [ let l = new_lexer $ File.File "a" "abc"
          in l ~=? seek l 0

        , "forward" ~:
            [ let f = File.File "a" "abcd\nefgh"
              in Lexer f 2 1 3 [] ~=? seek (Lexer f 0 1 1 []) 2
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 3 1 4 [] ~=? seek (Lexer f 0 1 1 []) 3
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 4 1 5 [] ~=? seek (Lexer f 0 1 1 []) 4
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 5 2 1 [] ~=? seek (Lexer f 0 1 1 []) 5
            ]

        , "backward" ~:
            [ let f = File.File "a" "abcd\nefgh"
              in Lexer f 6 2 2 [] ~=? seek (Lexer f 8 2 4 []) (-2)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 5 2 1 [] ~=? seek (Lexer f 8 2 4 []) (-3)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 4 1 5 [] ~=? seek (Lexer f 8 2 4 []) (-4)
            , let f = File.File "a" "abcd\nefgh"
              in Lexer f 3 1 4 [] ~=? seek (Lexer f 8 2 4 []) (-5)
            ]
        ]
    ]
