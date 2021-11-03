{-# LANGUAGE OverloadedStrings #-}

module UHF.Lexer
    ( lex
    , tests
    ) where

import Test.HUnit
import qualified UHF.IO.File as File
import qualified Data.Text as Text

data Lexer
    = Lexer
      { source_file :: File.File
      , source_location :: Int
      , line_n :: Int
      , col_n :: Int
      , indent_stack :: [IndentFrame]
      }

data IndentFrame
    = IndentationSensitive Int
    | IndentationInsensitive

l_contents :: Lexer -> Text.Text
l_contents = File.contents . source_file

remaining :: Lexer -> Text.Text
remaining l = Text.drop (source_location l) (l_contents l)

passed :: Lexer -> Text.Text
passed l = Text.take (source_location l) (l_contents l)

rev_passed :: Lexer -> Text.Text
rev_passed = Text.reverse . passed

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
    ]
