module UHF.Lexer
    ( lex
    ) where

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
