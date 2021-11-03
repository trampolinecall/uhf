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

remaining :: Lexer -> Text.Text
remaining = _

rev_passed :: Lexer -> Text.Text
rev_passed = _
