module UHF.Lexer
    ( UHF.Lexer.lex

    , LexError.LexError
    ) where

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.Lexer.MainLexer as MainLexer
import qualified UHF.Lexer.PostProcess as PostProcess

import qualified UHF.Token as Token
import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location

lex :: File.File -> ([LexError.LexError], [Location.Located Token.Token])
lex = PostProcess.group_identifiers . MainLexer.lex
