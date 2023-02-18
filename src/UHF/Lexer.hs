module UHF.Lexer
    ( UHF.Lexer.lex

    , LexError.LexError
    ) where

import UHF.Util.Prelude

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.Lexer.MainLexer as MainLexer
import qualified UHF.Lexer.IdentifierGrouper as IdentifierGrouper

import qualified UHF.Token as Token
import UHF.IO.Location (File)

lex :: File -> Writer [LexError.LexError] ([Token.LToken], Token.LToken)
lex f =
    MainLexer.lex f >>= \ (toks, eof) ->
    IdentifierGrouper.group_identifiers (toList toks) >>= \ toks' ->
    pure (toks', eof)
