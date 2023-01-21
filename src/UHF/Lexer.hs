module UHF.Lexer
    ( UHF.Lexer.lex

    , LexError.LexError
    ) where

import UHF.Util.Prelude

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.Lexer.MainLexer as MainLexer
import qualified UHF.Lexer.IdentifierGrouper as IdentifierGrouper

import qualified UHF.Token as Token
import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location

lex :: File.File -> ([LexError.LexError], [Token.LToken], Token.LToken)
lex f =
    let (errs, toks, eof) = MainLexer.lex f
        (errs', toks') = IdentifierGrouper.group_identifiers toks -- TODO: use writer monad for this
    in (errs ++ errs', toks', eof)
