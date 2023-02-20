module UHF.Lexer
    ( UHF.Lexer.lex

    , LexError.Error
    ) where

import UHF.Util.Prelude

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.Lexer.MainLexer as MainLexer
import qualified UHF.Lexer.IdentifierGrouper as IdentifierGrouper

import qualified UHF.Token as Token
import UHF.IO.File (File)

import qualified UHF.Compiler as Compiler

lex :: File -> Compiler.Compiler ([Token.LToken], Token.LToken)
lex f =
    let (res, errs) = runWriter (
                MainLexer.lex f >>= \ (toks, eof) ->
                let toks' = IdentifierGrouper.group_identifiers (toList toks)
                in pure (toks', eof)
            )
    in Compiler.errors errs >> pure res
