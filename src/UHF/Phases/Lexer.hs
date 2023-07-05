module UHF.Phases.Lexer
    ( UHF.Phases.Lexer.lex

    , LexError.Error
    ) where

import UHF.Util.Prelude

import qualified UHF.Phases.Lexer.LexError as LexError
import qualified UHF.Phases.Lexer.MainLexer as MainLexer
import qualified UHF.Phases.Lexer.IdentifierGrouper as IdentifierGrouper

import qualified UHF.Data.Token as Token
import UHF.IO.File (File)

import qualified UHF.Compiler as Compiler

lex :: File -> Compiler.WithDiagnostics LexError.Error Void ([Token.LToken], Token.LToken)
lex f =
    MainLexer.lex f >>= \ (toks, eof) ->
    let toks' = IdentifierGrouper.group_identifiers (toList toks)
    in pure (toks', eof)
