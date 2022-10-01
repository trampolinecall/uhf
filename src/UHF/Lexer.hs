module UHF.Lexer
    ( UHF.Lexer.lex

    , LexError.LexError
    ) where

import qualified UHF.Lexer.LexError as LexError
import qualified UHF.Lexer.MainLexer as MainLexer
import qualified UHF.Lexer.PostProcess as PostProcess
import qualified UHF.Lexer.IndentCounter as IndentCounter

import qualified UHF.Token as Token
import qualified UHF.IO.File as File

lex :: File.File -> ([LexError.LexError], [Token.LNormalToken], Token.LNormalToken)
lex f =
    let (errs, toks, eof) = MainLexer.lex f
        (errs', toks') = PostProcess.group_identifiers $ IndentCounter.count_indents (toks, eof)
    in (errs ++ errs', toks', eof)
