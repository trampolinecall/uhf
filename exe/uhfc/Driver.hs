module Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Lexer as Lexer
import qualified UHF.Token as Token

import qualified UHF.Parser as Parser
import qualified UHF.AST as AST

compile :: Phase File.File [AST.Decl]
compile file = lex_phase file >>= parse_phase

-- TODO: use Writer to collect diagnostics
type Phase a b = a -> Writer [Diagnostic.Diagnostic] b

lex_phase :: Phase File.File ([Token.LToken], Token.LToken)
lex_phase file = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (Lexer.lex file)

parse_phase :: Phase ([Token.LToken], Token.LToken) [AST.Decl]
parse_phase (toks, eof_tok) =
    let (diagnostics, res) = Parser.parse toks eof_tok
    in tell (map Diagnostic.to_diagnostic diagnostics) >> pure res
