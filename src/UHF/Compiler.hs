module UHF.Compiler
    ( compile
    ) where

import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Lexer as Lexer
import qualified UHF.Token as Token

import qualified UHF.Parser as Parser
import qualified UHF.AST as AST

compile :: Phase File.File [AST.Decl]
compile = lex_phase `link` parse_phase

-- TODO: use Writer to collect diagnostics
type Phase a b = a -> ([Diagnostic.Diagnostic], b)

link :: Phase a b -> Phase b c -> Phase a c
link phase1 phase2 a =
    let (diags1, b) = phase1 a
        (diags2, c) = phase2 b
    in (diags1 ++ diags2, c)

lex_phase :: Phase File.File ([Token.LNormalToken], Token.LNormalToken)
lex_phase file =
    let (diagnostics, res, eof_tok) = Lexer.lex file
    in (map Diagnostic.to_diagnostic diagnostics, (res, eof_tok))

parse_phase :: Phase ([Token.LNormalToken], Token.LNormalToken) [AST.Decl]
parse_phase (toks, eof_tok) =
    let (diagnostics, res) = Parser.parse toks eof_tok
    in (map Diagnostic.to_diagnostic diagnostics, res)
