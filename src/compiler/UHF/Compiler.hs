module UHF.Compiler
    ( compile
    ) where

import qualified UHF.IO.Location as Location
import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Lexer as Lexer
import qualified UHF.Token as Token

import qualified UHF.Parser as Parser
import qualified UHF.AST.Decl as AST.Decl

compile :: Phase File.File [AST.Decl.Decl]
compile = lex_phase `link` parse_phase

type Phase a b = a -> ([Diagnostic.Diagnostic], b)

link :: Phase a b -> Phase b c -> Phase a c
link phase1 phase2 a =
    let (diags1, b) = phase1 a
        (diags2, c) = phase2 b
    in (diags1 ++ diags2, c)

lex_phase :: Phase File.File [Token.LToken]
lex_phase file =
    let (diagnostics, res) = Lexer.lex file
    in (map Diagnostic.to_diagnostic diagnostics, res)

parse_phase :: Phase [Token.LToken] [AST.Decl.Decl]
parse_phase toks =
    let (diagnostics, res) = Parser.parse toks
    in (map Diagnostic.to_diagnostic diagnostics, res)
