module Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Token as Token
import qualified UHF.AST as AST
import qualified UHF.IR.Decl as IR.Decl

import qualified UHF.Lexer as Lexer
import qualified UHF.Parser as Parser
import qualified UHF.ASTToIR as ASTToIR

compile :: File.File -> Writer [Diagnostic.Diagnostic] (IR.Decl.Module (Location.Located [(Location.Located Text)]))
compile file = lex_phase file >>= parse_phase >>= to_ir_phase

lex_phase :: File.File -> Writer [Diagnostic.Diagnostic] ([Token.LToken], Token.LToken)
lex_phase file = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (Lexer.lex file)

parse_phase :: ([Token.LToken], Token.LToken) -> Writer [Diagnostic.Diagnostic] [AST.Decl]
parse_phase (toks, eof_tok) =
    let (diagnostics, res) = Parser.parse toks eof_tok
    in tell (map Diagnostic.to_diagnostic diagnostics) >> pure res

to_ir_phase :: [AST.Decl] -> Writer [Diagnostic.Diagnostic] (IR.Decl.Module (Location.Located [(Location.Located Text)]))
to_ir_phase decls = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (ASTToIR.convert decls)
