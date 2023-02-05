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
import qualified UHF.IR.Value as IR.Value

import qualified UHF.Lexer as Lexer
import qualified UHF.Parser as Parser
import qualified UHF.ASTToIR as ASTToIR
import qualified UHF.NameResolve as NameResolve

compile :: File.File -> Writer [Diagnostic.Diagnostic] (IR.Decl.Module (Location.Located IR.Value.ResolvedValue))
compile file = lex file >>= parse >>= to_ir >>= name_resolve

lex :: File.File -> Writer [Diagnostic.Diagnostic] ([Token.LToken], Token.LToken)
lex file = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (Lexer.lex file)

parse :: ([Token.LToken], Token.LToken) -> Writer [Diagnostic.Diagnostic] [AST.Decl]
parse (toks, eof_tok) =
    let (diagnostics, res) = Parser.parse toks eof_tok
    in tell (map Diagnostic.to_diagnostic diagnostics) >> pure res

to_ir :: [AST.Decl] -> Writer [Diagnostic.Diagnostic] (IR.Decl.Module (Location.Located [(Location.Located Text)]))
to_ir decls = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (ASTToIR.convert decls)

name_resolve :: IR.Decl.Module (Location.Located [(Location.Located Text)]) -> Writer [Diagnostic.Diagnostic] (IR.Decl.Module (Location.Located IR.Value.ResolvedValue))
name_resolve decls = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs)) (NameResolve.resolve decls)
