module Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified Arena

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

type ErrorAccumulated a = Writer [Diagnostic.Diagnostic] a

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl.Decl IR.Decl.Key, Arena.Arena (IR.Value.Value (Location.Located [Location.Located Text])) IR.Value.Key, IR.Decl.Key)
type NRIR = (Arena.Arena IR.Decl.Decl IR.Decl.Key, Arena.Arena (IR.Value.Value (Maybe IR.Value.Key)) IR.Value.Key)

compile :: File.File -> ErrorAccumulated NRIR
compile file = lex file >>= parse >>= to_ir >>= name_resolve

lex :: File.File -> ErrorAccumulated Tokens
lex file = convert_errors (Lexer.lex file)

parse :: Tokens -> ErrorAccumulated AST
parse (toks, eof_tok) =
    let (other_errors, bt_errors, res) = Parser.parse toks eof_tok
    in tell (map Diagnostic.to_diagnostic other_errors) >>
    tell [Diagnostic.to_diagnostic bt_errors] >>
    pure res

to_ir :: AST -> ErrorAccumulated FirstIR
to_ir decls = convert_errors (ASTToIR.convert decls)

name_resolve ::  FirstIR -> ErrorAccumulated NRIR
name_resolve (decls, values, mod) = convert_errors (NameResolve.resolve (decls, values, mod))

convert_errors :: Diagnostic.IsDiagnostic e => Writer [e] a -> Writer [Diagnostic.Diagnostic] a
convert_errors = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_diagnostic errs))
