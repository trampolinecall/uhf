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

type ErrorAccumulated a = Writer [Diagnostic.Error] a -- TODO: allow for warnings too

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl.Decl IR.Decl.Key, Arena.Arena (IR.Value.Value (Location.Located [Location.Located Text])) IR.Value.Key, IR.Decl.Key)
type NRIR = (Arena.Arena IR.Decl.Decl IR.Decl.Key, Arena.Arena (IR.Value.Value (Maybe IR.Value.Key)) IR.Value.Key)

compile :: File.File -> Either [Diagnostic.Error] NRIR
compile file =
    let (res, diags) = runWriter $ compile' file
    in if null diags
        then Right res
        else Left diags

compile' :: File.File -> ErrorAccumulated NRIR
compile' file = lex file >>= parse >>= to_ir >>= name_resolve

lex :: File.File -> ErrorAccumulated Tokens
lex file = convert_errors (Lexer.lex file)

parse :: Tokens -> ErrorAccumulated AST
parse (toks, eof_tok) =
    let (other_errors, bt_error, res) = Parser.parse toks eof_tok
    in tell (map Diagnostic.to_error other_errors) >>
    (case bt_error of
        Just bt_error -> tell [Diagnostic.to_error bt_error]
        Nothing -> pure ()) >>
    pure res

to_ir :: AST -> ErrorAccumulated FirstIR
to_ir decls = convert_errors (ASTToIR.convert decls)

name_resolve ::  FirstIR -> ErrorAccumulated NRIR
name_resolve (decls, values, mod) = convert_errors (NameResolve.resolve (decls, values, mod))

convert_errors :: Diagnostic.IsError e => Writer [e] a -> Writer [Diagnostic.Error] a
convert_errors = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_error errs))
