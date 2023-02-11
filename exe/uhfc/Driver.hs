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
import qualified UHF.IR as IR

import qualified UHF.Lexer as Lexer
import qualified UHF.Parser as Parser
import qualified UHF.ASTToIR as ASTToIR
import qualified UHF.NameResolve as NameResolve

type ErrorAccumulated a = Writer [Diagnostic.Error] a -- TODO: allow for warnings too

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (IR.NameContext, [Location.Located Text]))) IR.NominalTypeKey, Arena.Arena (IR.Binding (IR.NameContext, [Location.Located Text]) (IR.TypeExpr (IR.NameContext, [Location.Located Text]))) IR.BindingKey, Arena.Arena IR.BoundName IR.BoundNameKey)
type NRIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (Maybe IR.DeclKey))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Maybe IR.BoundNameKey) (IR.TypeExpr (Maybe IR.DeclKey))) IR.BindingKey)

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
    let (bt_error, res) = Parser.parse toks eof_tok
    in (case bt_error of
        Just bt_error -> tell [Diagnostic.to_error bt_error]
        Nothing -> pure ()) >>
    pure res

to_ir :: AST -> ErrorAccumulated FirstIR
to_ir decls = convert_errors (ASTToIR.convert decls)

name_resolve ::  FirstIR -> ErrorAccumulated NRIR
name_resolve (decls, nominals, bindings, bound_names) = convert_errors (NameResolve.resolve (decls, nominals, bindings))

convert_errors :: Diagnostic.IsError e => Writer [e] a -> Writer [Diagnostic.Error] a
convert_errors = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_error errs))
