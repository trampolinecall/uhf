module Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Compiler as Compiler

import qualified UHF.Token as Token
import qualified UHF.AST as AST
import qualified UHF.IR as IR

import qualified UHF.Lexer as Lexer
import qualified UHF.Parser as Parser
import qualified UHF.ASTToIR as ASTToIR
import qualified UHF.NameResolve as NameResolve
import qualified UHF.InfixGroup as InfixGroup
import qualified UHF.Type as Type

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (IR.NameContext, [Location.Located Text]))) IR.NominalTypeKey, Arena.Arena (IR.Binding (IR.NameContext, [Location.Located Text]) (IR.TypeExpr (IR.NameContext, [Location.Located Text])) () ()) IR.BindingKey, Arena.Arena (IR.BoundName ()) IR.BoundNameKey)
type NRIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (Maybe IR.DeclKey))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Maybe IR.BoundNameKey) (IR.TypeExpr (Maybe IR.DeclKey)) () ()) IR.BindingKey, Arena.Arena (IR.BoundName ()) IR.BoundNameKey)
type TypedIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (Maybe (IR.Type Void))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Maybe IR.BoundNameKey) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void) IR.BindingKey, Arena.Arena (IR.BoundName (Maybe (IR.Type Void))) IR.BoundNameKey)

compile :: File.File -> Compiler.Compiler TypedIR
compile file =
    Lexer.lex file >>= \ (tokens, eof_tok) ->
    Parser.parse tokens eof_tok >>= \ ast ->
    ASTToIR.convert ast >>= \ (decls, nominal_types, bindings, bound_names) ->
    NameResolve.resolve (decls, nominal_types, bindings) >>= \ (decls, nominal_types, bindings) ->
    let bindings' = InfixGroup.group bindings
    in Type.typecheck (decls, nominal_types, bindings', bound_names)
