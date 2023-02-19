module Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Location as Location
import UHF.IO.Location (File, Located)
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.Token as Token
import qualified UHF.AST as AST
import qualified UHF.IR as IR

import qualified UHF.Lexer as Lexer
import qualified UHF.Parser as Parser
import qualified UHF.ASTToIR as ASTToIR
import qualified UHF.NameResolve as NameResolve
import qualified UHF.InfixGroup as InfixGroup
import qualified UHF.Type as Type
import qualified UHF.ToGraph as ToGraph
import qualified UHF.RemovePoison as RemovePoison
import qualified UHF.ToDot as ToDot
import qualified UHF.TSBackend as TSBackend

type ErrorAccumulated a = Writer [Diagnostic.Error] a -- TODO: allow for warnings too

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (IR.NameContext, [Location.Located Text]))) IR.NominalTypeKey, Arena.Arena (IR.Binding (IR.NameContext, [Location.Located Text]) (IR.TypeExpr (IR.NameContext, [Location.Located Text])) () ()) IR.BindingKey, Arena.Arena (IR.BoundValue ()) IR.BoundValueKey)
type NRIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (Maybe IR.DeclKey))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Located (Maybe IR.BoundValueKey)) (IR.TypeExpr (Maybe IR.DeclKey)) () ()) IR.BindingKey, Arena.Arena (IR.BoundValue ()) IR.BoundValueKey)
type TypedIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (Maybe (IR.Type Void))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void) IR.BindingKey, Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey)
type GraphIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (Maybe (IR.Type Void))) IR.NominalTypeKey, Arena.Arena (IR.GraphNode (Maybe (IR.Type Void)) ()) IR.GraphNodeKey, Arena.Arena (IR.GraphParam (Maybe (IR.Type Void))) IR.GraphParamKey, Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey)
type NoPoisonIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.Type Void)) IR.NominalTypeKey, Arena.Arena (IR.GraphNode (IR.Type Void) Void) IR.GraphNodeKey, Arena.Arena (IR.GraphParam (IR.Type Void)) IR.GraphParamKey, Arena.Arena (IR.BoundValue (IR.Type Void)) IR.BoundValueKey)
type Dot = Text
type TS = Text

compile :: File -> Either [Diagnostic.Error] (Maybe TS)
compile file =
    let (res, diags) = runWriter $ compile' file
    in if null diags
        then Right res
        else Left diags

compile' :: File -> ErrorAccumulated (Maybe TS)
compile' file =
    -- TODO: clean up
    convert_errors (Lexer.lex file) >>= \ (tokens, eof_tok) ->
    let (bt_error, ast) = Parser.parse tokens eof_tok
    in (case bt_error of
        Just bt_error -> tell [Diagnostic.to_error bt_error]
        Nothing -> pure ()) >>
    convert_errors (ASTToIR.convert file ast) >>= \ (decls, nominal_types, bindings, bound_values) ->
    convert_errors (NameResolve.resolve (decls, nominal_types, bindings)) >>= \ (decls, nominal_types, bindings) ->
    let bindings' = InfixGroup.group bindings
    in convert_errors (Type.typecheck (decls, nominal_types, bindings', bound_values)) >>= \ (decls, nominal_types, bindings, bound_values) ->
    let (nodes, params) = (ToGraph.to_graph bound_values bindings)
        no_poison = RemovePoison.remove_poison (decls, nominal_types, nodes, params, bound_values)
        dot =
            case no_poison of
                Just (decls', nominal_types', nodes', params', bound_values') -> Just $ ToDot.to_dot decls' nominal_types' nodes' params'
                Nothing -> Nothing

        ts =
            case no_poison of
                Just (decls', nominal_types', nodes', params', bound_values') -> Just $ TSBackend.lower decls' nominal_types' nodes' params'
                Nothing -> Nothing
    in pure ts

convert_errors :: Diagnostic.IsError e => Writer [e] a -> Writer [Diagnostic.Error] a
convert_errors = mapWriter (\ (res, errs) -> (res, map Diagnostic.to_error errs))
