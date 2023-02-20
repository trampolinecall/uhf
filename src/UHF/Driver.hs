module UHF.Driver
    ( compile
    ) where

import UHF.Util.Prelude

import qualified Arena

import UHF.IO.File (File)
import qualified UHF.IO.File as File
import UHF.IO.Located (Located)

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

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
import qualified UHF.ToGraph as ToGraph
import qualified UHF.RemovePoison as RemovePoison
import qualified UHF.ToDot as ToDot
import qualified UHF.TSBackend as TSBackend

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (IR.NameContext, [Located Text]))) IR.NominalTypeKey, Arena.Arena (IR.Binding (IR.NameContext, [Located Text]) (IR.TypeExpr (IR.NameContext, [Located Text])) () ()) IR.BindingKey, Arena.Arena (IR.BoundValue ()) IR.BoundValueKey)
type NRIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.TypeExpr (Maybe IR.DeclKey))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Located (Maybe IR.BoundValueKey)) (IR.TypeExpr (Maybe IR.DeclKey)) () ()) IR.BindingKey, Arena.Arena (IR.BoundValue ()) IR.BoundValueKey)
type TypedIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (Maybe (IR.Type Void))) IR.NominalTypeKey, Arena.Arena (IR.Binding (Located (Maybe IR.BoundValueKey)) (Maybe (IR.Type Void)) (Maybe (IR.Type Void)) Void) IR.BindingKey, Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey)
type GraphIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (Maybe (IR.Type Void))) IR.NominalTypeKey, Arena.Arena (IR.GraphNode (Maybe (IR.Type Void)) ()) IR.GraphNodeKey, Arena.Arena (IR.GraphParam (Maybe (IR.Type Void))) IR.GraphParamKey, Arena.Arena (IR.BoundValue (Maybe (IR.Type Void))) IR.BoundValueKey)
type NoPoisonIR = (Arena.Arena IR.Decl IR.DeclKey, Arena.Arena (IR.NominalType (IR.Type Void)) IR.NominalTypeKey, Arena.Arena (IR.GraphNode (IR.Type Void) Void) IR.GraphNodeKey, Arena.Arena (IR.GraphParam (IR.Type Void)) IR.GraphParamKey, Arena.Arena (IR.BoundValue (IR.Type Void)) IR.BoundValueKey)
type Dot = Text
type TS = Text

compile :: FormattedString.ColorsNeeded -> DiagnosticSettings.Settings -> FilePath -> IO (Either () ())
compile c_needed diagnostic_settings path =
    File.open path >>= \ file ->
    Compiler.run_compiler (compile' file) c_needed diagnostic_settings >>= \case
        Just (Just res) -> putTextLn res >> pure (Right ()) -- TODO: get rid of double Maybe
        Just Nothing -> pure (Left ()) -- TODO: decide what should happen here
        Nothing -> pure (Left ())

compile' :: File -> Compiler.Compiler (Maybe TS)
compile' file =
    -- TODO: clean up
    Lexer.lex file >>= \ (tokens, eof_tok) ->
    Parser.parse tokens eof_tok >>= \ ast ->
    ASTToIR.convert ast >>= \ (decls, nominal_types, bindings, bound_values) ->
    NameResolve.resolve (decls, nominal_types, bindings) >>= \ (decls, nominal_types, bindings) ->
    let bindings' = InfixGroup.group bindings
    in Type.typecheck (decls, nominal_types, bindings', bound_values) >>= \ (decls, nominal_types, bindings, bound_values) ->
    let (nodes, params) = ToGraph.to_graph bound_values bindings
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
