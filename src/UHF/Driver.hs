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
import qualified UHF.HIR as HIR
import qualified UHF.ANFIR as ANFIR

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

type FirstIR = (Arena.Arena (HIR.Decl (HIR.NameContext, [Located Text]) (HIR.TypeExpr (HIR.NameContext, [Located Text])) () ()) HIR.DeclKey, Arena.Arena (HIR.NominalType (HIR.TypeExpr (HIR.NameContext, [Located Text]))) HIR.NominalTypeKey, Arena.Arena (HIR.BoundValue ()) HIR.BoundValueKey)
type NRIR = (Arena.Arena (HIR.Decl (Located (Maybe HIR.BoundValueKey)) (HIR.TypeExpr (Maybe HIR.DeclKey)) () ()) HIR.DeclKey, Arena.Arena (HIR.NominalType (HIR.TypeExpr (Maybe HIR.DeclKey))) HIR.NominalTypeKey, Arena.Arena (HIR.BoundValue ()) HIR.BoundValueKey)
type TypedIR = (Arena.Arena (HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void) HIR.DeclKey, Arena.Arena (HIR.NominalType (Maybe (HIR.Type Void))) HIR.NominalTypeKey, Arena.Arena (HIR.BoundValue (Maybe (HIR.Type Void))) HIR.BoundValueKey)
type GraphIR = (Arena.Arena (HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void) HIR.DeclKey, Arena.Arena (HIR.NominalType (Maybe (HIR.Type Void))) HIR.NominalTypeKey, Arena.Arena (ANFIR.Node (Maybe (HIR.Type Void)) ()) ANFIR.NodeKey, Arena.Arena (ANFIR.Param (Maybe (HIR.Type Void))) ANFIR.ParamKey, Arena.Arena (HIR.BoundValue (Maybe (HIR.Type Void))) HIR.BoundValueKey)
type NoPoisonIR = (Arena.Arena (HIR.Decl (Located (Maybe HIR.BoundValueKey)) (Maybe (HIR.Type Void)) (Maybe (HIR.Type Void)) Void) HIR.DeclKey, Arena.Arena (HIR.NominalType (HIR.Type Void)) HIR.NominalTypeKey, Arena.Arena (ANFIR.Node (HIR.Type Void) Void) ANFIR.NodeKey, Arena.Arena (ANFIR.Param (HIR.Type Void)) ANFIR.ParamKey, Arena.Arena (HIR.BoundValue (HIR.Type Void)) HIR.BoundValueKey)
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
    ASTToIR.convert ast >>= \ (decls, nominal_types, bound_values) ->
    NameResolve.resolve (decls, nominal_types) >>= \ (decls, nominal_types) ->
    let decls' = InfixGroup.group decls
    in Type.typecheck (decls', nominal_types, bound_values) >>= \ (decls', nominal_types, bound_values) ->
    let (nodes, params) = ToGraph.to_graph bound_values decls'
        no_poison = RemovePoison.remove_poison (decls', nominal_types, nodes, params, bound_values)
        dot =
            case no_poison of
                Just (decls', nominal_types', nodes', params', bound_values') -> Just $ ToDot.to_dot decls' nominal_types' nodes' params'
                Nothing -> Nothing

        ts =
            case no_poison of
                Just (decls', nominal_types', nodes', params', bound_values') -> Just $ TSBackend.lower decls' nominal_types' nodes' params'
                Nothing -> Nothing
    in pure ts
