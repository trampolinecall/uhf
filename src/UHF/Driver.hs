module UHF.Driver
    ( CompileOptions(..)
    , OutputFormat(..)
    , compile
    ) where

import UHF.Util.Prelude

import qualified Data.Text.IO as Text.IO

import qualified System.FilePath as FilePath

import qualified Arena

import UHF.IO.File (File)
import qualified UHF.IO.File as File
import UHF.IO.Located (Located)

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

import qualified UHF.Compiler as Compiler

import qualified UHF.Data.Token as Token
import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Keys as IR.Keys
import qualified UHF.Data.IR.Type as IR.Type

import qualified UHF.Phases.Front.Lexer as Lexer
import qualified UHF.Phases.Front.Parser as Parser
import qualified UHF.Phases.Middle.ToHIR as ToHIR
import qualified UHF.Phases.Middle.NameResolve as NameResolve
import qualified UHF.Phases.Middle.InfixGroup as InfixGroup
import qualified UHF.Phases.Middle.Type as Type
import qualified UHF.Phases.Middle.ToRIR as ToRIR
import qualified UHF.Phases.Middle.ToANFIR as ToANFIR
import qualified UHF.Phases.Middle.RemovePoison as RemovePoison
import qualified UHF.Phases.Back.ToDot as ToDot
import qualified UHF.Phases.Back.TSBackend as TSBackend

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]

type FirstIR = (Arena.Arena (HIR.Decl (HIR.NameContext, [Located Text]) (HIR.TypeExpr (HIR.NameContext, [Located Text])) () ()) IR.Keys.DeclKey, Arena.Arena (HIR.ADT (HIR.TypeExpr (HIR.NameContext, [Located Text]))) IR.Keys.ADTKey, Arena.Arena (HIR.TypeSynonym (HIR.TypeExpr (HIR.NameContext, [Located Text]))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.BoundValue ()) IR.Keys.BoundValueKey)
type NRIR = (Arena.Arena (HIR.Decl (Located (Maybe IR.Keys.BoundValueKey)) (HIR.TypeExpr (Maybe IR.Keys.DeclKey)) () ()) IR.Keys.DeclKey, Arena.Arena (HIR.ADT (HIR.TypeExpr (Maybe IR.Keys.DeclKey))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.TypeSynonym (HIR.TypeExpr (Maybe IR.Keys.DeclKey))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.BoundValue ()) IR.Keys.BoundValueKey)
type TypedIR = (Arena.Arena (HIR.Decl (Located (Maybe IR.Keys.BoundValueKey)) (Maybe (IR.Type.Type Void)) (Maybe (IR.Type.Type Void)) Void) IR.Keys.DeclKey, Arena.Arena (HIR.ADT (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.TypeSynonym (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.BoundValue (Maybe (IR.Type.Type Void))) IR.Keys.BoundValueKey)
type GraphIR = (Arena.Arena ANFIR.Decl IR.Keys.DeclKey, Arena.Arena (HIR.ADT (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (HIR.TypeSynonym (Maybe (IR.Type.Type Void))) IR.Keys.TypeSynonymKey, Arena.Arena (ANFIR.Binding (Maybe (IR.Type.Type Void)) ()) IR.Keys.BindingKey, Arena.Arena (ANFIR.Param (Maybe (IR.Type.Type Void))) IR.Keys.ParamKey, Arena.Arena (HIR.BoundValue (Maybe (IR.Type.Type Void))) IR.Keys.BoundValueKey)
type NoPoisonIR = (Arena.Arena ANFIR.Decl IR.Keys.DeclKey, Arena.Arena (HIR.ADT (IR.Type.Type Void)) IR.Keys.ADTKey, Arena.Arena (HIR.TypeSynonym (IR.Type.Type Void)) IR.Keys.TypeSynonymKey, Arena.Arena (ANFIR.Binding (IR.Type.Type Void) Void) IR.Keys.BindingKey, Arena.Arena (ANFIR.Param (IR.Type.Type Void)) IR.Keys.ParamKey, Arena.Arena (HIR.BoundValue (IR.Type.Type Void)) IR.Keys.BoundValueKey)
type Dot = Text
type TS = Text

data OutputFormat = AST | Dot | TS deriving Eq
data CompileOptions
    = CompileOptions
        { input_file :: FilePath
        , module_name :: Maybe Text
        , output_formats :: [OutputFormat]
        }

compile :: FormattedString.ColorsNeeded -> DiagnosticSettings.Settings -> CompileOptions -> IO (Either () ())
compile c_needed diagnostic_settings compile_options =
    File.open (input_file compile_options) >>= \ file ->
    Compiler.run_compiler (front file >>= \ ast -> analysis ast >>= \ ir -> pure (ast, ir)) c_needed diagnostic_settings >>= \case
        Just (ast, Just ir) ->
            (if AST `elem` output_formats compile_options -- TODO: should outputting ast skip anaylsis?
                then todo -- TODO
                else pure ()) >>
            (if Dot `elem` output_formats compile_options
                then write_output_file "dot" (to_dot ir)
                else pure ()) >>
            (if TS `elem` output_formats compile_options
                then write_output_file "ts" (to_ts ir)
                else pure ()) >>
            pure (Right ())

        Just (_, Nothing) -> pure (Left ())
        Nothing -> pure (Left ())

    where
        module_name' =
            case module_name compile_options of
                Just m -> convert_str m :: FilePath
                Nothing -> FilePath.takeBaseName (input_file compile_options)

        get_output_file_path ext = FilePath.takeDirectory (input_file compile_options) FilePath.</> module_name' FilePath.<.> ext

        write_output_file ext = Text.IO.writeFile (get_output_file_path ext)

front :: File -> Compiler.Compiler AST
front file =
    Lexer.lex file >>= \ (tokens, eof_tok) ->
    Parser.parse tokens eof_tok

analysis :: AST -> Compiler.Compiler (Maybe NoPoisonIR)
analysis ast =
    ToHIR.convert ast >>= \ (decls, adts, type_synonyms, bound_values) ->
    NameResolve.resolve (decls, adts, type_synonyms) >>= \ (decls, adts, type_synonyms) ->
    let decls' = InfixGroup.group decls
    in Type.typecheck (decls', adts, type_synonyms, bound_values) >>= \ (decls', adts, type_synonyms, bound_values) ->
    ToRIR.convert decls' bound_values >>= \ (decls', bound_values) ->
    let (decls'', nodes, params) = ToANFIR.convert bound_values decls'
        no_poison = RemovePoison.remove_poison (decls'', adts, type_synonyms, nodes, params, bound_values)
    in pure no_poison

to_ts :: NoPoisonIR -> TS
to_ts (decls, adts, type_synonyms, nodes, params, bound_values) = TSBackend.lower decls adts type_synonyms nodes params

to_dot :: NoPoisonIR -> Dot
to_dot (decls, adts, type_synonyms, nodes, params, bound_values) = ToDot.to_dot decls adts type_synonyms nodes params
