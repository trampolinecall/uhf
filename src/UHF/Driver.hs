module UHF.Driver
    ( CompileOptions(..)
    , OutputFormat(..)
    , compile
    ) where

import UHF.Util.Prelude

import qualified Data.Text.IO as Text.IO

import qualified System.FilePath as FilePath

import UHF.IO.File (File)
import qualified UHF.IO.File as File
import UHF.IO.Located (Located)

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

import qualified UHF.Compiler as Compiler

import qualified UHF.Data.Token as Token
import qualified UHF.Data.AST as AST
import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.RIR as RIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Keys as IR.Keys
import qualified UHF.Data.IR.Type as IR.Type
import qualified UHF.Data.AST.PP as AST.PP
import qualified UHF.Data.AST.Dump as AST.Dump
import qualified UHF.Data.IR.SIR.PP as SIR.PP
import qualified UHF.Data.IR.RIR.PP as RIR.PP
import qualified UHF.Data.IR.ANFIR.PP as ANFIR.PP

import qualified UHF.Phases.Front.Lexer as Lexer
import qualified UHF.Phases.Front.Parser as Parser
import qualified UHF.Phases.Middle.ToSIR as ToSIR
import qualified UHF.Phases.Middle.NameResolve as NameResolve
import qualified UHF.Phases.Middle.InfixGroup as InfixGroup
import qualified UHF.Phases.Middle.Type as Type
import qualified UHF.Phases.Middle.ReportHoles as ReportHoles
import qualified UHF.Phases.Middle.ToRIR as ToRIR
import qualified UHF.Phases.Middle.ToANFIR as ToANFIR
import qualified UHF.Phases.Middle.OptimizeANFIR as OptimizeANFIR
import qualified UHF.Phases.Middle.AnnotateCaptures as AnnotateCaptures
import qualified UHF.Phases.Middle.RemovePoison as RemovePoison
import qualified UHF.Phases.Back.ToDot as ToDot
import qualified UHF.Phases.Back.TSBackend as TSBackend

-- TODO: only print unerrored versions of each (double each thing in the PhaseResultsState, each one has ErrorsPossible and NoErrors variant, only print the NoErrors variant, but future phases use the ErrorsPossible variant to keep compilation going as long as possible)
type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstSIR = SIR.SIR (SIR.NameContext, [Located Text]) (SIR.NameContext, [Located Text]) (SIR.NameContext, [Located Text]) () ()
type NRSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) () ()
type InfixGroupedSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) () Void
type TypedSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) (Maybe (IR.Type.Type Void)) Void
type RIR = RIR.RIR
type ANFIR = ANFIR.ANFIR () (Maybe (IR.Type.Type Void)) ()
type ANFIRWithCaptures = ANFIR.ANFIR (Set ANFIR.BindingKey) (Maybe (IR.Type.Type Void)) ()
type NoPoisonIR = ANFIR.ANFIR (Set ANFIR.BindingKey) (IR.Type.Type Void) Void
type Dot = Text
type TS = Text

type WithDiagnosticsIO = Compiler.WithDiagnosticsT Diagnostic.Error Diagnostic.Warning IO

-- each output format requests the result of it from the state, which calculates it, caching the result so that any other output formats dont have to recalculate the result of that stage
data PhaseResultsCache
    = PhaseResultsCache
        { _get_file :: File
        , _get_tokens :: Maybe Tokens
        , _get_ast :: Maybe AST
        , _get_first_sir :: Maybe FirstSIR
        , _get_nrsir :: Maybe NRSIR
        , _get_infix_grouped :: Maybe InfixGroupedSIR
        , _get_typed_sir :: Maybe TypedSIR
        , _get_rir :: Maybe RIR
        , _get_anfir :: Maybe ANFIR
        , _get_anfir_with_captures :: Maybe ANFIRWithCaptures
        , _get_no_poison_ir :: Maybe (Maybe NoPoisonIR)
        , _get_dot :: Maybe (Maybe Dot)
        , _get_ts :: Maybe (Maybe TS)
        }
type PhaseResultsState = StateT PhaseResultsCache WithDiagnosticsIO

data OutputFormat = AST | ASTDump | SIR | NRSIR | InfixGroupedSIR | TypedSIR | RIR | ANFIR | ANFIRWithCaptures | Dot | TS
data CompileOptions
    = CompileOptions
        { input_file :: FilePath
        , module_name :: Maybe Text
        , output_formats :: [OutputFormat]
        }

compile :: FormattedString.ColorsNeeded -> DiagnosticSettings.Settings -> CompileOptions -> IO (Either () ())
compile c_needed diagnostic_settings compile_options =
    File.open (input_file compile_options) >>= \ file ->
    runWriterT (print_outputs compile_options file) >>= \ ((), diagnostics) ->
    Compiler.report_diagnostics c_needed diagnostic_settings diagnostics >>
    pure (if Compiler.had_errors diagnostics then Left () else Right ())

print_outputs :: CompileOptions -> File -> WithDiagnosticsIO ()
print_outputs compile_options file = runStateT (mapM print_output_format (output_formats compile_options)) (PhaseResultsCache file Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) >> pure ()
    where
        print_output_format AST = get_ast >>= \ ast -> lift (lift (putTextLn $ AST.PP.pp_decls ast))
        print_output_format ASTDump = get_ast >>= \ ast -> lift (lift (putTextLn $ AST.Dump.dump ast))
        print_output_format SIR = get_first_sir >>= \ ir -> lift (lift (write_output_file "uhf_sir" (SIR.PP.dump_main_module ir)))
        print_output_format NRSIR = get_nrsir >>= \ ir -> lift (lift (write_output_file "uhf_nrsir" (SIR.PP.dump_main_module ir)))
        print_output_format InfixGroupedSIR = get_infix_grouped >>= \ ir -> lift (lift (write_output_file "uhf_infix_grouped" (SIR.PP.dump_main_module ir)))
        print_output_format TypedSIR = get_typed_sir >>= \ ir -> lift (lift (write_output_file "uhf_typed_sir" (SIR.PP.dump_main_module ir)))
        print_output_format RIR = get_rir >>= \ ir -> lift (lift (write_output_file "uhf_rir" (RIR.PP.dump_main_module ir)))
        print_output_format ANFIR = get_anfir >>= \ ir -> lift (lift (write_output_file "uhf_anfir" (ANFIR.PP.dump_main_module ir)))
        print_output_format ANFIRWithCaptures = get_anfir_with_captures >>= \ ir -> lift (lift (write_output_file "uhf_anfir_captures" (ANFIR.PP.dump_main_module ir)))
        print_output_format Dot = get_dot >>= lift . lift . maybe (pure ()) (write_output_file "dot")
        print_output_format TS = get_ts >>= lift . lift . maybe (pure ()) (write_output_file "ts")

        module_name' =
            case module_name compile_options of
                Just m -> convert_str m :: FilePath
                Nothing -> FilePath.takeBaseName (input_file compile_options)

        get_output_file_path ext = FilePath.takeDirectory (input_file compile_options) FilePath.</> module_name' FilePath.<.> ext

        write_output_file ext = Text.IO.writeFile (get_output_file_path ext)

get_or_calculate :: (PhaseResultsCache -> Maybe r) -> (PhaseResultsCache -> Maybe r -> PhaseResultsCache) -> PhaseResultsState r -> PhaseResultsState r
get_or_calculate extract update calculate = extract <$> get >>= \case
    Just res -> pure res
    Nothing -> calculate >>= \ res -> modify (`update` Just res) >> pure res

convert_stage :: (Diagnostic.ToError e, Diagnostic.ToWarning w) => Compiler.WithDiagnostics e w r -> PhaseResultsState r
convert_stage s = (\ (res, diagnostics) -> lift (tell diagnostics) >> pure res) $ runWriter (Compiler.convert_diagnostics s)

get_tokens :: PhaseResultsState Tokens
get_tokens = get_or_calculate _get_tokens (\ cache tokens -> cache { _get_tokens = tokens }) lex
    where
        lex = _get_file <$> get >>= \ file -> convert_stage (Lexer.lex file)

get_ast :: PhaseResultsState AST
get_ast = get_or_calculate _get_ast (\ cache ast -> cache { _get_ast = ast }) parse_phase
    where
        parse_phase = get_tokens >>= \ (tokens, eof) -> convert_stage $ Parser.parse tokens eof

get_first_sir :: PhaseResultsState FirstSIR
get_first_sir = get_or_calculate _get_first_sir (\ cache first_sir -> cache { _get_first_sir = first_sir }) to_sir
    where
        to_sir = get_ast >>= \ ast -> convert_stage (ToSIR.convert ast)

get_nrsir :: PhaseResultsState NRSIR
get_nrsir = get_or_calculate _get_nrsir (\ cache nrsir -> cache { _get_nrsir = nrsir }) name_resolve
    where
        name_resolve = get_first_sir >>= \ sir -> convert_stage (NameResolve.resolve sir)

get_infix_grouped :: PhaseResultsState InfixGroupedSIR
get_infix_grouped = get_or_calculate _get_infix_grouped (\ cache infix_grouped -> cache { _get_infix_grouped = infix_grouped }) group_infix
    where
        group_infix = InfixGroup.group <$> get_nrsir

get_typed_sir :: PhaseResultsState TypedSIR
get_typed_sir = get_or_calculate _get_typed_sir (\ cache typed_sir -> cache { _get_typed_sir = typed_sir }) type_
    where
        type_ =
            get_infix_grouped >>= \ infix_grouped_ir ->
            convert_stage (Type.typecheck infix_grouped_ir) >>= \ typed_ir ->
            convert_stage (ReportHoles.report_holes typed_ir) >>
            pure typed_ir

get_rir :: PhaseResultsState RIR
get_rir = get_or_calculate _get_rir (\ cache rir -> cache { _get_rir = rir }) to_rir
    where
        to_rir = ToRIR.convert <$> get_typed_sir

get_anfir :: PhaseResultsState ANFIR
get_anfir = get_or_calculate _get_anfir (\ cache anfir -> cache { _get_anfir = anfir }) to_anfir
    where
        to_anfir = ToANFIR.convert <$> get_rir

get_optimized_anfir :: PhaseResultsState ANFIR
get_optimized_anfir = get_or_calculate _get_anfir (\ cache anfir -> cache { _get_anfir = anfir }) to_anfir
    where
        -- TODO: move after remove poison?
        to_anfir = OptimizeANFIR.optimize <$> get_anfir -- TODO: optimization levels

get_anfir_with_captures :: PhaseResultsState ANFIRWithCaptures
get_anfir_with_captures = get_or_calculate _get_anfir_with_captures (\ cache anfir -> cache { _get_anfir_with_captures = anfir }) to_anfir_with_captures
    where
        to_anfir_with_captures = AnnotateCaptures.annotate <$> get_optimized_anfir

get_no_poison_ir :: PhaseResultsState (Maybe NoPoisonIR)
get_no_poison_ir = get_or_calculate _get_no_poison_ir (\ cache no_poison_ir -> cache { _get_no_poison_ir = no_poison_ir }) remove_poison
    where
        remove_poison = RemovePoison.remove_poison <$> get_anfir_with_captures

get_dot :: PhaseResultsState (Maybe Dot)
get_dot = get_or_calculate _get_dot (\ cache dot -> cache { _get_dot = dot }) to_dot
    where
        to_dot = get_no_poison_ir >>= maybe (pure Nothing) (\ anfir -> pure (Just $ ToDot.to_dot anfir))

get_ts :: PhaseResultsState (Maybe TS)
get_ts = get_or_calculate _get_ts (\ cache ts -> cache { _get_ts = ts }) to_ts
    where
        to_ts = get_no_poison_ir >>= maybe (pure Nothing) (\ anfir -> pure (Just $ TSBackend.lower anfir))
