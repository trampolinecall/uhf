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
import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Keys as IR.Keys
import qualified UHF.Data.IR.Type as IR.Type
import qualified UHF.Data.AST.PP as AST.PP
import qualified UHF.Data.AST.Dump as AST.Dump
import qualified UHF.Data.IR.SIR.PP as SIR.PP
import qualified UHF.Data.IR.RIR.PP as RIR.PP
import qualified UHF.Data.IR.ANFIR.PP as ANFIR.PP
import qualified UHF.Data.IR.BackendIR.PP as BackendIR.PP

import qualified UHF.Phases.Lexer as Lexer
import qualified UHF.Phases.Parser as Parser
import qualified UHF.Phases.ToSIR as ToSIR
import qualified UHF.Phases.NameResolve as NameResolve
import qualified UHF.Phases.InfixGroup as InfixGroup
import qualified UHF.Phases.Type as Type
import qualified UHF.Phases.ReportHoles as ReportHoles
import qualified UHF.Phases.ToRIR as ToRIR
import qualified UHF.Phases.ToANFIR as ToANFIR
import qualified UHF.Phases.OptimizeANFIR as OptimizeANFIR
import qualified UHF.Phases.ToBackendIR as ToBackendIR
import qualified UHF.Phases.RemovePoison as RemovePoison
import qualified UHF.Phases.TSBackend as TSBackend

type Tokens = ([Token.LToken], Token.LToken)
type AST = [AST.Decl]
type FirstSIR = SIR.SIR ([Located Text]) ([Located Text]) ([Located Text]) () ()
type NRSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) () ()
type InfixGroupedSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) () Void
type TypedSIR = SIR.SIR (Maybe IR.Keys.DeclKey) (Located (Maybe IR.Keys.BoundValueKey)) (Maybe IR.Type.ADTVariantIndex) (Maybe (IR.Type.Type Void)) Void
type RIR = RIR.RIR
type ANFIR = ANFIR.ANFIR
type BackendIR = BackendIR.BackendIR (Maybe (IR.Type.Type Void)) ()
type NoPoisonIR = BackendIR.BackendIR (IR.Type.Type Void) Void
type TS = Text

type WithDiagnosticsIO = Compiler.WithDiagnosticsT Diagnostic.Error Diagnostic.Warning IO

-- each output format requests the result of it from the state, which calculates it, caching the result so that any other output formats dont have to recalculate the result of that stage
data PhaseResultsCache
    = PhaseResultsCache
        { _get_file :: File
        , _get_tokens :: Maybe (Either Tokens Tokens)
        , _get_ast :: Maybe (Either AST AST)
        , _get_first_sir :: Maybe (Either FirstSIR FirstSIR)
        , _get_nrsir :: Maybe (Either NRSIR NRSIR)
        , _get_infix_grouped :: Maybe (Either InfixGroupedSIR InfixGroupedSIR)
        , _get_typed_sir :: Maybe (Either TypedSIR TypedSIR)
        , _get_rir :: Maybe (Either RIR RIR)
        , _get_anfir :: Maybe (Either ANFIR ANFIR)
        , _get_optimized_anfir :: Maybe (Either ANFIR ANFIR)
        , _get_backend_ir :: Maybe (Either BackendIR BackendIR)
        , _get_no_poison_ir :: Maybe (Either () NoPoisonIR)
        , _get_ts :: Maybe (Either () TS)
        }
type PhaseResultsState = StateT PhaseResultsCache WithDiagnosticsIO

data OutputFormat = AST | ASTDump | SIR | NRSIR | InfixGroupedSIR | TypedSIR | RIR | ANFIR | OptimizedANFIR | BackendIR | TS
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
        print_output_format AST = get_ast >>= when_right (\ ast -> lift (lift (putTextLn $ AST.PP.pp_decls ast)))
        print_output_format ASTDump = get_ast >>= when_right (\ ast -> lift (lift (putTextLn $ AST.Dump.dump ast)))
        print_output_format SIR = get_first_sir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_sir" (SIR.PP.dump_main_module ir))))
        print_output_format NRSIR = get_nrsir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_nrsir" (SIR.PP.dump_main_module ir))))
        print_output_format InfixGroupedSIR = get_infix_grouped >>= when_right (\ ir -> lift (lift (write_output_file "uhf_infix_grouped" (SIR.PP.dump_main_module ir))))
        print_output_format TypedSIR = get_typed_sir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_typed_sir" (SIR.PP.dump_main_module ir))))
        print_output_format RIR = get_rir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_rir" (RIR.PP.dump_cu ir))))
        print_output_format ANFIR = get_anfir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_anfir" (ANFIR.PP.dump_cu ir))))
        print_output_format OptimizedANFIR = get_optimized_anfir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_anfir_optimized" (ANFIR.PP.dump_cu ir))))
        print_output_format BackendIR = get_backend_ir >>= when_right (\ ir -> lift (lift (write_output_file "uhf_backend_ir" (BackendIR.PP.dump_cu ir))))
        print_output_format TS = get_ts >>= when_right (lift . lift . write_output_file "ts")

        when_right output (Right s) = output s
        when_right _ (Left _) = pure ()

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

-- convert a stage with WithDiagnostics into a PhaseResultsState, and also changes the result into Either r r by making it Left r if there had been errors in this phase and Right r if there had been no errors
convert_stage :: (Diagnostic.ToError e, Diagnostic.ToWarning w) => Compiler.WithDiagnostics e w r -> PhaseResultsState (Either r r)
convert_stage s = runWriter (Compiler.convert_diagnostics s) & \ (res, diagnostics) -> lift (tell diagnostics) >> pure (if Compiler.had_errors diagnostics then Left res else Right res)

run_stage_on_both_either :: Functor m => (a -> m (Either b b)) -> Either a a -> m (Either b b)
run_stage_on_both_either f (Right a) = f a <&> \case
    Right r -> Right r
    Left l -> Left l
run_stage_on_both_either f (Left a) = f a <&> \case
    Right r -> Left r
    Left l -> Left l

map_either :: (a -> b) -> Either a a -> Either b b
map_either f (Right a) = Right $ f a
map_either f (Left a) = Left $ f a

--- TODO: use template haskell for this?
-- all of these functions return the result of a phase either by calculating it or retrieving it from the cache
-- for the phases whose results are in the form Either a a, the result will be Left a if that phase had an error or any of the previous phases had an error (which for most of the phases is accomplished with run_stage_on_both_either above)
-- the result will be Right a if there have been no errors in the chain of phases that led to the result of this phase
get_tokens :: PhaseResultsState (Either Tokens Tokens)
get_tokens = get_or_calculate _get_tokens (\ cache tokens -> cache { _get_tokens = tokens }) lex
    where
        lex = _get_file <$> get >>= \ file -> convert_stage (Lexer.lex file)

get_ast :: PhaseResultsState (Either AST AST)
get_ast = get_or_calculate _get_ast (\ cache ast -> cache { _get_ast = ast }) parse_phase
    where
        parse_phase = get_tokens >>= run_stage_on_both_either (\ (tokens, eof) -> convert_stage $ Parser.parse tokens eof)

get_first_sir :: PhaseResultsState (Either FirstSIR FirstSIR)
get_first_sir = get_or_calculate _get_first_sir (\ cache first_sir -> cache { _get_first_sir = first_sir }) to_sir
    where
        to_sir = get_ast >>= run_stage_on_both_either (\ ast -> convert_stage (ToSIR.convert ast))

get_nrsir :: PhaseResultsState (Either NRSIR NRSIR)
get_nrsir = get_or_calculate _get_nrsir (\ cache nrsir -> cache { _get_nrsir = nrsir }) name_resolve
    where
        name_resolve = get_first_sir >>= run_stage_on_both_either (\ sir -> convert_stage (NameResolve.resolve sir))

get_infix_grouped :: PhaseResultsState (Either InfixGroupedSIR InfixGroupedSIR)
get_infix_grouped = get_or_calculate _get_infix_grouped (\ cache infix_grouped -> cache { _get_infix_grouped = infix_grouped }) group_infix
    where
        group_infix = map_either InfixGroup.group <$> get_nrsir

get_typed_sir :: PhaseResultsState (Either TypedSIR TypedSIR)
get_typed_sir = get_or_calculate _get_typed_sir (\ cache typed_sir -> cache { _get_typed_sir = typed_sir }) type_
    where
        type_ =
            get_infix_grouped >>=
            run_stage_on_both_either (\ infix_grouped_ir -> convert_stage (Type.typecheck infix_grouped_ir)) >>= \ typed_ir ->
            run_stage_on_both_either (\ typed_ir -> convert_stage (ReportHoles.report_holes typed_ir)) typed_ir >>
            pure typed_ir

get_rir :: PhaseResultsState (Either RIR RIR)
get_rir = get_or_calculate _get_rir (\ cache rir -> cache { _get_rir = rir }) to_rir
    where
        to_rir = get_typed_sir >>= run_stage_on_both_either (convert_stage . ToRIR.convert)

get_anfir :: PhaseResultsState (Either ANFIR ANFIR)
get_anfir = get_or_calculate _get_anfir (\ cache anfir -> cache { _get_anfir = anfir }) to_anfir
    where
        to_anfir = map_either ToANFIR.convert <$> get_rir

get_optimized_anfir :: PhaseResultsState (Either ANFIR ANFIR)
get_optimized_anfir = get_or_calculate _get_optimized_anfir (\ cache anfir -> cache { _get_optimized_anfir = anfir }) optimize_anfir
    where
        -- TODO: move after remove poison?
        optimize_anfir = map_either OptimizeANFIR.optimize <$> get_anfir -- TODO: optimization levels

get_backend_ir :: PhaseResultsState (Either BackendIR BackendIR)
get_backend_ir = get_or_calculate _get_backend_ir (\ cache backend_ir -> cache { _get_backend_ir = backend_ir }) to_backend_ir
    where
        to_backend_ir = map_either ToBackendIR.convert <$> get_optimized_anfir

get_no_poison_ir :: PhaseResultsState (Either () NoPoisonIR)
get_no_poison_ir = get_or_calculate _get_no_poison_ir (\ cache no_poison_ir -> cache { _get_no_poison_ir = no_poison_ir }) remove_poison
    where
        remove_poison = get_backend_ir <&> \case
            Right bir -> case RemovePoison.remove_poison bir of
                Just res -> Right res
                Nothing -> Left ()
            Left _ -> Left ()

get_ts :: PhaseResultsState (Either () TS)
get_ts = get_or_calculate _get_ts (\ cache ts -> cache { _get_ts = ts }) to_ts
    where
        to_ts = get_no_poison_ir <&> (TSBackend.lower <$>)
