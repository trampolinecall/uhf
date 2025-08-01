module UHF.Driver
    ( CompileOptions(..)
    , OutputFormat(..)
    , compile
    , compile_returning_diagnostics
    ) where

import UHF.Prelude

import qualified Data.Text.IO as Text.IO
import qualified System.FilePath as FilePath
import qualified Pipes

import UHF.Source.File (File)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.ANFIR as ANFIR
import qualified UHF.Data.ANFIR.PP as ANFIR.PP
import qualified UHF.Data.AST as AST
import qualified UHF.Data.AST.PP as AST.PP
import qualified UHF.Data.BackendIR as BackendIR
import qualified UHF.Data.BackendIR.PP as BackendIR.PP
import qualified UHF.Data.IR.Type as IR.Type
import qualified UHF.Data.RIR as RIR
import qualified UHF.Data.RIR.PP as RIR.PP
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.SIR.PP as SIR.PP
import qualified UHF.Diagnostic as Diagnostic
import qualified UHF.Diagnostic.Settings as DiagnosticSettings
import qualified UHF.Parts.Lexer as Lexer
import qualified UHF.Parts.OptimizeANFIR as OptimizeANFIR
import qualified UHF.Parts.Parser as Parser
import qualified UHF.Parts.RemovePoison as RemovePoison
import qualified UHF.Parts.ReportHoles as ReportHoles
import qualified UHF.Parts.TSBackend as TSBackend
import qualified UHF.Parts.ToANFIR as ToANFIR
import qualified UHF.Parts.ToBackendIR as ToBackendIR
import qualified UHF.Parts.ToRIR as ToRIR
import qualified UHF.Parts.ToSIR as ToSIR
import qualified UHF.Source.File as File
import qualified UHF.Source.FormattedString as FormattedString
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledKey, TypeExprEvaledAsTypeKey)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult, InfixGroupedKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameResolve.NameMaps
import qualified UHF.Parts.UnifiedFrontendSolver as UnifiedFrontendSolver
import Data.Functor.Const (Const)

type AST = [AST.Decl]
type FirstSIR = SIR.SIR ((), Const () (), (), (), (), (), ())
type SolvedSIR = (SIR.SIR (NameResolve.NameMaps.NameContextKey, IdenResolvedKey (), IR.Type.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, Maybe IR.Type.Type, InfixGroupedKey)
        , Arena.Arena (Maybe (SIR.DeclRef IR.Type.Type)) (IdenResolvedKey (SIR.DeclRef IR.Type.Type))
        , Arena.Arena (Maybe SIR.ValueRef) (IdenResolvedKey SIR.ValueRef)
        , Arena.Arena (Maybe SIR.ADTVariantIndex) (IdenResolvedKey SIR.ADTVariantIndex)
        , Arena.Arena (Maybe (SIR.DeclRef IR.Type.Type)) TypeExprEvaledKey
        , Arena.Arena (Maybe IR.Type.Type) TypeExprEvaledAsTypeKey
        , Arena.Arena (Maybe InfixGroupResult) InfixGroupedKey)
type RIR = RIR.RIR
type ANFIR = ANFIR.ANFIR
type BackendIR = BackendIR.BackendIR (Either BackendIR.HasLoops BackendIR.TopologicallySorted) (Maybe IR.Type.Type) ()
type NoPoisonIR = BackendIR.BackendIR BackendIR.TopologicallySorted IR.Type.Type Void
type TS = Text

type WithDiagnosticsIO = Compiler.WithDiagnosticsT Diagnostic.Error Diagnostic.Warning IO

-- each output format requests the result of it from the state, which calculates it, caching the result so that any other output formats dont have to recalculate the result of that stage
data PhaseResultsCache
    = PhaseResultsCache
        { _get_file :: File
        , _get_ast :: Maybe (AST, Outputable)
        , _get_first_sir :: Maybe (FirstSIR, Outputable)
        , _get_solved_sir :: Maybe (SolvedSIR, Outputable)
        , _get_rir :: Maybe (RIR, Outputable)
        , _get_anfir :: Maybe (ANFIR, Outputable)
        , _get_optimized_anfir :: Maybe (ANFIR, Outputable)
        , _get_backend_ir :: Maybe (BackendIR, Outputable)
        , _get_no_poison_ir :: Maybe (Either () NoPoisonIR)
        , _get_ts :: Maybe (Either () TS)
        }
type PhaseResultsState = StateT PhaseResultsCache WithDiagnosticsIO

data OutputFormat = AST | SIR | SolvedSIR | RIR | ANFIR | OptimizedANFIR | BackendIR | TS | Check
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

compile_returning_diagnostics :: CompileOptions -> IO (Either (Compiler.Diagnostics Diagnostic.Error Diagnostic.Warning) ())
compile_returning_diagnostics compile_options =
    File.open (input_file compile_options) >>= \ file ->
    runWriterT (print_outputs compile_options file) >>= \ ((), diagnostics) ->
    pure (if Compiler.had_errors diagnostics then Left diagnostics else Right ())

print_outputs :: CompileOptions -> File -> WithDiagnosticsIO ()
print_outputs compile_options file = evalStateT (mapM_ print_output_format (output_formats compile_options)) (PhaseResultsCache file Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
    where
        print_output_format AST = get_ast >>= output_if_outputable (lift . lift . putTextLn . AST.PP.pp_decls)
        print_output_format SIR = get_first_sir >>= output_if_outputable (lift . lift . write_output_file "uhf_sir" . SIR.PP.dump_main_module)
        print_output_format SolvedSIR = get_solved_sir >>= output_if_outputable (\ (ir, _, _, _, _, _, _) -> lift (lift (write_output_file "uhf_solved_sir" (SIR.PP.dump_main_module ir))))
        print_output_format RIR = get_rir >>= output_if_outputable (lift . lift . write_output_file "uhf_rir" . RIR.PP.dump_main_module)
        print_output_format ANFIR = get_anfir >>= output_if_outputable (lift . lift . write_output_file "uhf_anfir" . ANFIR.PP.dump_cu)
        print_output_format OptimizedANFIR = get_optimized_anfir >>= output_if_outputable (lift . lift . write_output_file "uhf_anfir_optimized" . ANFIR.PP.dump_cu)
        print_output_format BackendIR = get_backend_ir >>= output_if_outputable (lift . lift . write_output_file "uhf_backend_ir" . BackendIR.PP.dump_cu)
        print_output_format TS = get_ts >>= output_when_right (lift . lift . write_output_file "ts")
        print_output_format Check = get_ts >> pure () -- TODO: this probably doesnt need to go all the way to TS

        output_if_outputable output (s, Outputable) = output s
        output_if_outputable _ (_, HadError) = pure ()

        output_when_right output (Right s) = output s
        output_when_right _ (Left _) = pure ()

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

-- convert WithDiagnostics into a PhaseResultsState by reporting the diagnostics onto IO, and also changes the result into (r, Outputable)
convert_errors :: (Diagnostic.ToError e, Diagnostic.ToWarning w) => Compiler.WithDiagnostics e w r -> PhaseResultsState (r, Outputable)
convert_errors s = do
    let (res, diagnostics) = runWriter (Compiler.convert_diagnostics s)
    lift (tell diagnostics)
    pure (res, if Compiler.had_errors diagnostics then HadError else Outputable)

-- for the phases that result in a (result, Outputable), the Outputable will be HadError if that phase or any of the previous phases had an error, indicating that the result of that phase is not completely correct and therefore cannot be outputed
-- if the Outputable is Outputable, then there had been no errors leading up to this phase and that result can be outputted
data Outputable = Outputable | HadError
instance Semigroup Outputable where
    HadError <> _ = HadError
    Outputable <> b = b

run_stage_on_previous_stage_output :: (previous -> PhaseResultsState (result, Outputable)) -> (previous, Outputable) -> PhaseResultsState (result, Outputable)
run_stage_on_previous_stage_output f (a, last_outputable) = do
    (res, this_outputable) <- f a
    pure (res, last_outputable <> this_outputable)

on_tuple_first :: (a -> b) -> (a, c) -> (b, c)
on_tuple_first f (a, b) = (f a, b)

-- TODO: clean up this file

-- TODO: use template haskell for this?
-- all of these functions return the result of a phase either by calculating it or retrieving it from the cache
get_ast :: PhaseResultsState (AST, Outputable)
get_ast = get_or_calculate _get_ast (\ cache ast -> cache { _get_ast = ast }) parse_phase
    where
        parse_phase :: StateT PhaseResultsCache WithDiagnosticsIO (AST, Outputable)
        parse_phase = do
            file <- _get_file <$> get
            let tokens = Lexer.lex file
            let tokens_converted = Pipes.hoist Compiler.convert_diagnostics (absurd <$> tokens)
            let parse = Pipes.hoist Compiler.convert_diagnostics Parser.parse
            convert_errors $ Pipes.runEffect $ tokens_converted Pipes.>-> parse

get_first_sir :: PhaseResultsState (FirstSIR, Outputable)
get_first_sir = get_or_calculate _get_first_sir (\ cache first_sir -> cache { _get_first_sir = first_sir }) to_sir
    where
        to_sir = get_ast >>= run_stage_on_previous_stage_output (convert_errors . ToSIR.convert)

get_solved_sir :: PhaseResultsState (SolvedSIR, Outputable)
get_solved_sir = get_or_calculate _get_solved_sir (\ cache solved_sir -> cache { _get_solved_sir = solved_sir }) solve_sir
    where
        solve_sir = do
            sir <- get_first_sir
            ((solved_sir, (decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena), infix_group_result_arena), outputable) <- run_stage_on_previous_stage_output (convert_errors . UnifiedFrontendSolver.solve) sir
            ((), _) <- convert_errors (ReportHoles.report_holes type_expr_evaled_as_type_arena solved_sir)
            pure ((solved_sir, decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena, infix_group_result_arena), outputable)

get_rir :: PhaseResultsState (RIR, Outputable)
get_rir = get_or_calculate _get_rir (\ cache rir -> cache { _get_rir = rir }) to_rir
    where
        to_rir = get_solved_sir >>= run_stage_on_previous_stage_output (convert_errors . (\ (solved_sir, decl_iden_resolved_arena, value_iden_resolved_arena, variant_iden_resolved_arena, type_expr_evaled_arena, type_expr_evaled_as_type_arena, infix_group_result_arena ) -> ToRIR.convert decl_iden_resolved_arena value_iden_resolved_arena variant_iden_resolved_arena type_expr_evaled_arena type_expr_evaled_as_type_arena infix_group_result_arena solved_sir))

get_anfir :: PhaseResultsState (ANFIR, Outputable)
get_anfir = get_or_calculate _get_anfir (\ cache anfir -> cache { _get_anfir = anfir }) to_anfir
    where
        to_anfir = on_tuple_first ToANFIR.convert <$> get_rir

get_optimized_anfir :: PhaseResultsState (ANFIR, Outputable)
get_optimized_anfir = get_or_calculate _get_optimized_anfir (\ cache anfir -> cache { _get_optimized_anfir = anfir }) optimize_anfir
    where
        -- TODO: move after remove poison?
        optimize_anfir = on_tuple_first OptimizeANFIR.optimize <$> get_anfir -- TODO: optimization levels

get_backend_ir :: PhaseResultsState (BackendIR, Outputable)
get_backend_ir = get_or_calculate _get_backend_ir (\ cache backend_ir -> cache { _get_backend_ir = backend_ir }) to_backend_ir
    where
        to_backend_ir = on_tuple_first ToBackendIR.convert <$> get_optimized_anfir

get_no_poison_ir :: PhaseResultsState (Either () NoPoisonIR)
get_no_poison_ir = get_or_calculate _get_no_poison_ir (\ cache no_poison_ir -> cache { _get_no_poison_ir = no_poison_ir }) remove_poison
    where
        remove_poison = get_backend_ir <&> \case
            (bir, Outputable) -> case RemovePoison.remove_poison bir of
                Just res -> Right res
                Nothing -> Left ()
            _ -> Left ()

get_ts :: PhaseResultsState (Either () TS)
get_ts = get_or_calculate _get_ts (\ cache ts -> cache { _get_ts = ts }) to_ts
    where
        to_ts = get_no_poison_ir <&> (TSBackend.lower <$>)
