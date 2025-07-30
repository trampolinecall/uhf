module UHF.Parts.UnifiedFrontendSolver.Solving
    ( SolvingStage
    , SolveMonad
    , ask_name_maps_arena
    , ask_sir_child_maps
    , ask_sir
    , get_decl_iden_resolved
    , get_value_iden_resolved
    , get_variant_iden_resolved
    , get_type_expr_evaled
    , get_type_expr_evaled_as_type
    ) where

import UHF.Prelude

import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.InfixGroupResultArena (InfixGroupedArena, InfixGroupedKey)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena
    ( IdenResolvedArena
    , IdenResolvedKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledKey
    )
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeWithInferVar
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.SolveMonad as SolveMonad
import qualified UHF.Util.Arena as Arena
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error

type SolvingStage =
    ( NameMaps.NameMapStackKey
    , IdenResolvedKey ()
    , TypeWithInferVar.Type
    , TypeExprEvaledKey
    , TypeExprEvaledAsTypeKey
    , TypeWithInferVar.Type
    , InfixGroupedKey
    )

type SolveMonad =
    StateT
        ( ( IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
          , IdenResolvedArena SIR.ValueRef
          , IdenResolvedArena Type.ADT.VariantIndex
          , TypeExprEvaledArena
          , TypeExprEvaledAsTypeArena
          )
        , InfixGroupedArena ()
        )
        ( ReaderT
            (Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey, NameMaps.SIRChildMaps, SIR.SIR SolvingStage)
            (SolveMonad.SolveMonad (Compiler.WithDiagnostics Error.Error Void))
        )

ask_name_maps_arena :: SolveMonad (Arena.Arena NameMaps.NameMapStack NameMaps.NameMapStackKey)
ask_name_maps_arena = (\(name_maps_arena, _, _) -> name_maps_arena) <$> ask
ask_sir_child_maps :: SolveMonad NameMaps.SIRChildMaps
ask_sir_child_maps = (\(_, sir_child_maps, _) -> sir_child_maps) <$> ask
ask_sir :: SolveMonad (SIR.SIR SolvingStage)
ask_sir = (\(_, _, sir) -> sir) <$> ask

get_decl_iden_resolved :: IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type) -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeWithInferVar.Type))
get_decl_iden_resolved key = do
    ((decl_iden_resolved_arena, _, _, _, _), _) <- get
    pure $ Arena.get decl_iden_resolved_arena key

get_value_iden_resolved :: IdenResolvedKey SIR.ValueRef -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise SIR.ValueRef)
get_value_iden_resolved key = do
    ((_, value_iden_resolved_arena, _, _, _), _) <- get
    pure $ Arena.get value_iden_resolved_arena key

get_variant_iden_resolved :: IdenResolvedKey Type.ADT.VariantIndex -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
get_variant_iden_resolved key = do
    ((_, _, variant_iden_resolved_arena, _, _), _) <- get
    pure $ Arena.get variant_iden_resolved_arena key

get_type_expr_evaled :: TypeExprEvaledKey -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise (SIR.DeclRef TypeWithInferVar.Type))
get_type_expr_evaled key = do
    ((_, _, _, type_expr_evaled_arena, _), _) <- get
    pure $ Arena.get type_expr_evaled_arena key

get_type_expr_evaled_as_type :: TypeExprEvaledAsTypeKey -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type)
get_type_expr_evaled_as_type key = do
    ((_, _, _, _, type_expr_evaled_as_type_arena), _) <- get
    pure $ Arena.get type_expr_evaled_as_type_arena key
