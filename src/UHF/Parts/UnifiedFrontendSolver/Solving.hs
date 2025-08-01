{-# LANGUAGE DataKinds #-}

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

import Data.Functor.Const (Const)
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.SIR.ID as SIR.ID
import UHF.Parts.UnifiedFrontendSolver.Error (Error)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResults)
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as NameResolve.Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef, ValueRef)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenResults
    , TypeExprsEvaled
    , TypeExprsEvaledAsTypes
    , ValueIdenResults
    , VariantIdenResults
    )
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.Result (TypeInfo)
import qualified UHF.Util.Arena as Arena

-- TODO: remove stage
type SolvingStage = ( NameMaps.NameContextKey , Const () () , () , () , () , () , ())

type SolveMonad =
    StateT
        ( ( DeclIdenResults
          , ValueIdenResults
          , VariantIdenResults
          , TypeExprsEvaled
          , TypeExprsEvaledAsTypes
          )
        , InfixGroupResults
        , (TypeInfo, TypeWithInferVar.InferVarArena)
        )
        -- TODO: eventually this should also be in the StateT because macro expansion can add to NameMaps and ChildMaps and identifier patterns need to be able to resolve as adt variant patterns with no fields
        ( ReaderT
            (Arena.Arena NameMaps.NameContext NameMaps.NameContextKey, NameMaps.SIRChildMaps, SIR.SIR SolvingStage)
            (Compiler.WithDiagnostics Error Void)
        )

ask_name_maps_arena :: SolveMonad (Arena.Arena NameMaps.NameContext NameMaps.NameContextKey)
ask_name_maps_arena = (\(name_maps_arena, _, _) -> name_maps_arena) <$> ask
ask_sir_child_maps :: SolveMonad NameMaps.SIRChildMaps
ask_sir_child_maps = (\(_, sir_child_maps, _) -> sir_child_maps) <$> ask
ask_sir :: SolveMonad (SIR.SIR SolvingStage)
ask_sir = (\(_, _, sir) -> sir) <$> ask

get_decl_iden_resolved ::
    SIR.ID.ID "DeclIden" -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
get_decl_iden_resolved key = do
    ((decl_iden_resolved_arena, _, _, _, _), _, _) <- get
    pure $ decl_iden_resolved_arena Map.! key

get_value_iden_resolved ::
    SIR.ID.ID "ValueIden" -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise ValueRef)
get_value_iden_resolved key = do
    ((_, value_iden_resolved_arena, _, _, _), _, _) <- get
    pure $ value_iden_resolved_arena Map.! key

get_variant_iden_resolved ::
    SIR.ID.ID "VariantIden" -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise Type.ADT.VariantIndex)
get_variant_iden_resolved key = do
    ((_, _, variant_iden_resolved_arena, _, _), _, _) <- get
    pure $ variant_iden_resolved_arena Map.! key

get_type_expr_evaled ::
    SIR.ID.ID "TypeExpr" -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise (DeclRef TypeWithInferVar.Type))
get_type_expr_evaled key = do
    ((_, _, _, type_exprs_evaled, _), _, _) <- get
    pure $ type_exprs_evaled Map.! key

get_type_expr_evaled_as_type ::
    SIR.ID.ID "TypeExprEvaledAsType" -> SolveMonad (SolveResult (Maybe NameResolve.Error.Error) Compiler.ErrorReportedPromise TypeWithInferVar.Type)
get_type_expr_evaled_as_type key = do
    ((_, _, _, _, type_exprs_evaled_as_types), _, _) <- get
    pure $ type_exprs_evaled_as_types Map.! key
