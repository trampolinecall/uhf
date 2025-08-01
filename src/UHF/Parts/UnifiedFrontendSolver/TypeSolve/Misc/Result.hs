{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Misc.Result (TypeInfo(..), FinalTypeInfo(..), empty_type_info) where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR.ID as SIR.ID
import qualified Data.Map as Map

data TypeInfo = TypeInfo
    { expr_types :: Map (SIR.ID.ID "Expr") TypeWithInferVar.Type
    , variable_types :: Map (SIR.ID.ID "Variable") TypeWithInferVar.Type
    , pattern_types :: Map (SIR.ID.ID "Pattern") TypeWithInferVar.Type
    , variant_pattern_applications :: Map (SIR.ID.ID "VariantPattern") [TypeWithInferVar.Type]
    }

data FinalTypeInfo = FinalTypeInfo
    { final_expr_types :: Map (SIR.ID.ID "Expr") (Maybe Type.Type)
    , final_variable_types :: Map (SIR.ID.ID "Variable") (Maybe Type.Type)
    , final_pattern_types :: Map (SIR.ID.ID "Pattern") (Maybe Type.Type)
    , final_variant_pattern_applications :: Map (SIR.ID.ID "VariantPattern") [Maybe Type.Type]
    }

empty_type_info :: TypeInfo
empty_type_info = TypeInfo Map.empty Map.empty Map.empty Map.empty
