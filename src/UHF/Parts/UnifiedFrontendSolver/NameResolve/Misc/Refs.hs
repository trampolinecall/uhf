module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs
    ( DeclRef (..)
    , ValueRef (..)
    ) where

import UHF.Prelude

import UHF.Data.SIR (ExternPackage, ModuleKey, VariableKey)
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.Intrinsics as Intrinsics

data DeclRef ty
    = DeclRef'Module ModuleKey
    | DeclRef'Type ty
    | DeclRef'ExternPackage ExternPackage -- TODO: change this to ExternModule? because referring to an external package would just refer to its root module
    deriving Show

data ValueRef
    = ValueRef'Variable VariableKey
    | ValueRef'ADTVariantConstructor Type.ADT.VariantIndex
    | ValueRef'Intrinsic Intrinsics.Intrinsic
    deriving Show
