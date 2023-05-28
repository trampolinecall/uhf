module UHF.Phases.Middle.OptimizeANFIR where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.ANFIR as ANFIR

type ANFIR = ANFIR.ANFIR () (Maybe (Type.Type Void)) ()

optimize :: ANFIR -> ANFIR
optimize = identity
