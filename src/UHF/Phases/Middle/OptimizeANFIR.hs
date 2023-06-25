module UHF.Phases.Middle.OptimizeANFIR (optimize) where

-- import UHF.Util.Prelude

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Phases.Middle.OptimizeANFIR.RemoveRefers as RemoveRefers

optimize :: ANFIR.ANFIR -> ANFIR.ANFIR
optimize = RemoveRefers.optimize
