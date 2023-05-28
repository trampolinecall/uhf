module UHF.Phases.Middle.OptimizeANFIR (optimize) where

-- import UHF.Util.Prelude

import UHF.Phases.Middle.OptimizeANFIR.Utils
import qualified UHF.Phases.Middle.OptimizeANFIR.RemoveRefers as RemoveRefers

optimize :: ANFIR -> ANFIR
optimize = RemoveRefers.optimize
