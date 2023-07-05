module UHF.Phases.Middle.OptimizeANFIR (optimize) where

import UHF.Util.Prelude

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Phases.Middle.OptimizeANFIR.RemoveRefers as RemoveRefers

optimize :: ANFIR.ANFIR -> ANFIR.ANFIR
optimize = identity -- TODO: RemoveRefers.optimize this messes up capture lists
-- for example in
-- other = ...
-- outside = other
-- l = \ param -> {
--         capture outside;
--         capture = outside;
--     }
--     ...
-- a is optimized to 'a = other' but the capture list isnt updated to say 'capture other;'
