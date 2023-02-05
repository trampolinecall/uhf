module UHF.NameResolve where

import UHF.Util.Prelude

import qualified UHF.IO.File as File
import qualified UHF.IO.Location as Location
import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.IR.Decl as IR.Decl
import qualified UHF.IR.Value as IR.Value

data Error

instance Diagnostic.IsDiagnostic Error where

resolve :: IR.Decl.Module (Location.Located [(Location.Located Text)]) -> Writer [Error] (IR.Decl.Module (Location.Located IR.Value.ResolvedValue))
resolve = not_implemented
