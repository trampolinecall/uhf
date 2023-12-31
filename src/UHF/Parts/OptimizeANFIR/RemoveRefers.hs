module UHF.Parts.OptimizeANFIR.RemoveRefers (optimize) where

import UHF.Prelude

import UHF.Parts.OptimizeANFIR.Utils
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.ANFIR as ANFIR

optimize :: ANFIR.ANFIR -> ANFIR.ANFIR
optimize anfir = runReader (iterate_over_all_subexpressions trace_passthrough anfir) anfir

trace_passthrough :: ANFIR.BindingKey -> Reader ANFIR.ANFIR ANFIR.BindingKey
trace_passthrough bk = trace [] bk
    where
        trace already_visited bk
            | bk `elem` already_visited = pure bk
            | otherwise =
                get_binding bk >>= \case
                    -- if this expression is referring to a refer node, recursively trace through the refer node's referents and make the expression refer to that
                    ANFIR.Binding (ANFIR.Expr'Refer _ _ referent) -> trace (bk:already_visited) referent
                    _ -> pure bk

        get_binding bk = ask >>= \ (ANFIR.ANFIR _ _ _ _ _ bindings _ _) -> pure (Arena.get bindings bk)

