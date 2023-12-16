module UHF.Phases.NameResolve.NRReader
    ( NRReader

    , ask_adt_arena -- TODO: remove
    , ask_var_arena -- TODO: remove?
    , ask_type_var_arena -- TODO: remove?
    , ask_sir_child_maps -- TODO: remove?
    ) where

-- TODO: clean up this and the 3 modules too

import UHF.Prelude

type NRReader adt_arena var_arena type_var_arena sir_child_maps = ReaderT (adt_arena, var_arena, type_var_arena, sir_child_maps)

ask_adt_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _) -> pure adts
ask_var_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under var_arena
ask_var_arena = ReaderT $ \ (_, vars, _, _) -> pure vars
ask_type_var_arena :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under type_var_arena
ask_type_var_arena = ReaderT $ \ (_, _, tvars, _) -> pure tvars
ask_sir_child_maps :: Applicative under => NRReader adt_arena var_arena type_var_arena sir_child_maps under sir_child_maps
ask_sir_child_maps = ReaderT $ \ (_, _, _, sir_child_maps) -> pure sir_child_maps
