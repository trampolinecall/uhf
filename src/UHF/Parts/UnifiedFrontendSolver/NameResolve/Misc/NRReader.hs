module UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NRReader
    ( NRReader

    , ask_adt_arena -- TODO: remove
    , ask_type_synonym_arena -- TODO: remove?
    , ask_var_arena -- TODO: remove?
    , ask_quant_var_arena -- TODO: remove?
    , ask_sir_child_maps -- TODO: remove?
    ) where

-- TODO: remove this

import UHF.Prelude

type NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps = ReaderT (adt_arena, type_synonym_arena, var_arena, quant_var_arena, sir_child_maps)

ask_adt_arena :: Applicative under => NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps under adt_arena
ask_adt_arena = ReaderT $ \ (adts, _, _, _, _) -> pure adts
ask_type_synonym_arena :: Applicative under => NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps under type_synonym_arena
ask_type_synonym_arena = ReaderT $ \ (_, type_synonyms, _, _, _) -> pure type_synonyms
ask_var_arena :: Applicative under => NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps under var_arena
ask_var_arena = ReaderT $ \ (_, _, vars, _, _) -> pure vars
ask_quant_var_arena :: Applicative under => NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps under quant_var_arena
ask_quant_var_arena = ReaderT $ \ (_, _, _, tvars, _) -> pure tvars
ask_sir_child_maps :: Applicative under => NRReader adt_arena type_synonym_arena var_arena quant_var_arena sir_child_maps under sir_child_maps
ask_sir_child_maps = ReaderT $ \ (_, _, _, _, sir_child_maps) -> pure sir_child_maps
