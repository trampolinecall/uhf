module UHF.Phases.SolveTypes.Solver.Utils (apply_type, new_type_unknown) where -- TODO: better organization that does not export new_type_unknown

import UHF.Prelude

import UHF.Phases.SolveTypes.StateWithInferVars (new_type_unknown) -- TODO: figure out how to organize modules so that this import isnt necessary
import UHF.Phases.SolveTypes.Solver.TypeWithInferVar
import UHF.Phases.SolveTypes.Solver.Constraint
import UHF.Source.Span (Span)

-- TODO: find a better place to put this
apply_type :: Monad under => InferVarForWhat -> Span -> Type -> Type -> StateT InferVarArena under (Constraint, Type)
apply_type for_what sp ty arg =
    new_type_unknown for_what >>= \ tyu ->
    pure (UnkIsApplyResult sp tyu ty arg, Type'InferVar tyu)
