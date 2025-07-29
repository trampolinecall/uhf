module UHF.Parts.RemovePoison (remove_poison) where

import UHF.Prelude

import qualified UHF.Data.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Util.Arena as Arena

type PoisonedTopologicalSortStatus = Either BackendIR.HasLoops BackendIR.TopologicallySorted
type PoisonedBackendIR = BackendIR.BackendIR PoisonedTopologicalSortStatus PoisonedType ()
type PoisonedType = Maybe Type.Type
type PoisonedADT = Type.ADT PoisonedType
type PoisonedTypeSynonym = Type.TypeSynonym PoisonedType
type PoisonedExpr = BackendIR.Expr PoisonedTopologicalSortStatus PoisonedType ()
type PoisonedBinding = BackendIR.Binding PoisonedTopologicalSortStatus PoisonedType ()
type PoisonedParam = BackendIR.Param PoisonedType

type NoPoisonTopologicalSortStatus = BackendIR.TopologicallySorted
type NoPoisonBackendIR = BackendIR.BackendIR NoPoisonTopologicalSortStatus NoPoisonType Void
type NoPoisonType = Type.Type
type NoPoisonADT = Type.ADT NoPoisonType
type NoPoisonTypeSynonym = Type.TypeSynonym NoPoisonType
type NoPoisonExpr = BackendIR.Expr NoPoisonTopologicalSortStatus NoPoisonType Void
type NoPoisonBinding = BackendIR.Binding NoPoisonTopologicalSortStatus NoPoisonType Void
type NoPoisonParam = BackendIR.Param NoPoisonType

remove_poison :: PoisonedBackendIR -> Maybe NoPoisonBackendIR
remove_poison (BackendIR.BackendIR adts type_synonyms type_vars bindings params cu) =
    BackendIR.BackendIR
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params
        <*> rp_cu cu

-- rp short for remove poison

rp_cu :: BackendIR.CU PoisonedTopologicalSortStatus () -> Maybe (BackendIR.CU NoPoisonTopologicalSortStatus Void)
rp_cu (BackendIR.CU (Right main_function) bindings adts type_synonyms) = BackendIR.CU (Right main_function) <$> rp_group bindings <*> pure adts <*> pure type_synonyms
rp_cu (BackendIR.CU (Left ()) _ _ _) = Nothing

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (Type.ADT name type_vars variants) = Type.ADT name type_vars <$> mapM rp_variant variants
    where
        rp_variant (Type.ADT.Variant'Named name fields) = Type.ADT.Variant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (Type.ADT.Variant'Anon name fields) = Type.ADT.Variant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (Type.TypeSynonym name expansion) = Type.TypeSynonym name <$> expansion

rp_group :: BackendIR.BindingGroup PoisonedTopologicalSortStatus -> Maybe (BackendIR.BindingGroup NoPoisonTopologicalSortStatus)
rp_group (BackendIR.BindingGroup (Right BackendIR.TopologicallySorted) bindings) = Just (BackendIR.BindingGroup BackendIR.TopologicallySorted bindings)
rp_group (BackendIR.BindingGroup (Left BackendIR.HasLoops) _) = Nothing

rp_binding :: PoisonedBinding -> Maybe NoPoisonBinding
rp_binding (BackendIR.Binding initializer) = BackendIR.Binding <$> rp_expr initializer

rp_expr :: PoisonedExpr -> Maybe NoPoisonExpr
rp_expr (BackendIR.Expr'Refer id ty b) = ty >>= \ ty -> pure (BackendIR.Expr'Refer id ty b)
rp_expr (BackendIR.Expr'Intrinsic id ty i) = ty >>= \ ty -> pure (BackendIR.Expr'Intrinsic id ty i)
rp_expr (BackendIR.Expr'Int id ty i) = ty >>= \ ty -> pure (BackendIR.Expr'Int id ty i)
rp_expr (BackendIR.Expr'Float id ty f) = ty >>= \ ty -> pure (BackendIR.Expr'Float id ty f)
rp_expr (BackendIR.Expr'Bool id ty b) = ty >>= \ ty -> pure (BackendIR.Expr'Bool id ty b)
rp_expr (BackendIR.Expr'Char id ty c) = ty >>= \ ty -> pure (BackendIR.Expr'Char id ty c)
rp_expr (BackendIR.Expr'String id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'String id ty t)
rp_expr (BackendIR.Expr'Tuple id ty a b) = ty >>= \ ty -> pure (BackendIR.Expr'Tuple id ty a b)

rp_expr (BackendIR.Expr'Lambda id ty a c g r) = ty >>= \ ty -> rp_group g >>= \ g -> pure (BackendIR.Expr'Lambda id ty a c g r)
rp_expr (BackendIR.Expr'Param id ty p) = ty >>= \ ty -> pure (BackendIR.Expr'Param id ty p)

rp_expr (BackendIR.Expr'Call id ty c a) = ty >>= \ ty -> pure (BackendIR.Expr'Call id ty c a)

rp_expr (BackendIR.Expr'Match id ty t) = ty >>= \ ty -> rp_tree t >>= \ t -> pure (BackendIR.Expr'Match id ty t)
    where
        rp_tree (BackendIR.MatchTree arms) =
            BackendIR.MatchTree
                <$> mapM
                    (\ (clauses, result) ->
                        (,)
                            <$> mapM rp_clause clauses
                            <*> case result of
                                    Right (group, e) -> rp_group group >>= \ group -> Just (Right (group, e))
                                    Left subtree -> Left <$> rp_tree subtree
                    )
                    arms

        rp_clause (BackendIR.MatchClause'Match bk matcher) = BackendIR.MatchClause'Match bk <$> rp_matcher matcher
        rp_clause (BackendIR.MatchClause'Binding bk) = Just $ BackendIR.MatchClause'Binding bk

        rp_matcher (BackendIR.Match'BoolLiteral b) = Just $ BackendIR.Match'BoolLiteral b
        rp_matcher BackendIR.Match'Tuple = Just BackendIR.Match'Tuple
        rp_matcher (BackendIR.Match'AnonADTVariant (Left ())) = Nothing
        rp_matcher (BackendIR.Match'AnonADTVariant (Right variant)) = Just $ BackendIR.Match'AnonADTVariant (Right variant)

rp_expr (BackendIR.Expr'TupleDestructure1 id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'TupleDestructure1 id ty t)
rp_expr (BackendIR.Expr'TupleDestructure2 id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'TupleDestructure2 id ty t)
rp_expr (BackendIR.Expr'ADTDestructure id ty b (Right field)) = ty >>= \ ty -> pure (BackendIR.Expr'ADTDestructure id ty b (Right field))
rp_expr (BackendIR.Expr'ADTDestructure _ _ _ (Left ())) = Nothing

rp_expr (BackendIR.Expr'Forall id ty vars group e) = ty >>= \ ty -> rp_group group >>= \ group -> pure (BackendIR.Expr'Forall id ty vars group e)
rp_expr (BackendIR.Expr'TypeApply id ty e arg) = ty >>= \ ty -> arg >>= \ arg -> pure (BackendIR.Expr'TypeApply id ty e arg)

rp_expr (BackendIR.Expr'MakeADT id ty variant tyargs args) = ty >>= \ ty -> sequence tyargs >>= \ tyargs -> pure (BackendIR.Expr'MakeADT id ty variant tyargs args)

rp_expr (BackendIR.Expr'Poison _ _ _) = Nothing

rp_param :: PoisonedParam -> Maybe NoPoisonParam
rp_param (BackendIR.Param id ty) = BackendIR.Param id <$> ty
