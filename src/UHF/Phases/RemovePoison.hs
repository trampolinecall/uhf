module UHF.Phases.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.BackendIR as BackendIR
import qualified UHF.Data.IR.Type as Type

type PoisonedBackendIR = BackendIR.BackendIR PoisonedType ()
type PoisonedType = Maybe (Type.Type Void)
type PoisonedADT = Type.ADT PoisonedType
type PoisonedTypeSynonym = Type.TypeSynonym PoisonedType
type PoisonedExpr = BackendIR.Expr PoisonedType ()
type PoisonedBinding = BackendIR.Binding PoisonedType ()
type PoisonedParam = BackendIR.Param PoisonedType

type NoPoisonBackendIR = BackendIR.BackendIR NoPoisonType Void
type NoPoisonType = Type.Type Void
type NoPoisonADT = Type.ADT NoPoisonType
type NoPoisonTypeSynonym = Type.TypeSynonym NoPoisonType
type NoPoisonExpr = BackendIR.Expr NoPoisonType Void
type NoPoisonBinding = BackendIR.Binding NoPoisonType Void
type NoPoisonParam = BackendIR.Param NoPoisonType

remove_poison :: PoisonedBackendIR -> Maybe (NoPoisonBackendIR)
remove_poison (BackendIR.BackendIR adts type_synonyms type_vars bindings params cu) =
    BackendIR.BackendIR
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params
        <*> pure cu

-- rp short for remove poison

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM rp_variant variants
    where
        rp_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> expansion

rp_binding :: PoisonedBinding -> Maybe (NoPoisonBinding)
rp_binding (BackendIR.Binding initializer) = BackendIR.Binding <$> rp_expr initializer

rp_expr :: PoisonedExpr -> Maybe (NoPoisonExpr)
rp_expr (BackendIR.Expr'Refer id ty b) = ty >>= \ ty -> pure (BackendIR.Expr'Refer id ty b)
rp_expr (BackendIR.Expr'Int id ty i) = ty >>= \ ty -> pure (BackendIR.Expr'Int id ty i)
rp_expr (BackendIR.Expr'Float id ty f) = ty >>= \ ty -> pure (BackendIR.Expr'Float id ty f)
rp_expr (BackendIR.Expr'Bool id ty b) = ty >>= \ ty -> pure (BackendIR.Expr'Bool id ty b)
rp_expr (BackendIR.Expr'Char id ty c) = ty >>= \ ty -> pure (BackendIR.Expr'Char id ty c)
rp_expr (BackendIR.Expr'String id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'String id ty t)
rp_expr (BackendIR.Expr'Tuple id ty a b) = ty >>= \ ty -> pure (BackendIR.Expr'Tuple id ty a b)

rp_expr (BackendIR.Expr'Lambda id ty a c g r) = ty >>= \ ty -> pure (BackendIR.Expr'Lambda id ty a c g r)
rp_expr (BackendIR.Expr'Param id ty p) = ty >>= \ ty -> pure (BackendIR.Expr'Param id ty p)

rp_expr (BackendIR.Expr'Call id ty c a) = ty >>= \ ty -> pure (BackendIR.Expr'Call id ty c a)

rp_expr (BackendIR.Expr'Switch id ty c a) = ty >>= \ ty -> pure (BackendIR.Expr'Switch id ty c a)

rp_expr (BackendIR.Expr'TupleDestructure1 id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'TupleDestructure1 id ty t)
rp_expr (BackendIR.Expr'TupleDestructure2 id ty t) = ty >>= \ ty -> pure (BackendIR.Expr'TupleDestructure2 id ty t)

rp_expr (BackendIR.Expr'Forall id ty vars group e) = ty >>= \ ty -> pure (BackendIR.Expr'Forall id ty vars group e)
rp_expr (BackendIR.Expr'TypeApply id ty e arg) = ty >>= \ ty -> arg >>= \ arg -> pure (BackendIR.Expr'TypeApply id ty e arg)

rp_expr (BackendIR.Expr'MakeADT id ty variant tyargs args) = ty >>= \ ty -> sequence tyargs >>= \ tyargs -> pure (BackendIR.Expr'MakeADT id ty variant tyargs args)

rp_expr (BackendIR.Expr'Poison _ _ _) = Nothing

rp_param :: PoisonedParam -> Maybe NoPoisonParam
rp_param (BackendIR.Param id ty) = BackendIR.Param id <$> ty
