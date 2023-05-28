module UHF.Phases.Middle.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type

type PoisonedANFIR captures = ANFIR.ANFIR captures PoisonedType ()
type PoisonedType = Maybe (Type.Type Void)
type PoisonedADT = Type.ADT PoisonedType
type PoisonedTypeSynonym = Type.TypeSynonym PoisonedType
type PoisonedExpr captures = ANFIR.Expr captures PoisonedType ()
type PoisonedBinding captures = ANFIR.Binding captures PoisonedType ()
type PoisonedParam = ANFIR.Param PoisonedType

type NoPoisonANFIR captures = ANFIR.ANFIR captures NoPoisonType Void
type NoPoisonType = Type.Type Void
type NoPoisonADT = Type.ADT NoPoisonType
type NoPoisonTypeSynonym = Type.TypeSynonym NoPoisonType
type NoPoisonExpr captures = ANFIR.Expr captures NoPoisonType Void
type NoPoisonBinding captures = ANFIR.Binding captures NoPoisonType Void
type NoPoisonParam = ANFIR.Param NoPoisonType

remove_poison :: PoisonedANFIR captures -> Maybe (NoPoisonANFIR captures)
remove_poison (ANFIR.ANFIR decls adts type_synonyms type_vars bindings params mod) =
    ANFIR.ANFIR decls
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params
        <*> pure mod

-- rp short for remove poison

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM rp_variant variants
    where
        rp_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> expansion

rp_binding :: PoisonedBinding captures -> Maybe (NoPoisonBinding captures)
rp_binding (ANFIR.Binding bound_where initializer) = ANFIR.Binding bound_where <$> rp_expr initializer

rp_expr :: PoisonedExpr captures -> Maybe (NoPoisonExpr captures)
rp_expr (ANFIR.Expr'Refer id ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Refer id ty b)
rp_expr (ANFIR.Expr'Int id ty i) = ty >>= \ ty -> pure (ANFIR.Expr'Int id ty i)
rp_expr (ANFIR.Expr'Float id ty f) = ty >>= \ ty -> pure (ANFIR.Expr'Float id ty f)
rp_expr (ANFIR.Expr'Bool id ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Bool id ty b)
rp_expr (ANFIR.Expr'Char id ty c) = ty >>= \ ty -> pure (ANFIR.Expr'Char id ty c)
rp_expr (ANFIR.Expr'String id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'String id ty t)
rp_expr (ANFIR.Expr'Tuple id ty a b) = ty >>= \ ty -> pure (ANFIR.Expr'Tuple id ty a b)

rp_expr (ANFIR.Expr'Lambda id ty a g r) = ty >>= \ ty -> pure (ANFIR.Expr'Lambda id ty a g r)
rp_expr (ANFIR.Expr'Param id ty p) = ty >>= \ ty -> pure (ANFIR.Expr'Param id ty p)

rp_expr (ANFIR.Expr'Call id ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Call id ty c a)

rp_expr (ANFIR.Expr'Switch id ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Switch id ty c a)

rp_expr (ANFIR.Expr'Seq id ty a b) = ty >>= \ ty -> pure (ANFIR.Expr'Seq id ty a b)

rp_expr (ANFIR.Expr'TupleDestructure1 id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure1 id ty t)
rp_expr (ANFIR.Expr'TupleDestructure2 id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure2 id ty t)

rp_expr (ANFIR.Expr'Forall id ty vars group e) = ty >>= \ ty -> pure (ANFIR.Expr'Forall id ty vars group e)
rp_expr (ANFIR.Expr'TypeApply id ty e arg) = ty >>= \ ty -> arg >>= \ arg -> pure (ANFIR.Expr'TypeApply id ty e arg)

rp_expr (ANFIR.Expr'MakeADT id ty variant args) = ty >>= \ ty -> pure (ANFIR.Expr'MakeADT id ty variant args)

rp_expr (ANFIR.Expr'Poison _ _ _) = Nothing

rp_param :: PoisonedParam -> Maybe NoPoisonParam
rp_param (ANFIR.Param id ty) = ANFIR.Param id <$> ty
