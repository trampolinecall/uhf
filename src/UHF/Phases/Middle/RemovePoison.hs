module UHF.Phases.Middle.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type

type PoisonedANFIR = ANFIR.ANFIR PoisonedType ()
type PoisonedType = Maybe (Type.Type Void)
type PoisonedADT = Type.ADT PoisonedType
type PoisonedTypeSynonym = Type.TypeSynonym PoisonedType
type PoisonedExpr = ANFIR.Expr PoisonedType ()
type PoisonedBinding = ANFIR.Binding PoisonedType ()
type PoisonedParam = ANFIR.Param PoisonedType

type NoPoisonANFIR = ANFIR.ANFIR NoPoisonType Void
type NoPoisonType = Type.Type Void
type NoPoisonADT = Type.ADT NoPoisonType
type NoPoisonTypeSynonym = Type.TypeSynonym NoPoisonType
type NoPoisonExpr = ANFIR.Expr NoPoisonType Void
type NoPoisonBinding = ANFIR.Binding NoPoisonType Void
type NoPoisonParam = ANFIR.Param NoPoisonType

remove_poison :: PoisonedANFIR -> Maybe NoPoisonANFIR
remove_poison (ANFIR.ANFIR decls adts type_synonyms bindings params mod) =
    ANFIR.ANFIR decls
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params
        <*> pure mod

-- rp short for remove poison

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (Type.ADT id name variants) = Type.ADT id name <$> mapM rp_variant variants
    where
        rp_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (Type.TypeSynonym id name expansion) = Type.TypeSynonym id name <$> expansion

rp_binding :: PoisonedBinding -> Maybe NoPoisonBinding
rp_binding (ANFIR.Binding initializer) = ANFIR.Binding <$> rp_expr initializer

rp_expr :: PoisonedExpr -> Maybe NoPoisonExpr
rp_expr (ANFIR.Expr'Identifier id ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Identifier id ty b)
rp_expr (ANFIR.Expr'Int id ty i) = ty >>= \ ty -> pure (ANFIR.Expr'Int id ty i)
rp_expr (ANFIR.Expr'Float id ty f) = ty >>= \ ty -> pure (ANFIR.Expr'Float id ty f)
rp_expr (ANFIR.Expr'Bool id ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Bool id ty b)
rp_expr (ANFIR.Expr'Char id ty c) = ty >>= \ ty -> pure (ANFIR.Expr'Char id ty c)
rp_expr (ANFIR.Expr'String id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'String id ty t)
rp_expr (ANFIR.Expr'Tuple id ty a b) = ty >>= \ ty -> pure (ANFIR.Expr'Tuple id ty a b)

rp_expr (ANFIR.Expr'Lambda id ty c a i b) = ty >>= \ ty -> pure (ANFIR.Expr'Lambda id ty c a i b)
rp_expr (ANFIR.Expr'Param id ty p) = ty >>= \ ty -> pure (ANFIR.Expr'Param id ty p)

rp_expr (ANFIR.Expr'Call id ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Call id ty c a)

rp_expr (ANFIR.Expr'Switch id ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Switch id ty c a)

rp_expr (ANFIR.Expr'Seq id ty a b) = ty >>= \ ty -> pure (ANFIR.Expr'Seq id ty a b)

rp_expr (ANFIR.Expr'TupleDestructure1 id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure1 id ty t)
rp_expr (ANFIR.Expr'TupleDestructure2 id ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure2 id ty t)

rp_expr (ANFIR.Expr'Forall id ty vars e) = ty >>= \ ty -> pure (ANFIR.Expr'Forall id ty vars e)
rp_expr (ANFIR.Expr'TypeApply id ty e arg) = ty >>= \ ty -> arg >>= \ arg -> pure (ANFIR.Expr'TypeApply id ty e arg)

rp_expr (ANFIR.Expr'Poison _ _ _) = Nothing

rp_param :: PoisonedParam -> Maybe NoPoisonParam
rp_param (ANFIR.Param id ty) = ANFIR.Param id <$> ty
