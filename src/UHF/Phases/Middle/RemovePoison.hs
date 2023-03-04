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
-- TODO: probably dont pass DeclArena if it is not going to be changed
remove_poison (ANFIR.ANFIR decls adts type_synonyms bindings params) =
    ANFIR.ANFIR decls
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params

-- rp short for remove poison

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (Type.ADT name variants) = Type.ADT name <$> mapM rp_variant variants
    where
        rp_variant (Type.ADTVariant'Named name fields) = Type.ADTVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (Type.ADTVariant'Anon name fields) = Type.ADTVariant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (Type.TypeSynonym name expansion) = Type.TypeSynonym name <$> expansion

rp_binding :: PoisonedBinding -> Maybe NoPoisonBinding
rp_binding (ANFIR.Binding bound_where initializer) = ANFIR.Binding bound_where <$> rp_expr initializer

rp_expr :: PoisonedExpr -> Maybe NoPoisonExpr
rp_expr (ANFIR.Expr'Identifier ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Identifier ty b)
rp_expr (ANFIR.Expr'Int ty i) = ty >>= \ ty -> pure (ANFIR.Expr'Int ty i)
rp_expr (ANFIR.Expr'Float ty f) = ty >>= \ ty -> pure (ANFIR.Expr'Float ty f)
rp_expr (ANFIR.Expr'Bool ty b) = ty >>= \ ty -> pure (ANFIR.Expr'Bool ty b)
rp_expr (ANFIR.Expr'Char ty c) = ty >>= \ ty -> pure (ANFIR.Expr'Char ty c)
rp_expr (ANFIR.Expr'String ty t) = ty >>= \ ty -> pure (ANFIR.Expr'String ty t)
rp_expr (ANFIR.Expr'Tuple ty a b) = ty >>= \ ty -> pure (ANFIR.Expr'Tuple ty a b)

rp_expr (ANFIR.Expr'Lambda ty a i b) = ty >>= \ ty -> pure (ANFIR.Expr'Lambda ty a i b)
rp_expr (ANFIR.Expr'Param ty p) = ty >>= \ ty -> pure (ANFIR.Expr'Param ty p)

rp_expr (ANFIR.Expr'Call ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Call ty c a)

rp_expr (ANFIR.Expr'Switch ty c a) = ty >>= \ ty -> pure (ANFIR.Expr'Switch ty c a)

rp_expr (ANFIR.Expr'TupleDestructure1 ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure1 ty t)
rp_expr (ANFIR.Expr'TupleDestructure2 ty t) = ty >>= \ ty -> pure (ANFIR.Expr'TupleDestructure2 ty t)

rp_expr (ANFIR.Expr'Poison _ _) = Nothing

rp_param :: PoisonedParam -> Maybe NoPoisonParam
rp_param (ANFIR.Param ty) = ANFIR.Param <$> ty
