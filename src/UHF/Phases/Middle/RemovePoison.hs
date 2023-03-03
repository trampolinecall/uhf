module UHF.Phases.Middle.RemovePoison (remove_poison) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Data.IR.HIR as HIR
import qualified UHF.Data.IR.ANFIR as ANFIR
import qualified UHF.Data.IR.Type as Type
import UHF.Data.IR.Keys

type Decl = ANFIR.Decl
type DeclArena = Arena.Arena Decl DeclKey

type PoisonedType = Maybe (Type.Type Void)
type PoisonedADT = HIR.ADT PoisonedType
type PoisonedTypeSynonym = HIR.TypeSynonym PoisonedType
type PoisonedExpr = ANFIR.Expr PoisonedType ()
type PoisonedBinding = ANFIR.Binding PoisonedType ()
type PoisonedParam = ANFIR.Param PoisonedType

type PoisonedADTArena = Arena.Arena PoisonedADT ADTKey
type PoisonedTypeSynonymArena = Arena.Arena PoisonedTypeSynonym HIR.TypeSynonymKey
type PoisonedBindingArena = Arena.Arena PoisonedBinding ANFIR.BindingKey
type PoisonedParamArena = Arena.Arena PoisonedParam ANFIR.ParamKey

type NoPoisonType = Type.Type Void
type NoPoisonADT = HIR.ADT NoPoisonType
type NoPoisonTypeSynonym = HIR.TypeSynonym NoPoisonType
type NoPoisonExpr = ANFIR.Expr NoPoisonType Void
type NoPoisonBinding = ANFIR.Binding NoPoisonType Void
type NoPoisonParam = ANFIR.Param NoPoisonType

type NoPoisonADTArena = Arena.Arena NoPoisonADT ADTKey
type NoPoisonTypeSynonymArena = Arena.Arena NoPoisonTypeSynonym HIR.TypeSynonymKey
type NoPoisonBindingArena = Arena.Arena NoPoisonBinding ANFIR.BindingKey
type NoPoisonParamArena = Arena.Arena NoPoisonParam ANFIR.ParamKey
remove_poison :: (DeclArena, PoisonedADTArena, PoisonedTypeSynonymArena, PoisonedBindingArena, PoisonedParamArena) -> Maybe (DeclArena, NoPoisonADTArena, NoPoisonTypeSynonymArena, NoPoisonBindingArena, NoPoisonParamArena)
-- TODO: probably dont pass DeclArena if it is not going to be changed
remove_poison (decls, adts, type_synonyms, bindings, params) =
    (decls,,,,)
        <$> Arena.transformM rp_adt adts
        <*> Arena.transformM rp_type_synonym type_synonyms
        <*> Arena.transformM rp_binding bindings
        <*> Arena.transformM rp_param params

-- rp short for remove poison

rp_adt :: PoisonedADT -> Maybe NoPoisonADT
rp_adt (HIR.ADT name variants) = HIR.ADT name <$> mapM rp_variant variants
    where
        rp_variant (HIR.ADTVariant'Named name fields) = HIR.ADTVariant'Named name <$> mapM (\ (field_name, field_ty) -> (field_name,) <$> field_ty) fields
        rp_variant (HIR.ADTVariant'Anon name fields) = HIR.ADTVariant'Anon name <$> sequence fields

rp_type_synonym :: PoisonedTypeSynonym -> Maybe NoPoisonTypeSynonym
rp_type_synonym (HIR.TypeSynonym name expansion) = HIR.TypeSynonym name <$> expansion

rp_binding :: PoisonedBinding -> Maybe NoPoisonBinding
rp_binding (ANFIR.Binding bound_where ty initializer) = ANFIR.Binding bound_where <$> ty <*> rp_expr initializer

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
