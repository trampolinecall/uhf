{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{- HLINT ignore "Functor law" -}

module UHF.Data.SIR.Visitor
    ( TransformsNameMapIndex (..)
    , TransformsIdenResolvedKey (..)
    , TransformsTypeInRefer (..)
    , TransformsTypeExprEvaledKey (..)
    , TransformsTypeExprEvaledAsTypeKey (..)
    , TransformsTypeInfo (..)
    , TransformsInfixGroupedKey (..)

    , SIRVisitor (..)
    , visit_sir'
    , CUVisitor (..)
    , visit_cu'
    , ADTVisitor (..)
    , visit_adt_variant
    , visit_adt'
    , visit_adt_variant'
    , TypeSynonymVisitor (..)
    , visit_type_synonym'
    , ModuleVisitor (..)
    , visit_module'
    , VariableVisitor (..)
    , visit_variable'
    , BindingVisitor (..)
    , visit_binding'
    , TypeExprVisitor (..)
    , visit_type_expr
    , visit_type_expr'
    , ExprIdentifierRefVisitor (..)
    , visit_expr_identifier_ref
    , visit_expr_identifier_ref'
    , OperatorRefVisitor (..)
    , visit_operator_ref
    , visit_operator_ref'
    , ExprVisitor (..)
    , visit_expr
    , visit_expr'
    , PatternADTVariantRefVisitor (..)
    , visit_pattern_adt_variant_ref
    , visit_pattern_adt_variant_ref'
    , PatternVisitor (..)
    , visit_pattern
    , visit_pattern'
    ) where

import UHF.Prelude

import Data.Data (Proxy (..))
import qualified UHF.Data.IR.ID as ID
import UHF.Data.IR.Keys (ADTKey, QuantVarKey)
import UHF.Data.IR.Type (TypeSynonymKey)
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import UHF.Data.SIR (ADT, Binding (..), CU (..), DeclRef (..), Expr (..), ExprIdentifierRef, HoleIdentifier, IdenResolvedKey, InfixGroupedKey, Module (..), NameMapIndex, OperatorRef, Pattern (..), PatternADTVariantRef, SIR (..), SplitIdentifier (..), TypeExpr (..), TypeExprEvaledAsTypeKey, TypeExprEvaledKey, TypeInRefer, TypeInfo, TypeSynonym, ValueRef, Variable (..), VariableKey)
import UHF.Source.Located (Located)
import UHF.Source.Span (Span)
import qualified UHF.Util.Arena as Arena

class Monad m => TransformsNameMapIndex stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_name_map_index :: Proxy visitor -> cx -> NameMapIndex stage1 -> m (NameMapIndex stage2)
    default transform_name_map_index :: NameMapIndex stage1 ~ NameMapIndex stage2 => Proxy visitor -> cx -> NameMapIndex stage1 -> m (NameMapIndex stage2)
    transform_name_map_index _ _ = pure
class Monad m => TransformsIdenResolvedKey stage1 stage2 rs1 rs2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_iden_resolved_key :: Proxy visitor -> cx -> IdenResolvedKey stage1 rs1 -> m (IdenResolvedKey stage2 rs2)
    default transform_iden_resolved_key :: (IdenResolvedKey stage1 rs1 ~ IdenResolvedKey stage2 rs2) => Proxy visitor -> cx -> IdenResolvedKey stage1 rs1 -> m (IdenResolvedKey stage2 rs2)
    transform_iden_resolved_key _ _ = pure
class Monad m => TransformsTypeInRefer stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_type_in_refer :: Proxy visitor -> cx -> TypeInRefer stage1 -> m (TypeInRefer stage2)
    default transform_type_in_refer :: TypeInRefer stage1 ~ TypeInRefer stage2 => Proxy visitor -> cx -> TypeInRefer stage1 -> m (TypeInRefer stage2)
    transform_type_in_refer _ _ = pure
class Monad m => TransformsTypeExprEvaledKey stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_type_expr_evaled_key :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> m (TypeExprEvaledKey stage2)
    default transform_type_expr_evaled_key :: TypeExprEvaledKey stage1 ~ TypeExprEvaledKey stage2 => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> m (TypeExprEvaledKey stage2)
    transform_type_expr_evaled_key _ _ = pure
class Monad m => TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_type_expr_evaled_as_type_key :: Proxy visitor -> cx -> TypeExprEvaledAsTypeKey stage1 -> m (TypeExprEvaledAsTypeKey stage2)
    default transform_type_expr_evaled_as_type_key :: TypeExprEvaledAsTypeKey stage1 ~ TypeExprEvaledAsTypeKey stage2 => Proxy visitor -> cx -> TypeExprEvaledAsTypeKey stage1 -> m (TypeExprEvaledAsTypeKey stage2)
    transform_type_expr_evaled_as_type_key _ _ = pure
class Monad m => TransformsTypeInfo stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_type_info :: Proxy visitor -> cx -> TypeInfo stage1 -> m (TypeInfo stage2)
    default transform_type_info :: TypeInfo stage1 ~ TypeInfo stage2 => Proxy visitor -> cx -> TypeInfo stage1 -> m (TypeInfo stage2)
    transform_type_info _ _ = pure
class Monad m => TransformsInfixGroupedKey stage1 stage2 cx m visitor | visitor -> stage1 stage2 cx m where
    transform_infix_grouped_key :: Proxy visitor -> cx -> InfixGroupedKey stage1 -> m (InfixGroupedKey stage2)
    default transform_infix_grouped_key :: InfixGroupedKey stage1 ~ InfixGroupedKey stage2 => Proxy visitor -> cx -> InfixGroupedKey stage1 -> m (InfixGroupedKey stage2)
    transform_infix_grouped_key _ _ = pure

class Monad m => SIRVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_sir :: Proxy visitor -> cx -> SIR stage1 -> m (SIR stage2, res)
    default visit_sir :: (CUVisitor stage1 stage2 cx m () visitor, ADTVisitor stage1 stage2 cx m () visitor, TypeSynonymVisitor stage1 stage2 cx m () visitor, ModuleVisitor stage1 stage2 cx m () visitor, VariableVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> SIR stage1 -> m (SIR stage2, res)
    visit_sir proxy cx (SIR modules adts type_synonyms quant_vars variables cu) = do
        modules <- Arena.transformM (fmap fst . visit_module proxy cx) modules
        adts <- Arena.transformM (fmap fst . visit_adt proxy cx) adts
        type_synonyms <- Arena.transformM (fmap fst . visit_type_synonym proxy cx) type_synonyms
        variables <- Arena.transformM (fmap fst . visit_variable proxy cx) variables
        (cu, ()) <- visit_cu proxy cx cu
        pure (SIR modules adts type_synonyms quant_vars variables cu, ())

visit_sir' :: SIRVisitor stage1 stage2 () m () visitor => Proxy visitor -> SIR stage1 -> m (SIR stage2)
visit_sir' proxy = fmap fst . visit_sir proxy ()

class Monad m => CUVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_cu :: Proxy visitor -> cx -> CU stage1 -> m (CU stage2, res)
    default visit_cu :: (res ~ ()) => Proxy visitor -> cx -> CU stage1 -> m (CU stage2, res)
    visit_cu _ _ (CU root_module main_function) = pure (CU root_module main_function, ())

visit_cu' :: CUVisitor stage1 stage2 () m () visitor => Proxy visitor -> CU stage1 -> m (CU stage2)
visit_cu' proxy = fmap fst . visit_cu proxy ()

class Monad m => ADTVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_adt :: Proxy visitor -> cx -> ADT stage1 -> m (ADT stage2, res)
    default visit_adt :: (res ~ ()) => Proxy visitor -> cx -> ADT stage1 -> m (ADT stage2, res)
    visit_adt proxy cx (Type.ADT id name qvks variants) = do
        variants <- mapM (fmap fst . visit_adt_variant proxy cx) variants
        pure (Type.ADT id name qvks variants, ())

    visit_adt_variant_named :: Proxy visitor -> cx -> Located Text -> ID.ADTVariantID -> [(ID.ADTFieldID, Text, (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1))] -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2), res)
    default visit_adt_variant_named :: (TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> Located Text -> ID.ADTVariantID -> [(ID.ADTFieldID, Text, (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1))] -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2), res)
    visit_adt_variant_named proxy cx name id fields = do
        fields <- mapM (\ (field_id, field_name, (field_tye, field_tyeastk)) -> (field_id, field_name,) <$> ((,) <$> (fst <$> visit_type_expr proxy cx field_tye) <*> transform_type_expr_evaled_as_type_key proxy cx field_tyeastk)) fields
        pure (Type.ADT.Variant'Named name id fields, ())

    visit_adt_variant_anon :: Proxy visitor -> cx -> Located Text -> ID.ADTVariantID -> [(ID.ADTFieldID, (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1))] -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2), res)
    default visit_adt_variant_anon :: (TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> Located Text -> ID.ADTVariantID -> [(ID.ADTFieldID, (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1))] -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2), res)
    visit_adt_variant_anon proxy cx name id fields = do
        fields <- mapM (\ (field_id, (field_tye, field_tyeastk)) -> (field_id,) <$> ((,) <$> (fst <$> visit_type_expr proxy cx field_tye) <*> transform_type_expr_evaled_as_type_key proxy cx field_tyeastk)) fields
        pure (Type.ADT.Variant'Anon name id fields, ())

visit_adt_variant :: ADTVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> Type.ADT.Variant (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2), res)
visit_adt_variant proxy cx (Type.ADT.Variant'Named name id fields) = visit_adt_variant_named proxy cx name id fields
visit_adt_variant proxy cx (Type.ADT.Variant'Anon name id fields) = visit_adt_variant_anon proxy cx name id fields

visit_adt' :: ADTVisitor stage1 stage2 () m () visitor => Proxy visitor -> ADT stage1 -> m (ADT stage2)
visit_adt' proxy = fmap fst . visit_adt proxy ()

visit_adt_variant' :: ADTVisitor stage1 stage2 () m () visitor => Proxy visitor -> Type.ADT.Variant (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> m (Type.ADT.Variant (TypeExpr stage2, TypeExprEvaledAsTypeKey stage2))
visit_adt_variant' proxy = fmap fst . visit_adt_variant proxy ()

class Monad m => TypeSynonymVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_type_synonym :: Proxy visitor -> cx -> TypeSynonym stage1 -> m (TypeSynonym stage2, res)
    default visit_type_synonym :: (TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeSynonym stage1 -> m (TypeSynonym stage2, res)
    visit_type_synonym proxy cx (Type.TypeSynonym id name (tye, tyeastk)) = do
        (tye, ()) <- visit_type_expr proxy cx tye
        tyeastk <- transform_type_expr_evaled_as_type_key proxy cx tyeastk
        pure (Type.TypeSynonym id name (tye, tyeastk), ())

visit_type_synonym' :: TypeSynonymVisitor stage1 stage2 () m () visitor => Proxy visitor -> TypeSynonym stage1 -> m (TypeSynonym stage2)
visit_type_synonym' proxy = fmap fst . visit_type_synonym proxy ()

class Monad m => ModuleVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_module :: Proxy visitor -> cx -> Module stage1 -> m (Module stage2, res)
    default visit_module :: (TransformsNameMapIndex stage1 stage2 cx m visitor, BindingVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> Module stage1 -> m (Module stage2, res)
    visit_module proxy cx (Module id name_map_index bindings adts type_synonyms) = do
        name_map_index <- transform_name_map_index proxy cx name_map_index
        bindings <- mapM (fmap fst . visit_binding proxy cx) bindings
        pure (Module id name_map_index bindings adts type_synonyms, ())

visit_module' :: ModuleVisitor stage1 stage2 () m () visitor => Proxy visitor -> Module stage1 -> m (Module stage2)
visit_module' proxy = fmap fst . visit_module proxy ()

class Monad m => VariableVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_variable :: Proxy visitor -> cx -> Variable stage1 -> m (Variable stage2, res)
    default visit_variable :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> Variable stage1 -> m (Variable stage2, res)
    visit_variable proxy cx (Variable id type_info name) = do
        type_info <- transform_type_info proxy cx type_info
        pure (Variable id type_info name, ())

visit_variable' :: VariableVisitor stage1 stage2 () m () visitor => Proxy visitor -> Variable stage1 -> m (Variable stage2)
visit_variable' proxy = fmap fst . visit_variable proxy ()

class Monad m => BindingVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_binding :: Proxy visitor -> cx -> Binding stage1 -> m (Binding stage2, res)
    default visit_binding :: (ExprVisitor stage1 stage2 cx m () visitor, PatternVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> Binding stage1 -> m (Binding stage2, res)
    visit_binding proxy cx (Binding lhs eq_sp rhs) = do
        (lhs, ()) <- visit_pattern proxy cx lhs
        (rhs, ()) <- visit_expr proxy cx rhs
        pure (Binding lhs eq_sp rhs, ())

visit_binding' :: BindingVisitor stage1 stage2 () m () visitor => Proxy visitor -> Binding stage1 -> m (Binding stage2)
visit_binding' proxy = fmap fst . visit_binding proxy ()

class Monad m => TypeExprVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_type_expr_refer :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> IdenResolvedKey stage1 (DeclRef (TypeInRefer stage1)) -> Span -> NameMapIndex stage1 -> Located Text -> m (TypeExpr stage2, res)
    default visit_type_expr_refer :: (TransformsNameMapIndex stage1 stage2 cx m visitor, TransformsIdenResolvedKey stage1 stage2 (DeclRef (TypeInRefer stage1)) (DeclRef (TypeInRefer stage2)) cx m visitor, TransformsTypeInRefer stage1 stage2 cx m visitor, TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> IdenResolvedKey stage1 (DeclRef (TypeInRefer stage1)) -> Span -> NameMapIndex stage1 -> Located Text -> m (TypeExpr stage2, res)
    visit_type_expr_refer proxy cx evaled resolved sp name_maps id = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        resolved <- transform_iden_resolved_key proxy cx resolved
        name_maps <- transform_name_map_index proxy cx name_maps
        pure (TypeExpr'Refer evaled resolved sp name_maps id, ())

    visit_type_expr_get :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> IdenResolvedKey stage1 (DeclRef (TypeInRefer stage1)) -> Span -> TypeExpr stage1 -> Located Text -> m (TypeExpr stage2, res)
    default visit_type_expr_get :: (TransformsIdenResolvedKey stage1 stage2 (DeclRef (TypeInRefer stage1)) (DeclRef (TypeInRefer stage2)) cx m visitor, TransformsTypeInRefer stage1 stage2 cx m visitor, TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> IdenResolvedKey stage1 (DeclRef (TypeInRefer stage1)) -> Span -> TypeExpr stage1 -> Located Text -> m (TypeExpr stage2, res)
    visit_type_expr_get proxy cx evaled resolved sp parent name = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        resolved <- transform_iden_resolved_key proxy cx resolved
        (parent, ()) <- visit_type_expr proxy cx parent
        pure (TypeExpr'Get evaled resolved sp parent name, ())

    visit_type_expr_tuple :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    default visit_type_expr_tuple :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    visit_type_expr_tuple proxy cx evaled sp a b = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        (a, ()) <- visit_type_expr proxy cx a
        (b, ()) <- visit_type_expr proxy cx b
        pure (TypeExpr'Tuple evaled sp a b, ())

    visit_type_expr_hole :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> TypeExprEvaledAsTypeKey stage1 -> Span -> HoleIdentifier -> m (TypeExpr stage2, res)
    default visit_type_expr_hole :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> TypeExprEvaledAsTypeKey stage1 -> Span -> HoleIdentifier -> m (TypeExpr stage2, res)
    visit_type_expr_hole proxy cx evaled evaled_as_type sp hid = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        evaled_as_type <- transform_type_expr_evaled_as_type_key proxy cx evaled_as_type
        pure (TypeExpr'Hole evaled evaled_as_type sp hid, ())

    visit_type_expr_function :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    default visit_type_expr_function :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    visit_type_expr_function proxy cx evaled sp arg res = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        (arg, ()) <- visit_type_expr proxy cx arg
        (res, ()) <- visit_type_expr proxy cx res
        pure (TypeExpr'Function evaled sp arg res, ())

    visit_type_expr_forall :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> NameMapIndex stage1 -> NonEmpty QuantVarKey -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    default visit_type_expr_forall :: (TransformsNameMapIndex stage1 stage2 cx m visitor, TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> NameMapIndex stage1 -> NonEmpty QuantVarKey -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    visit_type_expr_forall proxy cx evaled sp name_maps vars ty = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        name_maps <- transform_name_map_index proxy cx name_maps
        (ty, ()) <- visit_type_expr proxy cx ty
        pure (TypeExpr'Forall evaled sp name_maps vars ty, ())

    visit_type_expr_apply :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    default visit_type_expr_apply :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> TypeExpr stage1 -> TypeExpr stage1 -> m (TypeExpr stage2, res)
    visit_type_expr_apply proxy cx evaled sp ty arg = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        (ty, ()) <- visit_type_expr proxy cx ty
        (arg, ()) <- visit_type_expr proxy cx arg
        pure (TypeExpr'Apply evaled sp ty arg, ())

    visit_type_expr_wild :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> m (TypeExpr stage2, res)
    default visit_type_expr_wild :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> m (TypeExpr stage2, res)
    visit_type_expr_wild proxy cx evaled sp = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        pure (TypeExpr'Wild evaled sp, ())

    visit_type_expr_poison :: Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> m (TypeExpr stage2, res)
    default visit_type_expr_poison :: (TransformsTypeExprEvaledKey stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeExprEvaledKey stage1 -> Span -> m (TypeExpr stage2, res)
    visit_type_expr_poison proxy cx evaled sp = do
        evaled <- transform_type_expr_evaled_key proxy cx evaled
        pure (TypeExpr'Poison evaled sp, ())

visit_type_expr :: TypeExprVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> TypeExpr stage1 -> m (TypeExpr stage2, res)
visit_type_expr proxy cx (TypeExpr'Refer evaled resolved sp name_maps id) = visit_type_expr_refer proxy cx evaled resolved sp name_maps id
visit_type_expr proxy cx (TypeExpr'Get evaled resolved sp parent name) = visit_type_expr_get proxy cx evaled resolved sp parent name
visit_type_expr proxy cx (TypeExpr'Tuple evaled sp a b) = visit_type_expr_tuple proxy cx evaled sp a b
visit_type_expr proxy cx (TypeExpr'Hole evaled evaled_as_type sp hid) = visit_type_expr_hole proxy cx evaled evaled_as_type sp hid
visit_type_expr proxy cx (TypeExpr'Function evaled sp arg res) = visit_type_expr_function proxy cx evaled sp arg res
visit_type_expr proxy cx (TypeExpr'Forall evaled sp name_maps vars ty) = visit_type_expr_forall proxy cx evaled sp name_maps vars ty
visit_type_expr proxy cx (TypeExpr'Apply evaled sp ty arg) = visit_type_expr_apply proxy cx evaled sp ty arg
visit_type_expr proxy cx (TypeExpr'Wild evaled sp) = visit_type_expr_wild proxy cx evaled sp
visit_type_expr proxy cx (TypeExpr'Poison evaled sp) = visit_type_expr_poison proxy cx evaled sp

visit_type_expr' :: TypeExprVisitor stage1 stage2 () m () visitor => Proxy visitor -> TypeExpr stage1 -> m (TypeExpr stage2)
visit_type_expr' proxy = fmap fst . visit_type_expr proxy ()

class Monad m => ExprIdentifierRefVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_expr_identifier_ref_get :: Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (ExprIdentifierRef stage2, res)
    default visit_expr_identifier_ref_get :: (TransformsIdenResolvedKey stage1 stage2 ValueRef ValueRef cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (ExprIdentifierRef stage2, res)
    visit_expr_identifier_ref_get proxy cx t n r = do
        (t, ()) <- visit_type_expr proxy cx t
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Get t n r, ())

    visit_expr_identifier_ref_single :: Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (ExprIdentifierRef stage2, res)
    default visit_expr_identifier_ref_single :: (TransformsNameMapIndex stage1 stage2 cx m visitor, TransformsIdenResolvedKey stage1 stage2 ValueRef ValueRef cx m visitor, res ~ ()) => Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (ExprIdentifierRef stage2, res)
    visit_expr_identifier_ref_single proxy cx nm n r = do
        nm <- transform_name_map_index proxy cx nm
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Single nm n r, ())

visit_expr_identifier_ref :: ExprIdentifierRefVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> ExprIdentifierRef stage1 -> m (ExprIdentifierRef stage2, res)
visit_expr_identifier_ref proxy cx (SplitIdentifier'Get t n r)= visit_expr_identifier_ref_get proxy cx t n r
visit_expr_identifier_ref proxy cx (SplitIdentifier'Single nm n r) = visit_expr_identifier_ref_single proxy cx nm n r

visit_expr_identifier_ref' :: ExprIdentifierRefVisitor stage1 stage2 () m () visitor => Proxy visitor -> ExprIdentifierRef stage1 -> m (ExprIdentifierRef stage2)
visit_expr_identifier_ref' proxy = fmap fst . visit_expr_identifier_ref proxy ()

class Monad m => OperatorRefVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_operator_ref_get :: Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (OperatorRef stage2, res)
    default visit_operator_ref_get :: (TransformsIdenResolvedKey stage1 stage2 ValueRef ValueRef cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (OperatorRef stage2, res)
    visit_operator_ref_get proxy cx t n r = do
        (t, ()) <- visit_type_expr proxy cx t
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Get t n r, ())

    visit_operator_ref_single :: Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (OperatorRef stage2, res)
    default visit_operator_ref_single :: (TransformsNameMapIndex stage1 stage2 cx m visitor, TransformsIdenResolvedKey stage1 stage2 ValueRef ValueRef cx m visitor, res ~ ()) => Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 ValueRef -> m (OperatorRef stage2, res)
    visit_operator_ref_single proxy cx nm n r = do
        nm <- transform_name_map_index proxy cx nm
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Single nm n r, ())

visit_operator_ref :: OperatorRefVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> OperatorRef stage1 -> m (OperatorRef stage2, res)
visit_operator_ref proxy cx (SplitIdentifier'Get t n r)= visit_operator_ref_get proxy cx t n r
visit_operator_ref proxy cx (SplitIdentifier'Single nm n r) = visit_operator_ref_single proxy cx nm n r

visit_operator_ref' :: OperatorRefVisitor stage1 stage2 () m () visitor => Proxy visitor -> OperatorRef stage1 -> m (OperatorRef stage2)
visit_operator_ref' proxy = fmap fst . visit_operator_ref proxy ()

class Monad m => ExprVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_expr_refer :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> ExprIdentifierRef stage1 -> m (Expr stage2, res)
    default visit_expr_refer :: (TransformsTypeInfo stage1 stage2 cx m visitor, ExprIdentifierRefVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> ExprIdentifierRef stage1 -> m (Expr stage2, res)
    visit_expr_refer proxy cx id type_info span ref = do
        type_info <- transform_type_info proxy cx type_info
        (idref, ()) <- visit_expr_identifier_ref proxy cx ref
        pure (Expr'Refer id type_info span idref, ())

    visit_expr_char :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Char -> m (Expr stage2, res)
    default visit_expr_char :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Char -> m (Expr stage2, res)
    visit_expr_char proxy cx id type_info span ch = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Char id type_info span ch, ())

    visit_expr_string :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Text -> m (Expr stage2, res)
    default visit_expr_string :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Text -> m (Expr stage2, res)
    visit_expr_string proxy cx id type_info span s = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'String id type_info span s, ())

    visit_expr_int :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Integer -> m (Expr stage2, res)
    default visit_expr_int :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Integer -> m (Expr stage2, res)
    visit_expr_int proxy cx id type_info span i = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Int id type_info span i, ())

    visit_expr_float :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Rational -> m (Expr stage2, res)
    default visit_expr_float :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Rational -> m (Expr stage2, res)
    visit_expr_float proxy cx id type_info sp f = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Float id type_info sp f, ())

    visit_expr_bool :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Bool -> m (Expr stage2, res)
    default visit_expr_bool :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Bool -> m (Expr stage2, res)
    visit_expr_bool proxy cx id type_info sp b = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Bool id type_info sp b, ())

    visit_expr_tuple :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_tuple :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_tuple proxy cx id type_info sp a b = do
        type_info <- transform_type_info proxy cx type_info
        (a, ()) <- visit_expr proxy cx a
        (b, ()) <- visit_expr proxy cx b
        pure (Expr'Tuple id type_info sp a b, ())

    visit_expr_lambda :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Pattern stage1 -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_lambda :: (TransformsTypeInfo stage1 stage2 cx m visitor, PatternVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Pattern stage1 -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_lambda proxy cx id type_info sp p e = do
        type_info <- transform_type_info proxy cx type_info
        (p, ()) <- visit_pattern proxy cx p
        (e, ()) <- visit_expr proxy cx e
        pure (Expr'Lambda id type_info sp p e, ())

    visit_expr_let :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> [Binding stage1] -> [ADTKey] -> [TypeSynonymKey] -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_let :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsNameMapIndex stage1 stage2 cx m visitor, BindingVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> [Binding stage1] -> [ADTKey] -> [TypeSynonymKey] -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_let proxy cx id type_info sp nmi bs adts tss res = do
        type_info <- transform_type_info proxy cx type_info
        nmi <- transform_name_map_index proxy cx nmi
        bs <- mapM (fmap fst . visit_binding proxy cx) bs
        (res, ()) <- visit_expr proxy cx res
        pure (Expr'Let id type_info sp nmi bs adts tss res, ())

    visit_expr_let_rec :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> [Binding stage1] -> [ADTKey] -> [TypeSynonymKey] -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_let_rec :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsNameMapIndex stage1 stage2 cx m visitor, BindingVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> [Binding stage1] -> [ADTKey] -> [TypeSynonymKey] -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_let_rec proxy cx id type_info sp nmi bs adts tss res = do
        type_info <- transform_type_info proxy cx type_info
        nmi <- transform_name_map_index proxy cx nmi
        bs <- mapM (fmap fst . visit_binding proxy cx) bs
        (res, ()) <- visit_expr proxy cx res
        pure (Expr'LetRec id type_info sp nmi bs adts tss res, ())

    visit_expr_binary_ops :: Proxy visitor -> cx -> ID.ExprID -> InfixGroupedKey stage1 -> TypeInfo stage1 -> Span -> Expr stage1 -> [(Span, OperatorRef stage1, Expr stage1)] -> m (Expr stage2, res)
    default visit_expr_binary_ops :: (TransformsInfixGroupedKey stage1 stage2 cx m visitor, TransformsTypeInfo stage1 stage2 cx m visitor, OperatorRefVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> InfixGroupedKey stage1 -> TypeInfo stage1 -> Span -> Expr stage1 -> [(Span, OperatorRef stage1, Expr stage1)] -> m (Expr stage2, res)
    visit_expr_binary_ops proxy cx id igk type_info sp first more = do
        igk <- transform_infix_grouped_key proxy cx igk
        type_info <- transform_type_info proxy cx type_info
        (first, ()) <- visit_expr proxy cx first
        more <- mapM (\ (sp, op, rhs) -> (sp,,) <$> (fst <$> visit_operator_ref proxy cx op) <*> (fst <$> visit_expr proxy cx rhs)) more
        pure (Expr'BinaryOps id igk type_info sp first more, ())

    visit_expr_call :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_call :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_call proxy cx id type_info sp callee arg = do
        type_info <- transform_type_info proxy cx type_info
        (callee, ()) <- visit_expr proxy cx callee
        (arg, ()) <- visit_expr proxy cx arg
        pure (Expr'Call id type_info sp callee arg, ())

    visit_expr_if :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Span -> Expr stage1 -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_if :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Span -> Expr stage1 -> Expr stage1 -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_if proxy cx id type_info sp if_sp cond true false = do
        type_info <- transform_type_info proxy cx type_info
        (cond, ()) <- visit_expr proxy cx cond
        (true, ()) <- visit_expr proxy cx true
        (false, ()) <- visit_expr proxy cx false
        pure (Expr'If id type_info sp if_sp cond true false, ())

    visit_expr_match :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Span -> Expr stage1 -> [(NameMapIndex stage1, Pattern stage1, Expr stage1)] -> m (Expr stage2, res)
    default visit_expr_match :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsNameMapIndex stage1 stage2 cx m visitor, PatternVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Span -> Expr stage1 -> [(NameMapIndex stage1, Pattern stage1, Expr stage1)] -> m (Expr stage2, res)
    visit_expr_match proxy cx id type_info sp match_sp scrutinee arms = do
        type_info <- transform_type_info proxy cx type_info
        (scrutinee, ()) <- visit_expr proxy cx scrutinee
        arms <- mapM (\ (nmi, pat, body) -> (,,) <$> transform_name_map_index proxy cx nmi <*> (fst <$> visit_pattern proxy cx pat) <*> (fst <$> visit_expr proxy cx body)) arms
        pure (Expr'Match id type_info sp match_sp scrutinee arms, ())

    visit_expr_forall :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> NonEmpty QuantVarKey -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_forall :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsNameMapIndex stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> NameMapIndex stage1 -> NonEmpty QuantVarKey -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_forall proxy cx id type_info sp nmi qvars body = do
        type_info <- transform_type_info proxy cx type_info
        nmi <- transform_name_map_index proxy cx nmi
        (body, ()) <- visit_expr proxy cx body
        pure (Expr'Forall id type_info sp nmi qvars body, ())

    visit_expr_type_apply :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> m (Expr stage2, res)
    default visit_expr_type_apply :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> Expr stage1 -> (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> m (Expr stage2, res)
    visit_expr_type_apply proxy cx id type_info sp callee (targe, targeastk) = do
        type_info <- transform_type_info proxy cx type_info
        (callee, ()) <- visit_expr proxy cx callee
        (targe, ()) <- visit_type_expr proxy cx targe
        targestk <- transform_type_expr_evaled_as_type_key proxy cx targeastk
        pure (Expr'TypeApply id type_info sp callee (targe, targestk), ())

    visit_expr_type_annotation :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> Expr stage1 -> m (Expr stage2, res)
    default visit_expr_type_annotation :: (TransformsTypeInfo stage1 stage2 cx m visitor, TransformsTypeExprEvaledAsTypeKey stage1 stage2 cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> (TypeExpr stage1, TypeExprEvaledAsTypeKey stage1) -> Expr stage1 -> m (Expr stage2, res)
    visit_expr_type_annotation proxy cx id type_info sp (tye, tyeastk) subexpr = do
        type_info <- transform_type_info proxy cx type_info
        (tye, ()) <- visit_type_expr proxy cx tye
        tyeastk <- transform_type_expr_evaled_as_type_key proxy cx tyeastk
        (subexpr, ()) <- visit_expr proxy cx subexpr
        pure (Expr'TypeAnnotation id type_info sp (tye, tyeastk) subexpr, ())

    visit_expr_hole :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> HoleIdentifier -> m (Expr stage2, res)
    default visit_expr_hole :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> HoleIdentifier -> m (Expr stage2, res)
    visit_expr_hole proxy cx id type_info sp hid = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Hole id type_info sp hid, ())

    visit_expr_poison :: Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> m (Expr stage2, res)
    default visit_expr_poison :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> ID.ExprID -> TypeInfo stage1 -> Span -> m (Expr stage2, res)
    visit_expr_poison proxy cx id type_info sp = do
        type_info <- transform_type_info proxy cx type_info
        pure (Expr'Poison id type_info sp, ())

visit_expr :: ExprVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> Expr stage1 -> m (Expr stage2, res)
visit_expr proxy cx (Expr'Refer id type_info span ref) = visit_expr_refer proxy cx id type_info span ref
visit_expr proxy cx (Expr'Char id type_info span ch) = visit_expr_char proxy cx id type_info span ch
visit_expr proxy cx (Expr'String id type_info span s) = visit_expr_string proxy cx id type_info span s
visit_expr proxy cx (Expr'Int id type_info span i) = visit_expr_int proxy cx id type_info span i
visit_expr proxy cx (Expr'Float id type_info sp f) = visit_expr_float proxy cx id type_info sp f
visit_expr proxy cx (Expr'Bool id type_info sp b) = visit_expr_bool proxy cx id type_info sp b
visit_expr proxy cx (Expr'Tuple id type_info sp a b) = visit_expr_tuple proxy cx id type_info sp a b
visit_expr proxy cx (Expr'Lambda id type_info sp p e) = visit_expr_lambda proxy cx id type_info sp p e
visit_expr proxy cx (Expr'Let id type_info sp nmi bs adts tss res) = visit_expr_let proxy cx id type_info sp nmi bs adts tss res
visit_expr proxy cx (Expr'LetRec id type_info sp nmi bs adts tss res) = visit_expr_let_rec proxy cx id type_info sp nmi bs adts tss res
visit_expr proxy cx (Expr'BinaryOps id igk type_info sp first more) = visit_expr_binary_ops proxy cx id igk type_info sp first more
visit_expr proxy cx (Expr'Call id type_info sp callee arg) = visit_expr_call proxy cx id type_info sp callee arg
visit_expr proxy cx (Expr'If id type_info sp if_sp cond true false) = visit_expr_if proxy cx id type_info sp if_sp cond true false
visit_expr proxy cx (Expr'Match id type_info sp match_sp scrutinee arms) = visit_expr_match proxy cx id type_info sp match_sp scrutinee arms
visit_expr proxy cx (Expr'Forall id type_info sp nmi qvars body) = visit_expr_forall proxy cx id type_info sp nmi qvars body
visit_expr proxy cx (Expr'TypeApply id type_info sp callee targ) = visit_expr_type_apply proxy cx id type_info sp callee targ
visit_expr proxy cx (Expr'TypeAnnotation id type_info sp ty subexpr) = visit_expr_type_annotation proxy cx id type_info sp ty subexpr
visit_expr proxy cx (Expr'Hole id type_info sp hid) = visit_expr_hole proxy cx id type_info sp hid
visit_expr proxy cx (Expr'Poison id type_info sp) = visit_expr_poison proxy cx id type_info sp

visit_expr' :: ExprVisitor stage1 stage2 () m () visitor => Proxy visitor -> Expr stage1 -> m (Expr stage2)
visit_expr' proxy = fmap fst . visit_expr proxy ()

class Monad m => PatternADTVariantRefVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_pattern_adt_variant_ref_get :: Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 Type.ADT.VariantIndex -> m (PatternADTVariantRef stage2, res)
    default visit_pattern_adt_variant_ref_get :: (TransformsIdenResolvedKey stage1 stage2 Type.ADT.VariantIndex Type.ADT.VariantIndex cx m visitor, TypeExprVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeExpr stage1 -> Located Text -> IdenResolvedKey stage1 Type.ADT.VariantIndex -> m (PatternADTVariantRef stage2, res)
    visit_pattern_adt_variant_ref_get proxy cx t n r = do
        (t, ()) <- visit_type_expr proxy cx t
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Get t n r, ())

    visit_pattern_adt_variant_ref_single :: Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 Type.ADT.VariantIndex -> m (PatternADTVariantRef stage2, res)
    default visit_pattern_adt_variant_ref_single :: (TransformsNameMapIndex stage1 stage2 cx m visitor, TransformsIdenResolvedKey stage1 stage2 Type.ADT.VariantIndex Type.ADT.VariantIndex cx m visitor, res ~ ()) => Proxy visitor -> cx -> NameMapIndex stage1 -> Located Text -> IdenResolvedKey stage1 Type.ADT.VariantIndex -> m (PatternADTVariantRef stage2, res)
    visit_pattern_adt_variant_ref_single proxy cx nm n r = do
        nm <- transform_name_map_index proxy cx nm
        r <- transform_iden_resolved_key proxy cx r
        pure (SplitIdentifier'Single nm n r, ())

visit_pattern_adt_variant_ref :: PatternADTVariantRefVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> PatternADTVariantRef stage1 -> m (PatternADTVariantRef stage2, res)
visit_pattern_adt_variant_ref proxy cx (SplitIdentifier'Get t n r)= visit_pattern_adt_variant_ref_get proxy cx t n r
visit_pattern_adt_variant_ref proxy cx (SplitIdentifier'Single nm n r) = visit_pattern_adt_variant_ref_single proxy cx nm n r

visit_pattern_adt_variant_ref' :: PatternADTVariantRefVisitor stage1 stage2 () m () visitor => Proxy visitor -> PatternADTVariantRef stage1 -> m (PatternADTVariantRef stage2)
visit_pattern_adt_variant_ref' proxy = fmap fst . visit_pattern_adt_variant_ref proxy ()

class Monad m => PatternVisitor stage1 stage2 cx m res visitor | visitor -> stage1 stage2 cx m res where
    visit_pattern_variable :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> VariableKey -> m (Pattern stage2, res)
    default visit_pattern_variable :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> VariableKey -> m (Pattern stage2, res)
    visit_pattern_variable proxy cx
        type_info sp vk
        = do
        type_info <- transform_type_info proxy cx type_info
        pure (Pattern'Variable type_info sp vk, ())

    visit_pattern_wildcard :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> m (Pattern stage2, res)
    default visit_pattern_wildcard :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> m (Pattern stage2, res)
    visit_pattern_wildcard proxy cx
        type_info sp
        = do
        type_info <- transform_type_info proxy cx type_info
        pure (Pattern'Wildcard type_info sp, ())

    visit_pattern_tuple :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> Pattern stage1 -> Pattern stage1 -> m (Pattern stage2, res)
    default visit_pattern_tuple :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> Pattern stage1 -> Pattern stage1 -> m (Pattern stage2, res)
    visit_pattern_tuple proxy cx
        type_info sp a b
        = do
        type_info <- transform_type_info proxy cx type_info
        (a, ()) <- visit_pattern proxy cx a
        (b, ()) <- visit_pattern proxy cx b
        pure (Pattern'Tuple type_info sp a b, ())

    visit_pattern_named :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> Span -> Located VariableKey -> Pattern stage1 -> m (Pattern stage2, res)
    default visit_pattern_named :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> Span -> Located VariableKey -> Pattern stage1 -> m (Pattern stage2, res)
    visit_pattern_named proxy cx
        type_info sp at_sp vk subpat
        = do
        type_info <- transform_type_info proxy cx type_info
        (subpat, ()) <- visit_pattern proxy cx subpat
        pure (Pattern'Named type_info sp at_sp vk subpat, ())

    visit_pattern_anon_adt_variant :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> PatternADTVariantRef stage1 -> [TypeInfo stage1] -> [Pattern stage1] -> m (Pattern stage2, res)
    default visit_pattern_anon_adt_variant :: (TransformsTypeInfo stage1 stage2 cx m visitor, PatternADTVariantRefVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> PatternADTVariantRef stage1 -> [TypeInfo stage1] -> [Pattern stage1] -> m (Pattern stage2, res)
    visit_pattern_anon_adt_variant proxy cx
        type_info sp variant field_tys fields
        = do
        type_info <- transform_type_info proxy cx type_info
        (variant, ()) <- visit_pattern_adt_variant_ref proxy cx variant
        field_tys <- mapM (transform_type_info proxy cx) field_tys
        fields <- mapM (fmap fst . visit_pattern proxy cx) fields
        pure (Pattern'AnonADTVariant type_info sp variant field_tys fields, ())

    visit_pattern_named_adt_variant :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> PatternADTVariantRef stage1 -> [TypeInfo stage1] -> [(Located Text, Pattern stage1)] -> m (Pattern stage2, res)
    default visit_pattern_named_adt_variant :: (TransformsTypeInfo stage1 stage2 cx m visitor, PatternADTVariantRefVisitor stage1 stage2 cx m () visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> PatternADTVariantRef stage1 -> [TypeInfo stage1] -> [(Located Text, Pattern stage1)] -> m (Pattern stage2, res)
    visit_pattern_named_adt_variant proxy cx
        type_info sp variant field_tys fields
        = do
        type_info <- transform_type_info proxy cx type_info
        (variant, ()) <- visit_pattern_adt_variant_ref proxy cx variant
        field_tys <- mapM (transform_type_info proxy cx) field_tys
        fields <- mapM (\(field_name, field_pat) -> (field_name,) <$> (fst <$> visit_pattern proxy cx field_pat)) fields
        pure (Pattern'NamedADTVariant type_info sp variant field_tys fields, ())

    visit_pattern_poison :: Proxy visitor -> cx -> TypeInfo stage1 -> Span -> m (Pattern stage2, res)
    default visit_pattern_poison :: (TransformsTypeInfo stage1 stage2 cx m visitor, res ~ ()) => Proxy visitor -> cx -> TypeInfo stage1 -> Span -> m (Pattern stage2, res)
    visit_pattern_poison proxy cx
        type_info sp
        = do
        type_info <- transform_type_info proxy cx type_info
        pure (Pattern'Poison type_info sp, ())

visit_pattern :: PatternVisitor stage1 stage2 cx m res visitor => Proxy visitor -> cx -> Pattern stage1 -> m (Pattern stage2, res)
visit_pattern proxy cx (Pattern'Variable type_info sp vk) = visit_pattern_variable proxy cx type_info sp vk
visit_pattern proxy cx (Pattern'Wildcard type_info sp) = visit_pattern_wildcard proxy cx type_info sp
visit_pattern proxy cx (Pattern'Tuple type_info sp a b) = visit_pattern_tuple proxy cx type_info sp a b
visit_pattern proxy cx (Pattern'Named type_info sp at_sp vk subpat) = visit_pattern_named proxy cx type_info sp at_sp vk subpat
visit_pattern proxy cx (Pattern'AnonADTVariant type_info sp variant field_tys fields) = visit_pattern_anon_adt_variant proxy cx type_info sp variant field_tys fields
visit_pattern proxy cx (Pattern'NamedADTVariant type_info sp variant field_tys fields) = visit_pattern_named_adt_variant proxy cx type_info sp variant field_tys fields
visit_pattern proxy cx (Pattern'Poison type_info sp) = visit_pattern_poison proxy cx type_info sp

visit_pattern' :: PatternVisitor stage1 stage2 () m () visitor => Proxy visitor -> Pattern stage1 -> m (Pattern stage2)
visit_pattern' proxy = fmap fst . visit_pattern proxy ()
