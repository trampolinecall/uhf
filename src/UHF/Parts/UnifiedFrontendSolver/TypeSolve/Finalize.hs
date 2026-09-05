{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Finalize (remove_infer_vars) where

import UHF.Prelude

import Control.Monad.Fix (mfix)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, convert_decl_iden_resolved_key)
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.Error as SolveError
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena
import UHF.Data.IR.Type (Type)
import UHF.Parts.UnifiedFrontendSolver.TypeSolve.Error (Error(..))
import Data.Data (Proxy (..))
import UHF.Data.SIR.Visitor

type WithInferVars =
    ( NameMaps.NameContextKey
    , IdenResolvedKey ()
    , TypeWithInferVar.Type
    , TypeExprEvaledKey
    , TypeExprEvaledAsTypeKey
    , TypeWithInferVar.Type
    , InfixGroupedKey
    )
type WithoutInferVars =
    ( NameMaps.NameContextKey
    , IdenResolvedKey ()
    , Type.Type
    , TypeExprEvaledKey
    , TypeExprEvaledAsTypeKey
    , Maybe Type.Type
    , InfixGroupedKey
    )

remove_infer_vars ::
    TypeWithInferVar.InferVarArena ->
    Arena.Arena (Maybe (SIR.DeclRef TypeWithInferVar.Type)) (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type)) ->
    Arena.Arena (Maybe (SIR.DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey ->
    Arena.Arena (Maybe TypeWithInferVar.Type) TypeExprEvaledAsTypeKey ->
    SIR.SIR WithInferVars ->
    Compiler.WithDiagnostics SolveError.Error Void
        ( SIR.SIR WithoutInferVars
        , Arena.Arena (Maybe (SIR.DeclRef Type.Type)) (IdenResolvedKey (SIR.DeclRef Type.Type))
        , Arena.Arena (Maybe (SIR.DeclRef Type.Type)) TypeExprEvaledKey
        , Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey
        )
remove_infer_vars infer_vars decl_iden_resolved_arena type_expr_evaled_arena type_expr_evaled_as_type_arena sir = do
    infer_vars <- convert_vars infer_vars
    pure $ runReader
        ((,,,)
            <$> visit_sir' (Proxy :: Proxy FinalizeVisitor) sir
            <*> (Arena.change_key <$> Arena.transformM (maybe (pure Nothing) decl_ref) decl_iden_resolved_arena)
            <*> Arena.transformM (maybe (pure Nothing) decl_ref) type_expr_evaled_arena
            <*> Arena.transformM (maybe (pure Nothing) type_) type_expr_evaled_as_type_arena)
        infer_vars

convert_vars :: TypeWithInferVar.InferVarArena -> Compiler.WithDiagnostics SolveError.Error Void (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)
convert_vars infer_vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    mfix (\infer_vars_converted -> Arena.transformM (runMaybeT . convert_var infer_vars_converted) infer_vars)
    where
        r infer_vars_converted t =
            case t of
                TypeWithInferVar.Type'Int -> pure Type.Type'Int
                TypeWithInferVar.Type'Float -> pure Type.Type'Float
                TypeWithInferVar.Type'Char -> pure Type.Type'Char
                TypeWithInferVar.Type'String -> pure Type.Type'String
                TypeWithInferVar.Type'Bool -> pure Type.Type'Bool
                TypeWithInferVar.Type'ADT a params -> Type.Type'ADT a <$> mapM (r infer_vars_converted) params
                TypeWithInferVar.Type'Synonym s -> pure $ Type.Type'Synonym s
                TypeWithInferVar.Type'Function arg res -> Type.Type'Function <$> r infer_vars_converted arg <*> r infer_vars_converted res
                TypeWithInferVar.Type'Tuple a b -> Type.Type'Tuple <$> r infer_vars_converted a <*> r infer_vars_converted b
                TypeWithInferVar.Type'InferVar v -> MaybeT $ pure $ Arena.get infer_vars_converted v
                TypeWithInferVar.Type'QuantVar v -> pure $ Type.Type'QuantVar v
                TypeWithInferVar.Type'Forall vars ty -> Type.Type'Forall vars <$> r infer_vars_converted ty
                TypeWithInferVar.Type'Kind'Type -> pure Type.Type'Kind'Type
                TypeWithInferVar.Type'Kind'Arrow a b -> Type.Type'Kind'Arrow <$> r infer_vars_converted a <*> r infer_vars_converted b
                TypeWithInferVar.Type'Kind'Kind -> pure Type.Type'Kind'Kind

        convert_var infer_vars_converted (TypeWithInferVar.InferVar _ (TypeWithInferVar.Substituted s)) = r infer_vars_converted s
        convert_var _ (TypeWithInferVar.InferVar for_what TypeWithInferVar.Fresh) = lift (Compiler.tell_error $ SolveError.TSError $ AmbiguousType for_what) >> MaybeT (pure Nothing)

decl_ref :: SIR.DeclRef TypeWithInferVar.Type -> Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey) (Maybe (SIR.DeclRef Type))
decl_ref (SIR.DeclRef'Module m) = pure $ Just $ SIR.DeclRef'Module m
decl_ref (SIR.DeclRef'Type t) = fmap SIR.DeclRef'Type <$> type_ t
decl_ref (SIR.DeclRef'ExternPackage ep) = pure $ Just $ SIR.DeclRef'ExternPackage ep

type_ ::  TypeWithInferVar.Type -> Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey) (Maybe Type)
type_ t = ask >>= \ infer_vars -> pure $ r infer_vars t
    where
        r :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> TypeWithInferVar.Type -> Maybe Type
        r _ TypeWithInferVar.Type'Int = pure Type.Type'Int
        r _ TypeWithInferVar.Type'Float = pure Type.Type'Float
        r _ TypeWithInferVar.Type'Char = pure Type.Type'Char
        r _ TypeWithInferVar.Type'String = pure Type.Type'String
        r _ TypeWithInferVar.Type'Bool = pure Type.Type'Bool
        r infer_vars (TypeWithInferVar.Type'ADT a params) = Type.Type'ADT a <$> mapM (r infer_vars) params
        r _ (TypeWithInferVar.Type'Synonym s) = pure $ Type.Type'Synonym s
        r infer_vars (TypeWithInferVar.Type'Function arg res) = Type.Type'Function <$> r infer_vars arg <*> r infer_vars res
        r infer_vars (TypeWithInferVar.Type'Tuple a b) = Type.Type'Tuple <$> r infer_vars a <*> r infer_vars b
        r infer_vars (TypeWithInferVar.Type'InferVar u) = Arena.get infer_vars u
        r _ (TypeWithInferVar.Type'QuantVar v) = Just $ Type.Type'QuantVar v
        r infer_vars (TypeWithInferVar.Type'Forall vars ty) = Type.Type'Forall vars <$> r infer_vars ty
        r _ TypeWithInferVar.Type'Kind'Type = pure Type.Type'Kind'Type
        r infer_vars (TypeWithInferVar.Type'Kind'Arrow a b) = Type.Type'Kind'Arrow <$> r infer_vars a <*> r infer_vars b
        r _ TypeWithInferVar.Type'Kind'Kind = pure Type.Type'Kind'Kind

data FinalizeVisitor

instance TransformsNameMapIndex WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
instance TransformsIdenResolvedKey WithInferVars WithoutInferVars (SIR.DeclRef TypeWithInferVar.Type) (SIR.DeclRef Type.Type) () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
    transform_iden_resolved_key Proxy () = pure . convert_decl_iden_resolved_key
instance TransformsIdenResolvedKey WithInferVars WithoutInferVars SIR.ValueRef SIR.ValueRef () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
instance TransformsIdenResolvedKey WithInferVars WithoutInferVars Type.ADT.VariantIndex Type.ADT.VariantIndex () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
instance TransformsTypeExprEvaledKey WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
instance TransformsTypeExprEvaledAsTypeKey WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
instance TransformsTypeInfo WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where
    transform_type_info Proxy () = type_
instance TransformsInfixGroupedKey WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) FinalizeVisitor where

instance SIRVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance CUVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance ADTVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance TypeSynonymVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance ModuleVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance VariableVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance BindingVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where

instance TypeExprVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where

instance ExprIdentifierRefVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance OperatorRefVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance ExprVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where

instance PatternADTVariantRefVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
instance PatternVisitor WithInferVars WithoutInferVars () (Reader (Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey)) () FinalizeVisitor where
