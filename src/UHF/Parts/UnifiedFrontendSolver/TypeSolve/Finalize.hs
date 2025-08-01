module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Finalize (remove_infer_vars) where

import UHF.Prelude

import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad.Fix (mfix)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledKey, TypeExprEvaledAsTypeKey)
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
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (DeclRef (..))

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
    Arena.Arena (Maybe (DeclRef TypeWithInferVar.Type)) (IdenResolvedKey (DeclRef TypeWithInferVar.Type)) ->
    Arena.Arena (Maybe (DeclRef TypeWithInferVar.Type)) TypeExprEvaledKey ->
    Arena.Arena (Maybe TypeWithInferVar.Type) TypeExprEvaledAsTypeKey ->
    SIR.SIR WithInferVars ->
    Compiler.WithDiagnostics SolveError.Error Void
        ( SIR.SIR WithoutInferVars
        , Arena.Arena (Maybe (DeclRef Type.Type)) (IdenResolvedKey (DeclRef Type.Type))
        , Arena.Arena (Maybe (DeclRef Type.Type)) TypeExprEvaledKey
        , Arena.Arena (Maybe Type.Type) TypeExprEvaledAsTypeKey
        )
remove_infer_vars infer_vars decl_iden_resolved_arena type_expr_evaled_arena type_expr_evaled_as_type_arena (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    infer_vars <- convert_vars infer_vars
    pure $
        ( SIR.SIR
            (Arena.transform (module_ infer_vars) modules)
            (Arena.transform (adt infer_vars) adts)
            (Arena.transform (type_synonym infer_vars) type_synonyms)
            type_vars
            (Arena.transform (variable infer_vars) variables)
            (SIR.CU root_module main_function)
        , Arena.change_key $ Arena.transform (>>= decl_ref infer_vars) decl_iden_resolved_arena
        , Arena.transform (>>= decl_ref infer_vars) type_expr_evaled_arena
        , Arena.transform (>>= type_ infer_vars) type_expr_evaled_as_type_arena
        )

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

module_ :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Module WithInferVars -> SIR.Module WithoutInferVars
-- TODO: rename mid to id
module_ infer_vars (SIR.Module mid id name_maps_index bindings adts type_synonyms) = SIR.Module mid id name_maps_index (map (binding infer_vars) bindings) adts type_synonyms

variable :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Variable WithInferVars -> SIR.Variable WithoutInferVars
-- TODO: rename vid to id
variable infer_vars (SIR.Variable vid id ty name) = SIR.Variable vid id (type_ infer_vars ty) name

adt :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.ADT WithInferVars -> SIR.ADT WithoutInferVars
adt infer_vars (Type.ADT id name quant_var variants) = Type.ADT id name quant_var (map variant variants)
    where
        variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id (map (\(name, id, (ty, as_type)) -> (name, id, (type_expr infer_vars ty, as_type))) fields)
        variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id (map (\(id, (ty, as_type)) -> (id, (type_expr infer_vars ty, as_type))) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.TypeSynonym WithInferVars -> SIR.TypeSynonym WithoutInferVars
type_synonym infer_vars (Type.TypeSynonym id name (expansion, expansion_as_type)) = Type.TypeSynonym id name (type_expr infer_vars expansion, expansion_as_type)

binding :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Binding WithInferVars -> SIR.Binding WithoutInferVars
binding infer_vars (SIR.Binding bid p eq_sp e) = SIR.Binding bid (pattern infer_vars p) eq_sp (expr infer_vars e)

pattern :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Pattern WithInferVars -> SIR.Pattern WithoutInferVars
pattern infer_vars (SIR.Pattern'Variable id ty sp bn) = SIR.Pattern'Variable id (type_ infer_vars ty) sp bn
pattern infer_vars (SIR.Pattern'Wildcard id ty sp) = SIR.Pattern'Wildcard id (type_ infer_vars ty) sp
pattern infer_vars (SIR.Pattern'Tuple id ty sp l r) = SIR.Pattern'Tuple id (type_ infer_vars ty) sp (pattern infer_vars l) (pattern infer_vars r)
pattern infer_vars (SIR.Pattern'Named id ty sp at_sp bnk subpat) = SIR.Pattern'Named id (type_ infer_vars ty) sp at_sp bnk (pattern infer_vars subpat)
pattern infer_vars (SIR.Pattern'AnonADTVariant id ty sp variant_iden tyargs fields) =
    SIR.Pattern'AnonADTVariant id
        (type_ infer_vars ty)
        sp
        (split_identifier infer_vars variant_iden)
        (map (type_ infer_vars) tyargs)
        (map (pattern infer_vars) fields)
pattern infer_vars (SIR.Pattern'NamedADTVariant id ty sp variant_iden tyargs fields) =
    SIR.Pattern'NamedADTVariant id
        (type_ infer_vars ty)
        sp
        (split_identifier infer_vars variant_iden)
        (map (type_ infer_vars) tyargs)
        (map (\(field_name, field_pat) -> (field_name, pattern infer_vars field_pat)) fields)
pattern infer_vars (SIR.Pattern'Poison id ty sp) = SIR.Pattern'Poison id (type_ infer_vars ty) sp

expr :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Expr WithInferVars -> SIR.Expr WithoutInferVars
-- TODO: rename eid to id
expr infer_vars (SIR.Expr'Refer eid id ty sp iden) = SIR.Expr'Refer eid id (type_ infer_vars ty) sp (split_identifier infer_vars iden)
expr infer_vars (SIR.Expr'Char eid id ty sp c) = SIR.Expr'Char eid id (type_ infer_vars ty) sp c
expr infer_vars (SIR.Expr'String eid id ty sp t) = SIR.Expr'String eid id (type_ infer_vars ty) sp t
expr infer_vars (SIR.Expr'Int eid id ty sp i) = SIR.Expr'Int eid id (type_ infer_vars ty) sp i
expr infer_vars (SIR.Expr'Float eid id ty sp r) = SIR.Expr'Float eid id (type_ infer_vars ty) sp r
expr infer_vars (SIR.Expr'Bool eid id ty sp b) = SIR.Expr'Bool eid id (type_ infer_vars ty) sp b
expr infer_vars (SIR.Expr'Tuple eid id ty sp l r) = SIR.Expr'Tuple eid id (type_ infer_vars ty) sp (expr infer_vars l) (expr infer_vars r)
expr infer_vars (SIR.Expr'Lambda eid id ty sp param body) = SIR.Expr'Lambda eid id (type_ infer_vars ty) sp (pattern infer_vars param) (expr infer_vars body)
expr infer_vars (SIR.Expr'Let eid id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'Let eid id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr infer_vars (SIR.Expr'LetRec eid id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'LetRec eid id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr _ (SIR.Expr'BinaryOps _ _ _ _ _ _ _) = todo
expr infer_vars (SIR.Expr'Call eid id ty sp callee arg) = SIR.Expr'Call eid id (type_ infer_vars ty) sp (expr infer_vars callee) (expr infer_vars arg)
expr infer_vars (SIR.Expr'If eid id ty sp if_sp cond true false) = SIR.Expr'If eid id (type_ infer_vars ty) sp if_sp (expr infer_vars cond) (expr infer_vars true) (expr infer_vars false)
expr infer_vars (SIR.Expr'Match eid id ty sp match_tok_sp testing arms) =
    SIR.Expr'Match
        eid id
        (type_ infer_vars ty)
        sp
        match_tok_sp
        (expr infer_vars testing)
        (map (\(nci, p, e) -> (nci, pattern infer_vars p, expr infer_vars e)) arms)
expr infer_vars (SIR.Expr'TypeAnnotation eid id ty sp (annotation, annotation_evaled) e) = SIR.Expr'TypeAnnotation eid id (type_ infer_vars ty) sp (type_expr infer_vars annotation, annotation_evaled) (expr infer_vars e)
expr infer_vars (SIR.Expr'Forall eid id ty sp name_maps_index names e) = SIR.Expr'Forall eid id (type_ infer_vars ty) sp name_maps_index (NonEmpty.map identity names) (expr infer_vars e)
expr infer_vars (SIR.Expr'TypeApply eid id ty sp e (arg, arg_evaled)) = SIR.Expr'TypeApply eid id (type_ infer_vars ty) sp (expr infer_vars e) (type_expr infer_vars arg, arg_evaled)
expr infer_vars (SIR.Expr'Hole eid id ty sp hid) = SIR.Expr'Hole eid id (type_ infer_vars ty) sp hid
expr infer_vars (SIR.Expr'Poison eid id ty sp) = SIR.Expr'Poison eid id (type_ infer_vars ty) sp

type_expr :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.TypeExpr WithInferVars -> SIR.TypeExpr WithoutInferVars
type_expr _ (SIR.TypeExpr'Refer id evaled sp name_context iden) = SIR.TypeExpr'Refer id evaled sp name_context iden
type_expr infer_vars (SIR.TypeExpr'Get id evaled sp parent name) = SIR.TypeExpr'Get id evaled sp (type_expr infer_vars parent) name
type_expr infer_vars (SIR.TypeExpr'Tuple id evaled sp a b) = SIR.TypeExpr'Tuple id evaled sp (type_expr infer_vars a) (type_expr infer_vars b)
type_expr _ (SIR.TypeExpr'Hole id evaled tyinfo sp hid) = SIR.TypeExpr'Hole id evaled tyinfo sp hid
type_expr infer_vars (SIR.TypeExpr'Function id evaled sp arg res) = SIR.TypeExpr'Function id evaled sp (type_expr infer_vars arg) (type_expr infer_vars res)
type_expr infer_vars (SIR.TypeExpr'Forall id evaled sp name_map_index names sub) = SIR.TypeExpr'Forall id evaled sp name_map_index names (type_expr infer_vars sub)
type_expr infer_vars (SIR.TypeExpr'Apply id evaled sp applied_to args) = SIR.TypeExpr'Apply id evaled sp (type_expr infer_vars applied_to) (type_expr infer_vars args)
type_expr _ (SIR.TypeExpr'Wild id evaled sp) = SIR.TypeExpr'Wild id evaled sp
type_expr _ (SIR.TypeExpr'Poison id evaled sp) = SIR.TypeExpr'Poison id evaled sp

split_identifier ::
    Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.SplitIdentifier WithInferVars -> SIR.SplitIdentifier WithoutInferVars
split_identifier infer_vars (SIR.SplitIdentifier'Get id texpr next) = SIR.SplitIdentifier'Get id (type_expr infer_vars texpr) next
split_identifier _ (SIR.SplitIdentifier'Single id name_context name) = SIR.SplitIdentifier'Single id name_context name

m_decl :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> Maybe (DeclRef TypeWithInferVar.Type) -> Maybe (DeclRef Type)
m_decl infer_vars d = d >>= decl_ref infer_vars

decl_ref :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> DeclRef TypeWithInferVar.Type -> Maybe (DeclRef Type)
decl_ref _ (DeclRef'Module m) = Just $ DeclRef'Module m
decl_ref infer_vars (DeclRef'Type t) = DeclRef'Type <$> type_ infer_vars t
decl_ref _ (DeclRef'ExternPackage ep) = Just $ DeclRef'ExternPackage ep

type_ :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> TypeWithInferVar.Type -> Maybe Type
type_ infer_vars = r
    where
        r TypeWithInferVar.Type'Int = pure Type.Type'Int
        r TypeWithInferVar.Type'Float = pure Type.Type'Float
        r TypeWithInferVar.Type'Char = pure Type.Type'Char
        r TypeWithInferVar.Type'String = pure Type.Type'String
        r TypeWithInferVar.Type'Bool = pure Type.Type'Bool
        r (TypeWithInferVar.Type'ADT a params) = Type.Type'ADT a <$> mapM r params
        r (TypeWithInferVar.Type'Synonym s) = pure $ Type.Type'Synonym s
        r (TypeWithInferVar.Type'Function arg res) = Type.Type'Function <$> r arg <*> r res
        r (TypeWithInferVar.Type'Tuple a b) = Type.Type'Tuple <$> r a <*> r b
        r (TypeWithInferVar.Type'InferVar u) = Arena.get infer_vars u
        r (TypeWithInferVar.Type'QuantVar v) = Just $ Type.Type'QuantVar v
        r (TypeWithInferVar.Type'Forall vars ty) = Type.Type'Forall vars <$> r ty
        r TypeWithInferVar.Type'Kind'Type = pure Type.Type'Kind'Type
        r (TypeWithInferVar.Type'Kind'Arrow a b) = Type.Type'Kind'Arrow <$> r a <*> r b
        r TypeWithInferVar.Type'Kind'Kind = pure Type.Type'Kind'Kind
