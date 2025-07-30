module UHF.Parts.UnifiedFrontendSolver.TypeSolve.Finalize (remove_infer_vars) where

import UHF.Prelude

import qualified Data.List.NonEmpty as NonEmpty

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

type WithInferVars =
    ( NameMaps.NameMapStackKey
    , IdenResolvedKey ()
    , TypeWithInferVar.Type
    , TypeExprEvaledKey
    , TypeExprEvaledAsTypeKey
    , TypeWithInferVar.Type
    , InfixGroupedKey
    )
type WithoutInferVars =
    ( NameMaps.NameMapStackKey
    , IdenResolvedKey ()
    , Type.Type
    , TypeExprEvaledKey
    , TypeExprEvaledAsTypeKey
    , Maybe Type.Type
    , InfixGroupedKey
    )

remove_infer_vars :: TypeWithInferVar.InferVarArena -> SIR.SIR WithInferVars -> Compiler.WithDiagnostics SolveError.Error Void (SIR.SIR WithoutInferVars)
remove_infer_vars infer_vars (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) = do
    infer_vars <- convert_vars infer_vars
    pure $
        SIR.SIR
            (Arena.transform (module_ infer_vars) modules)
            (Arena.transform (adt infer_vars) adts)
            (Arena.transform (type_synonym infer_vars) type_synonyms)
            type_vars
            (Arena.transform (variable infer_vars) variables)
            (SIR.CU root_module main_function)

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
module_ infer_vars (SIR.Module id name_maps_index bindings adts type_synonyms) = SIR.Module id name_maps_index (map (binding infer_vars) bindings) adts type_synonyms

variable :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Variable WithInferVars -> SIR.Variable WithoutInferVars
variable infer_vars (SIR.Variable id ty name) = SIR.Variable id (type_ infer_vars ty) name

adt :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.ADT WithInferVars -> SIR.ADT WithoutInferVars
adt infer_vars (Type.ADT id name quant_var variants) = Type.ADT id name quant_var (map variant variants)
    where
        variant (Type.ADT.Variant'Named name id fields) = Type.ADT.Variant'Named name id (map (\(name, id, (ty, as_type)) -> (name, id, (type_expr infer_vars ty, as_type))) fields)
        variant (Type.ADT.Variant'Anon name id fields) = Type.ADT.Variant'Anon name id (map (\(id, (ty, as_type)) -> (id, (type_expr infer_vars ty, as_type))) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.TypeSynonym WithInferVars -> SIR.TypeSynonym WithoutInferVars
type_synonym infer_vars (Type.TypeSynonym id name (expansion, expansion_as_type)) = Type.TypeSynonym id name (type_expr infer_vars expansion, expansion_as_type)

binding :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Binding WithInferVars -> SIR.Binding WithoutInferVars
binding infer_vars (SIR.Binding p eq_sp e) = SIR.Binding (pattern infer_vars p) eq_sp (expr infer_vars e)

pattern :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Pattern WithInferVars -> SIR.Pattern WithoutInferVars
pattern infer_vars (SIR.Pattern'Variable ty sp bn) = SIR.Pattern'Variable (type_ infer_vars ty) sp bn
pattern infer_vars (SIR.Pattern'Wildcard ty sp) = SIR.Pattern'Wildcard (type_ infer_vars ty) sp
pattern infer_vars (SIR.Pattern'Tuple ty sp l r) = SIR.Pattern'Tuple (type_ infer_vars ty) sp (pattern infer_vars l) (pattern infer_vars r)
pattern infer_vars (SIR.Pattern'Named ty sp at_sp bnk subpat) = SIR.Pattern'Named (type_ infer_vars ty) sp at_sp bnk (pattern infer_vars subpat)
pattern infer_vars (SIR.Pattern'AnonADTVariant ty sp variant_iden tyargs fields) =
    SIR.Pattern'AnonADTVariant
        (type_ infer_vars ty)
        sp
        (split_identifier infer_vars variant_iden)
        (map (type_ infer_vars) tyargs)
        (map (pattern infer_vars) fields)
pattern infer_vars (SIR.Pattern'NamedADTVariant ty sp variant_iden tyargs fields) =
    SIR.Pattern'NamedADTVariant
        (type_ infer_vars ty)
        sp
        (split_identifier infer_vars variant_iden)
        (map (type_ infer_vars) tyargs)
        (map (\(field_name, field_pat) -> (field_name, pattern infer_vars field_pat)) fields)
pattern infer_vars (SIR.Pattern'Poison ty sp) = SIR.Pattern'Poison (type_ infer_vars ty) sp

expr :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.Expr WithInferVars -> SIR.Expr WithoutInferVars
expr infer_vars (SIR.Expr'Refer id ty sp iden) = SIR.Expr'Refer id (type_ infer_vars ty) sp (split_identifier infer_vars iden)
expr infer_vars (SIR.Expr'Char id ty sp c) = SIR.Expr'Char id (type_ infer_vars ty) sp c
expr infer_vars (SIR.Expr'String id ty sp t) = SIR.Expr'String id (type_ infer_vars ty) sp t
expr infer_vars (SIR.Expr'Int id ty sp i) = SIR.Expr'Int id (type_ infer_vars ty) sp i
expr infer_vars (SIR.Expr'Float id ty sp r) = SIR.Expr'Float id (type_ infer_vars ty) sp r
expr infer_vars (SIR.Expr'Bool id ty sp b) = SIR.Expr'Bool id (type_ infer_vars ty) sp b
expr infer_vars (SIR.Expr'Tuple id ty sp l r) = SIR.Expr'Tuple id (type_ infer_vars ty) sp (expr infer_vars l) (expr infer_vars r)
expr infer_vars (SIR.Expr'Lambda id ty sp param body) = SIR.Expr'Lambda id (type_ infer_vars ty) sp (pattern infer_vars param) (expr infer_vars body)
expr infer_vars (SIR.Expr'Let id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'Let id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr infer_vars (SIR.Expr'LetRec id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'LetRec id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr _ (SIR.Expr'BinaryOps _ _ _ _ _ _) = todo
expr infer_vars (SIR.Expr'Call id ty sp callee arg) = SIR.Expr'Call id (type_ infer_vars ty) sp (expr infer_vars callee) (expr infer_vars arg)
expr infer_vars (SIR.Expr'If id ty sp if_sp cond true false) = SIR.Expr'If id (type_ infer_vars ty) sp if_sp (expr infer_vars cond) (expr infer_vars true) (expr infer_vars false)
expr infer_vars (SIR.Expr'Match id ty sp match_tok_sp testing arms) =
    SIR.Expr'Match
        id
        (type_ infer_vars ty)
        sp
        match_tok_sp
        (expr infer_vars testing)
        (map (\(nci, p, e) -> (nci, pattern infer_vars p, expr infer_vars e)) arms)
expr infer_vars (SIR.Expr'TypeAnnotation id ty sp (annotation, annotation_evaled) e) = SIR.Expr'TypeAnnotation id (type_ infer_vars ty) sp (type_expr infer_vars annotation, annotation_evaled) (expr infer_vars e)
expr infer_vars (SIR.Expr'Forall id ty sp name_maps_index names e) = SIR.Expr'Forall id (type_ infer_vars ty) sp name_maps_index (NonEmpty.map identity names) (expr infer_vars e)
expr infer_vars (SIR.Expr'TypeApply id ty sp e (arg, arg_evaled)) = SIR.Expr'TypeApply id (type_ infer_vars ty) sp (expr infer_vars e) (type_expr infer_vars arg, arg_evaled)
expr infer_vars (SIR.Expr'Hole id ty sp hid) = SIR.Expr'Hole id (type_ infer_vars ty) sp hid
expr infer_vars (SIR.Expr'Poison id ty sp) = SIR.Expr'Poison id (type_ infer_vars ty) sp

type_expr :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.TypeExpr WithInferVars -> SIR.TypeExpr WithoutInferVars
type_expr infer_vars (SIR.TypeExpr'Refer evaled resolved sp name_context iden) = SIR.TypeExpr'Refer evaled (convert_decl_iden_resolved_key resolved) sp name_context iden
type_expr infer_vars (SIR.TypeExpr'Get evaled resolved sp parent name) = SIR.TypeExpr'Get evaled (convert_decl_iden_resolved_key resolved) sp (type_expr infer_vars parent) name
type_expr infer_vars (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp (type_expr infer_vars a) (type_expr infer_vars b)
type_expr infer_vars (SIR.TypeExpr'Hole evaled tyinfo sp hid) = SIR.TypeExpr'Hole evaled tyinfo sp hid
type_expr infer_vars (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function evaled sp (type_expr infer_vars arg) (type_expr infer_vars res)
type_expr infer_vars (SIR.TypeExpr'Forall evaled sp name_map_index names sub) = SIR.TypeExpr'Forall evaled sp name_map_index names (type_expr infer_vars sub)
type_expr infer_vars (SIR.TypeExpr'Apply evaled sp applied_to args) = SIR.TypeExpr'Apply evaled sp (type_expr infer_vars applied_to) (type_expr infer_vars args)
type_expr infer_vars (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild evaled sp
type_expr infer_vars (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison evaled sp

type_expr_and_type ::
    Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> (SIR.TypeExpr WithInferVars, TypeWithInferVar.Type) -> (SIR.TypeExpr WithoutInferVars, Maybe Type)
type_expr_and_type infer_vars (te, t) = (type_expr infer_vars te, type_ infer_vars t)

split_identifier ::
    Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.SplitIdentifier single WithInferVars -> SIR.SplitIdentifier single WithoutInferVars
split_identifier infer_vars (SIR.SplitIdentifier'Get texpr next resolved) = SIR.SplitIdentifier'Get (type_expr infer_vars texpr) next resolved
split_identifier _ (SIR.SplitIdentifier'Single name_context name resolved) = SIR.SplitIdentifier'Single name_context name resolved

m_decl :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> Maybe (SIR.DeclRef TypeWithInferVar.Type) -> Maybe (SIR.DeclRef Type)
m_decl infer_vars d = d >>= decl infer_vars

decl :: Arena.Arena (Maybe Type) TypeWithInferVar.InferVarKey -> SIR.DeclRef TypeWithInferVar.Type -> Maybe (SIR.DeclRef Type)
decl _ (SIR.DeclRef'Module m) = Just $ SIR.DeclRef'Module m
decl infer_vars (SIR.DeclRef'Type t) = SIR.DeclRef'Type <$> type_ infer_vars t
decl _ (SIR.DeclRef'ExternPackage ep) = Just $ SIR.DeclRef'ExternPackage ep

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
