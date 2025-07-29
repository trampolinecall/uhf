module UHF.Parts.UnifiedFrontendSolver.SolveTypes.RemoveInferVars (remove) where

import UHF.Prelude

import qualified Data.List.NonEmpty as NonEmpty

import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Aliases
import UHF.Parts.UnifiedFrontendSolver.SolveTypes.Error
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import qualified UHF.Util.Arena as Arena

import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Control.Monad.Fix (mfix)

remove :: TypeSolver.InferVarArena -> TypedWithInferVarsModuleArena -> TypedWithInferVarsADTArena -> TypedWithInferVarsTypeSynonymArena -> TypedWithInferVarsVariableArena -> Compiler.WithDiagnostics Error Void (TypedModuleArena, TypedADTArena, TypedTypeSynonymArena, TypedVariableArena)
remove infer_vars mods adts type_synonyms vars =
    convert_vars infer_vars >>= \ infer_vars ->
    pure (Arena.transform (module_ infer_vars) mods, Arena.transform (adt infer_vars) adts, Arena.transform (type_synonym infer_vars) type_synonyms, Arena.transform (variable infer_vars) vars)

convert_vars :: TypeSolver.InferVarArena -> Compiler.WithDiagnostics Error Void (Arena.Arena (Maybe Type) TypeSolver.InferVarKey)
convert_vars infer_vars =
    -- infinite recursion is not possible because occurs check prevents loops in substitution
    mfix (\ infer_vars_converted -> Arena.transformM (runMaybeT . convert_var infer_vars_converted) infer_vars)
    where
        r infer_vars_converted t =
            case t of
                TypeSolver.Type'Int -> pure Type.Type'Int
                TypeSolver.Type'Float -> pure Type.Type'Float
                TypeSolver.Type'Char -> pure Type.Type'Char
                TypeSolver.Type'String -> pure Type.Type'String
                TypeSolver.Type'Bool -> pure Type.Type'Bool
                TypeSolver.Type'ADT a params -> Type.Type'ADT a <$> mapM (r infer_vars_converted) params
                TypeSolver.Type'Synonym s -> pure $ Type.Type'Synonym s
                TypeSolver.Type'Function arg res -> Type.Type'Function <$> r infer_vars_converted arg <*> r infer_vars_converted res
                TypeSolver.Type'Tuple a b -> Type.Type'Tuple <$> r infer_vars_converted a <*> r infer_vars_converted b
                TypeSolver.Type'InferVar v -> MaybeT $ pure $ Arena.get infer_vars_converted v
                TypeSolver.Type'QuantVar v -> pure $ Type.Type'QuantVar v
                TypeSolver.Type'Forall vars ty -> Type.Type'Forall vars <$> r infer_vars_converted ty
                TypeSolver.Type'Kind'Type -> pure Type.Type'Kind'Type
                TypeSolver.Type'Kind'Arrow a b -> Type.Type'Kind'Arrow <$> r infer_vars_converted a <*> r infer_vars_converted b
                TypeSolver.Type'Kind'Kind -> pure Type.Type'Kind'Kind

        convert_var infer_vars_converted (TypeSolver.InferVar _ (TypeSolver.Substituted s)) = r infer_vars_converted s
        convert_var _ (TypeSolver.InferVar for_what TypeSolver.Fresh) = lift (Compiler.tell_error $ AmbiguousType for_what) >> MaybeT (pure Nothing)

module_ :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsModule -> TypedModule
module_ infer_vars (SIR.Module id name_maps_index bindings adts type_synonyms) = SIR.Module id name_maps_index (map (binding infer_vars) bindings) adts type_synonyms

variable :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsVariable -> TypedVariable
variable infer_vars (SIR.Variable id ty name) = SIR.Variable id (type_ infer_vars ty) name

adt :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsADT -> TypedADT
adt infer_vars (SIR.ADT id name quant_var variants) = SIR.ADT id name quant_var (map variant variants)
    where
        variant (SIR.ADTVariant'Named name id fields) = SIR.ADTVariant'Named name id (map (\ (name, id, ty) -> (name, id, type_expr infer_vars ty)) fields)
        variant (SIR.ADTVariant'Anon name id fields) = SIR.ADTVariant'Anon name id (map (\ (id, ty) -> (id, type_expr infer_vars ty)) fields)

type_synonym :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsTypeSynonym -> TypedTypeSynonym
type_synonym infer_vars (SIR.TypeSynonym id name expansion) = SIR.TypeSynonym id name (type_expr infer_vars expansion)

binding :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsBinding -> TypedBinding
binding infer_vars (SIR.Binding p eq_sp e) = SIR.Binding (pattern infer_vars p) eq_sp (expr infer_vars e)

pattern :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsPattern -> TypedPattern
pattern infer_vars (SIR.Pattern'Variable ty sp bn) = SIR.Pattern'Variable (type_ infer_vars ty) sp bn
pattern infer_vars (SIR.Pattern'Wildcard ty sp) = SIR.Pattern'Wildcard (type_ infer_vars ty) sp
pattern infer_vars (SIR.Pattern'Tuple ty sp l r) = SIR.Pattern'Tuple (type_ infer_vars ty) sp (pattern infer_vars l) (pattern infer_vars r)
pattern infer_vars (SIR.Pattern'Named ty sp at_sp bnk subpat) = SIR.Pattern'Named (type_ infer_vars ty) sp at_sp bnk (pattern infer_vars subpat)
pattern infer_vars (SIR.Pattern'AnonADTVariant ty sp variant_iden variant_resolved tyargs fields) = SIR.Pattern'AnonADTVariant (type_ infer_vars ty) sp (split_identifier infer_vars variant_iden) variant_resolved (map (type_ infer_vars) tyargs) (map (pattern infer_vars) fields)
pattern infer_vars (SIR.Pattern'NamedADTVariant ty sp variant_iden variant_resolved tyargs fields) = SIR.Pattern'NamedADTVariant (type_ infer_vars ty) sp (split_identifier infer_vars variant_iden) variant_resolved (map (type_ infer_vars) tyargs) (map (\ (field_name, field_pat) -> (field_name, pattern infer_vars field_pat)) fields)
pattern infer_vars (SIR.Pattern'Poison ty sp) = SIR.Pattern'Poison (type_ infer_vars ty) sp

expr :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsExpr -> TypedExpr
expr infer_vars (SIR.Expr'Refer id ty sp iden iden_resolved) = SIR.Expr'Refer id (type_ infer_vars ty) sp (split_identifier infer_vars iden) iden_resolved
expr infer_vars (SIR.Expr'Char id ty sp c) = SIR.Expr'Char id (type_ infer_vars ty) sp c
expr infer_vars (SIR.Expr'String id ty sp t) = SIR.Expr'String id (type_ infer_vars ty) sp t
expr infer_vars (SIR.Expr'Int id ty sp i) = SIR.Expr'Int id (type_ infer_vars ty) sp i
expr infer_vars (SIR.Expr'Float id ty sp r) = SIR.Expr'Float id (type_ infer_vars ty) sp r
expr infer_vars (SIR.Expr'Bool id ty sp b) = SIR.Expr'Bool id (type_ infer_vars ty) sp b
expr infer_vars (SIR.Expr'Tuple id ty sp l r) = SIR.Expr'Tuple id (type_ infer_vars ty) sp (expr infer_vars l) (expr infer_vars r)
expr infer_vars (SIR.Expr'Lambda id ty sp param body) = SIR.Expr'Lambda id (type_ infer_vars ty) sp (pattern infer_vars param) (expr infer_vars body)
expr infer_vars (SIR.Expr'Let id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'Let id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr infer_vars (SIR.Expr'LetRec id ty sp name_maps_index bindings adts type_synonyms result) = SIR.Expr'LetRec id (type_ infer_vars ty) sp name_maps_index (map (binding infer_vars) bindings) adts type_synonyms (expr infer_vars result)
expr _ (SIR.Expr'BinaryOps _ void _ _ _ _) = absurd void
expr infer_vars (SIR.Expr'Call id ty sp callee arg) = SIR.Expr'Call id (type_ infer_vars ty) sp (expr infer_vars callee) (expr infer_vars arg)
expr infer_vars (SIR.Expr'If id ty sp if_sp cond true false) = SIR.Expr'If id (type_ infer_vars ty) sp if_sp (expr infer_vars cond) (expr infer_vars true) (expr infer_vars false)
expr infer_vars (SIR.Expr'Match id ty sp match_tok_sp testing arms) = SIR.Expr'Match id (type_ infer_vars ty) sp match_tok_sp (expr infer_vars testing) (map (\ (nci, p, e) -> (nci, pattern infer_vars p, expr infer_vars e)) arms)
expr infer_vars (SIR.Expr'TypeAnnotation id ty sp annotation e) = SIR.Expr'TypeAnnotation id (type_ infer_vars ty) sp (type_expr_and_type infer_vars annotation) (expr infer_vars e)
expr infer_vars (SIR.Expr'Forall id ty sp name_maps_index names e) = SIR.Expr'Forall id (type_ infer_vars ty) sp name_maps_index (NonEmpty.map identity names) (expr infer_vars e)
expr infer_vars (SIR.Expr'TypeApply id ty sp e args) = SIR.Expr'TypeApply id (type_ infer_vars ty) sp (expr infer_vars e) (type_expr_and_type infer_vars args)
expr infer_vars (SIR.Expr'Hole id ty sp hid) = SIR.Expr'Hole id (type_ infer_vars ty) sp hid
expr infer_vars (SIR.Expr'Poison id ty sp) = SIR.Expr'Poison id (type_ infer_vars ty) sp

type_expr :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypedWithInferVarsTypeExpr -> TypedTypeExpr
type_expr infer_vars (SIR.TypeExpr'Refer evaled sp name_context iden resolved) = SIR.TypeExpr'Refer (m_decl infer_vars evaled) sp name_context iden (m_decl infer_vars resolved)
type_expr infer_vars (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get (m_decl infer_vars evaled) sp (type_expr infer_vars parent) name
type_expr infer_vars (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple (m_decl infer_vars evaled) sp (type_expr infer_vars a) (type_expr infer_vars b)
type_expr infer_vars (SIR.TypeExpr'Hole evaled tyinfo sp hid) = SIR.TypeExpr'Hole (m_decl infer_vars evaled) (type_ infer_vars tyinfo) sp hid
type_expr infer_vars (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function (m_decl infer_vars evaled) sp (type_expr infer_vars arg) (type_expr infer_vars res)
type_expr infer_vars (SIR.TypeExpr'Forall evaled sp name_map_index names sub) = SIR.TypeExpr'Forall (m_decl infer_vars evaled) sp name_map_index names (type_expr infer_vars sub)
type_expr infer_vars (SIR.TypeExpr'Apply evaled sp applied_to args) = SIR.TypeExpr'Apply (m_decl infer_vars evaled) sp (type_expr infer_vars applied_to) (type_expr infer_vars args)
type_expr infer_vars (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild (m_decl infer_vars evaled) sp
type_expr infer_vars (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison (m_decl infer_vars evaled) sp

type_expr_and_type :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> (TypedWithInferVarsTypeExpr, TypeWithInferVars) -> (TypedTypeExpr, Maybe Type)
type_expr_and_type infer_vars (te, t) = (type_expr infer_vars te, type_ infer_vars t)

split_identifier :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> SIR.SplitIdentifier single TypedWithInferVars -> SIR.SplitIdentifier single Typed
split_identifier infer_vars (SIR.SplitIdentifier'Get texpr next resolved) = SIR.SplitIdentifier'Get (type_expr infer_vars texpr) next resolved
split_identifier _ (SIR.SplitIdentifier'Single name_context name resolved) = SIR.SplitIdentifier'Single name_context name resolved

m_decl :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> Maybe (SIR.DeclRef TypeWithInferVars) -> Maybe (SIR.DeclRef Type)
m_decl infer_vars d = d >>= decl infer_vars

decl :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> SIR.DeclRef TypeWithInferVars -> Maybe (SIR.DeclRef Type)
decl _ (SIR.DeclRef'Module m) = Just $ SIR.DeclRef'Module m
decl infer_vars (SIR.DeclRef'Type t) = SIR.DeclRef'Type <$> type_ infer_vars t
decl _ (SIR.DeclRef'ExternPackage ep) = Just $ SIR.DeclRef'ExternPackage ep

type_ :: Arena.Arena (Maybe Type) TypeSolver.InferVarKey -> TypeWithInferVars -> Maybe Type
type_ infer_vars = r
    where
        r TypeSolver.Type'Int = pure Type.Type'Int
        r TypeSolver.Type'Float = pure Type.Type'Float
        r TypeSolver.Type'Char = pure Type.Type'Char
        r TypeSolver.Type'String = pure Type.Type'String
        r TypeSolver.Type'Bool = pure Type.Type'Bool
        r (TypeSolver.Type'ADT a params) = Type.Type'ADT a <$> mapM r params
        r (TypeSolver.Type'Synonym s) = pure $ Type.Type'Synonym s
        r (TypeSolver.Type'Function arg res) = Type.Type'Function <$> r arg <*> r res
        r (TypeSolver.Type'Tuple a b) = Type.Type'Tuple <$> r a <*> r b
        r (TypeSolver.Type'InferVar u) = Arena.get infer_vars u
        r (TypeSolver.Type'QuantVar v) = Just $ Type.Type'QuantVar v
        r (TypeSolver.Type'Forall vars ty) = Type.Type'Forall vars <$> r ty
        r TypeSolver.Type'Kind'Type = pure Type.Type'Kind'Type
        r (TypeSolver.Type'Kind'Arrow a b) = Type.Type'Kind'Arrow <$> r a <*> r b
        r TypeSolver.Type'Kind'Kind = pure Type.Type'Kind'Kind
