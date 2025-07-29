module UHF.Parts.UnifiedFrontendSolver.NameResolve.Finalize (finalize) where

import UHF.Prelude

import Control.Arrow (first, second)
import Data.Functor.Const (Const (Const))
import qualified Data.Map as Map
import qualified UHF.Compiler as Compiler
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Error as Error
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver as TypeSolver
import UHF.Source.Located (Located)
import qualified UHF.Util.Arena as Arena

type Resolved =
    ( NameMaps.NameMapStackKey
    , ResolveResult Error.Error Compiler.ErrorReportedPromise ()
    , SIR.DeclRef TypeSolver.Type
    , TypeSolver.Type
    , ()
    , ()
    )
type Finalized = (NameMaps.NameMapStackKey, Maybe (), SIR.DeclRef TypeSolver.Type, TypeSolver.Type, (), ())

finalize :: SIR.SIR Resolved -> Error.WithErrors (SIR.SIR Finalized)
finalize (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        <$> Arena.transformM finalize_mod mods
        <*> Arena.transformM finalize_adt adts
        <*> Arena.transformM finalize_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM finalize_variable variables
        <*> pure (SIR.CU root_module main_function)

finalize_mod :: SIR.Module Resolved -> Error.WithErrors (SIR.Module Finalized)
finalize_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms

finalize_adt :: SIR.ADT Resolved -> Error.WithErrors (SIR.ADT Finalized)
finalize_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM finalize_variant variants
    where
        finalize_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    (\(id, name, (ty, as_type)) -> finalize_type_expr ty >>= \ty -> finalize_result as_type >>= \as_type -> pure (id, name, (ty, as_type)))
                    fields
        finalize_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM (\(id, (ty, as_type)) -> finalize_type_expr ty >>= \ty -> finalize_result as_type >>= \as_type -> pure (id, (ty, as_type))) fields

finalize_type_synonym :: SIR.TypeSynonym Resolved -> Error.WithErrors (SIR.TypeSynonym Finalized)
finalize_type_synonym (Type.TypeSynonym id name (expansion, as_type)) = do
    expansion <- finalize_type_expr expansion
    as_type <- finalize_result as_type
    pure $ Type.TypeSynonym id name (expansion, as_type)

finalize_variable :: SIR.Variable Resolved -> Error.WithErrors (SIR.Variable Finalized)
finalize_variable (SIR.Variable varid tyinfo n) = pure $ SIR.Variable varid tyinfo n

finalize_binding :: SIR.Binding Resolved -> Error.WithErrors (SIR.Binding Finalized)
finalize_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> finalize_pat target <*> pure eq_sp <*> finalize_expr expr

finalize_type_expr :: SIR.TypeExpr Resolved -> Error.WithErrors (SIR.TypeExpr Finalized)
finalize_type_expr (SIR.TypeExpr'Refer evaled sp name_maps id id_resolved) = SIR.TypeExpr'Refer <$> finalize_result evaled <*> pure sp <*> pure name_maps <*> pure id <*> finalize_result id_resolved
finalize_type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get <$> finalize_result evaled <*> pure sp <*> finalize_type_expr parent <*> pure name
finalize_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple <$> finalize_result evaled <*> pure sp <*> finalize_type_expr a <*> finalize_type_expr b
finalize_type_expr (SIR.TypeExpr'Hole evaled evaled_as_type sp hid) = SIR.TypeExpr'Hole <$> finalize_result evaled <*> finalize_result evaled_as_type <*> pure sp <*> pure hid
finalize_type_expr (SIR.TypeExpr'Function evaled sp arg res) = SIR.TypeExpr'Function <$> finalize_result evaled <*> pure sp <*> finalize_type_expr arg <*> finalize_type_expr res
finalize_type_expr (SIR.TypeExpr'Forall evaled sp name_maps vars ty) = SIR.TypeExpr'Forall <$> finalize_result evaled <*> pure sp <*> pure name_maps <*> pure vars <*> finalize_type_expr ty
finalize_type_expr (SIR.TypeExpr'Apply evaled sp ty args) = SIR.TypeExpr'Apply <$> finalize_result evaled <*> pure sp <*> finalize_type_expr ty <*> finalize_type_expr args
finalize_type_expr (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild <$> finalize_result evaled <*> pure sp
finalize_type_expr (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison <$> finalize_result evaled <*> pure sp

finalize_expr :: SIR.Expr Resolved -> Error.WithErrors (SIR.Expr Finalized)
finalize_expr (SIR.Expr'Refer id type_info sp iden iden_resolved) = SIR.Expr'Refer id type_info sp <$> finalize_split_iden iden <*> finalize_result iden_resolved
finalize_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
finalize_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
finalize_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
finalize_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
finalize_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
finalize_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> finalize_expr a <*> finalize_expr b
finalize_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> finalize_pat param <*> finalize_expr body
finalize_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms <*> finalize_expr body
finalize_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps <$> mapM finalize_binding bindings <*> pure adts <*> pure type_synonyms <*> finalize_expr body
finalize_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> finalize_expr first
        <*> mapM (\(sp, iden, op_resolved, rhs) -> (sp,,,) <$> finalize_split_iden iden <*> finalize_result op_resolved <*> finalize_expr rhs) ops
finalize_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> finalize_expr callee <*> finalize_expr arg
finalize_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> finalize_expr cond <*> finalize_expr t <*> finalize_expr f
finalize_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> finalize_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> finalize_pat pat <*> finalize_expr expr) arms
finalize_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) e) =
    finalize_type_expr ty >>= \ty -> finalize_result ty_resolved >>= \ty_resolved -> SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) <$> finalize_expr e
finalize_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars <$> finalize_expr e
finalize_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_resolved)) =
    finalize_type_expr arg >>= \arg -> finalize_result arg_resolved >>= \arg_resolved -> SIR.Expr'TypeApply id type_info sp <$> finalize_expr e <*> pure (arg, arg_resolved)
finalize_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
finalize_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

finalize_pat :: SIR.Pattern Resolved -> Error.WithErrors (SIR.Pattern Finalized)
finalize_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
finalize_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
finalize_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> finalize_pat a <*> finalize_pat b
finalize_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> finalize_pat subpat
finalize_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden variant_resolved tyargs subpat) =
    SIR.Pattern'AnonADTVariant type_info sp
        <$> finalize_split_iden variant_iden
        <*> finalize_result variant_resolved
        <*> pure tyargs
        <*> mapM finalize_pat subpat
finalize_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden variant_resolved tyargs subpat) =
    SIR.Pattern'NamedADTVariant
        type_info
        sp
        <$> finalize_split_iden variant_iden
        <*> finalize_result variant_resolved
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> finalize_pat pat) subpat
finalize_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

finalize_split_iden :: SIR.SplitIdentifier resolved Resolved -> Error.WithErrors (SIR.SplitIdentifier resolved Finalized)
finalize_split_iden (SIR.SplitIdentifier'Get texpr next resolved) = SIR.SplitIdentifier'Get <$> finalize_type_expr texpr <*> pure next <*> finalize_result resolved
finalize_split_iden (SIR.SplitIdentifier'Single name_maps i resolved) = SIR.SplitIdentifier'Single name_maps i <$> finalize_result resolved

finalize_result :: ResolveResult Error.Error Compiler.ErrorReportedPromise a -> Error.WithErrors (Maybe a)
finalize_result (Inconclusive err) = Compiler.tell_err err >> pure Nothing
finalize_result (Errored _) = pure Nothing
finalize_result (Resolved r) = pure $ Just r
