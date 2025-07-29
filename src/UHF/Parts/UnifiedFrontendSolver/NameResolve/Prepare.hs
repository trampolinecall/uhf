module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

import Control.Arrow (second)
import Data.Functor.Const (Const (Const))
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import qualified UHF.Util.Arena as Arena

type Unprepared = (NameMaps.NameMapStackKey, Const () (), (), (), (), ())
type Prepared = (NameMaps.NameMapStackKey, ResolveResult () () (), (), (), (), ())

prepare :: SIR.SIR Unprepared -> SIR.SIR Prepared
prepare (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        (Arena.transform prepare_mod mods)
        (Arena.transform prepare_adt adts)
        (Arena.transform prepare_type_synonym type_synonyms)
        type_vars
        (Arena.transform prepare_variable variables)
        (SIR.CU root_module main_function)

prepare_mod :: SIR.Module Unprepared -> SIR.Module Prepared
prepare_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map (map prepare_binding bindings) adts type_synonyms

prepare_adt :: SIR.ADT Unprepared -> SIR.ADT Prepared
prepare_adt (SIR.ADT id name type_vars variants) = SIR.ADT id name type_vars (map prepare_variant variants)
    where
        prepare_variant (SIR.ADTVariant'Named name id fields) = SIR.ADTVariant'Named name id (map (\(id, name, ty) -> (id, name, prepare_type_expr ty)) fields)
        prepare_variant (SIR.ADTVariant'Anon name id fields) = SIR.ADTVariant'Anon name id (map (\(id, ty) -> (id, prepare_type_expr ty)) fields)

prepare_type_synonym :: SIR.TypeSynonym Unprepared -> SIR.TypeSynonym Prepared
prepare_type_synonym (SIR.TypeSynonym id name expansion) = SIR.TypeSynonym id name (prepare_type_expr expansion)

prepare_variable :: SIR.Variable Unprepared -> SIR.Variable Prepared
prepare_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

prepare_binding :: SIR.Binding Unprepared -> SIR.Binding Prepared
prepare_binding (SIR.Binding target eq_sp expr) = SIR.Binding (prepare_pat target) eq_sp (prepare_expr expr)

prepare_type_expr :: SIR.TypeExpr Unprepared -> SIR.TypeExpr Prepared
prepare_type_expr (SIR.TypeExpr'Refer (Const ()) sp name_maps id (Const ())) = SIR.TypeExpr'Refer (Inconclusive ()) sp name_maps id (Inconclusive ())
prepare_type_expr (SIR.TypeExpr'Get (Const ()) sp parent name) = SIR.TypeExpr'Get (Inconclusive ()) sp (prepare_type_expr parent) name
prepare_type_expr (SIR.TypeExpr'Tuple (Const ()) sp a b) = SIR.TypeExpr'Tuple (Inconclusive ()) sp (prepare_type_expr a) (prepare_type_expr b)
prepare_type_expr (SIR.TypeExpr'Hole (Const ()) (Const ()) sp hid) = SIR.TypeExpr'Hole (Inconclusive ()) (Inconclusive ()) sp hid
prepare_type_expr (SIR.TypeExpr'Function (Const ()) sp arg res) = SIR.TypeExpr'Function (Inconclusive ()) sp (prepare_type_expr arg) (prepare_type_expr res)
prepare_type_expr (SIR.TypeExpr'Forall (Const ()) sp name_maps vars ty) = SIR.TypeExpr'Forall (Inconclusive ()) sp name_maps vars (prepare_type_expr ty)
prepare_type_expr (SIR.TypeExpr'Apply (Const ()) sp ty args) = SIR.TypeExpr'Apply (Inconclusive ()) sp (prepare_type_expr ty) (prepare_type_expr args)
prepare_type_expr (SIR.TypeExpr'Wild (Const ()) sp) = SIR.TypeExpr'Wild (Inconclusive ()) sp
prepare_type_expr (SIR.TypeExpr'Poison (Const ()) sp) = SIR.TypeExpr'Poison (Inconclusive ()) sp

prepare_expr :: SIR.Expr Unprepared -> SIR.Expr Prepared
prepare_expr (SIR.Expr'Refer id type_info sp iden (Const ())) = SIR.Expr'Refer id type_info sp (prepare_split_iden iden) (Inconclusive ())
prepare_expr (SIR.Expr'Char id type_info sp c) = SIR.Expr'Char id type_info sp c
prepare_expr (SIR.Expr'String id type_info sp s) = SIR.Expr'String id type_info sp s
prepare_expr (SIR.Expr'Int id type_info sp i) = SIR.Expr'Int id type_info sp i
prepare_expr (SIR.Expr'Float id type_info sp f) = SIR.Expr'Float id type_info sp f
prepare_expr (SIR.Expr'Bool id type_info sp b) = SIR.Expr'Bool id type_info sp b
prepare_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp (prepare_expr a) (prepare_expr b)
prepare_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp (prepare_pat param) (prepare_expr body)
prepare_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps (map prepare_binding bindings) adts type_synonyms (prepare_expr body)
prepare_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps (map prepare_binding bindings) adts type_synonyms (prepare_expr body)
prepare_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps
        id
        allowed
        type_info
        sp
        (prepare_expr first)
        (map (\(sp, iden, Const (), rhs) -> (sp, prepare_split_iden iden, Inconclusive (), prepare_expr rhs)) ops)
prepare_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp (prepare_expr callee) (prepare_expr arg)
prepare_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp (prepare_expr cond) (prepare_expr t) (prepare_expr f)
prepare_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) = SIR.Expr'Match id type_info sp match_tok_sp (prepare_expr e) (map (\(ncs, pat, expr) -> (ncs, prepare_pat pat, prepare_expr expr)) arms)
prepare_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, Const ()) e) = SIR.Expr'TypeAnnotation id type_info sp (prepare_type_expr ty, Inconclusive ()) (prepare_expr e)
prepare_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars (prepare_expr e)
prepare_expr (SIR.Expr'TypeApply id type_info sp e (arg, Const ())) = SIR.Expr'TypeApply id type_info sp (prepare_expr e) (prepare_type_expr arg, Inconclusive ())
prepare_expr (SIR.Expr'Hole id type_info sp hid) = SIR.Expr'Hole id type_info sp hid
prepare_expr (SIR.Expr'Poison id type_info sp) = SIR.Expr'Poison id type_info sp

prepare_pat :: SIR.Pattern Unprepared -> SIR.Pattern Prepared
prepare_pat (SIR.Pattern'Variable type_info sp bnk) = SIR.Pattern'Variable type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard type_info sp) = SIR.Pattern'Wildcard type_info sp
prepare_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp (prepare_pat a) (prepare_pat b)
prepare_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk (prepare_pat subpat)
prepare_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden (Const ()) tyargs subpat) = SIR.Pattern'AnonADTVariant type_info sp (prepare_split_iden variant_iden) (Inconclusive ()) tyargs (map prepare_pat subpat)
prepare_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden (Const ()) tyargs subpat) = SIR.Pattern'NamedADTVariant type_info sp (prepare_split_iden variant_iden) (Inconclusive ()) tyargs (map (second prepare_pat) subpat)
prepare_pat (SIR.Pattern'Poison type_info sp) = SIR.Pattern'Poison type_info sp

prepare_split_iden :: SIR.SplitIdentifier resolved Unprepared -> SIR.SplitIdentifier resolved Prepared
prepare_split_iden (SIR.SplitIdentifier'Get texpr next (Const ())) = SIR.SplitIdentifier'Get (prepare_type_expr texpr) next (Inconclusive ())
prepare_split_iden (SIR.SplitIdentifier'Single name_maps i (Const ())) = SIR.SplitIdentifier'Single name_maps i (Inconclusive ())
