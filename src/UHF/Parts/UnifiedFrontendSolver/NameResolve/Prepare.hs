module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

import Data.Functor.Const (Const (Const))
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.NameResolveResultArena
    ( IdenResolvedArena
    , IdenResolvedKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledKey
    )
import UHF.Parts.UnifiedFrontendSolver.NameResolve.ResolveResult
import qualified UHF.Parts.UnifiedFrontendSolver.TypeSolver.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Util.Arena as Arena

type Unprepared = (NameMaps.NameMapStackKey, Const () (), TypeWithInferVar.Type, (), (), (), ())
type Prepared = (NameMaps.NameMapStackKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())

type PrepareState =
    State
        ( IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
        , IdenResolvedArena SIR.ValueRef
        , IdenResolvedArena Type.ADT.VariantIndex
        , TypeExprEvaledArena ()
        , TypeExprEvaledAsTypeArena ()
        )

new_decl_iden_resolved_key :: PrepareState (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type))
new_decl_iden_resolved_key = state $ \(decls, vals, variants, tees, teeats) -> let (key, decls') = Arena.put (Inconclusive ()) decls in (key, (decls', vals, variants, tees, teeats))

new_val_iden_resolved_key :: PrepareState (IdenResolvedKey SIR.ValueRef)
new_val_iden_resolved_key = state $ \(decls, vals, variants, tees, teeats) -> let (key, vals') = Arena.put (Inconclusive ()) vals in (key, (decls, vals', variants, tees, teeats))

new_variant_iden_resolved_key :: PrepareState (IdenResolvedKey Type.ADT.VariantIndex)
new_variant_iden_resolved_key = state $ \(decls, vals, variants, tees, teeats) -> let (key, variants') = Arena.put (Inconclusive ()) variants in (key, (decls, vals, variants', tees, teeats))

new_type_expr_evaled_key :: PrepareState TypeExprEvaledKey
new_type_expr_evaled_key = state $ \(decls, vals, variants, tees, teeats) -> let (key, tees') = Arena.put (Inconclusive ()) tees in (key, (decls, vals, variants, tees', teeats))

new_type_expr_evaled_as_type_key :: PrepareState TypeExprEvaledAsTypeKey
new_type_expr_evaled_as_type_key = state $ \(decls, vals, variants, tees, teeats) -> let (key, teeats') = Arena.put (Inconclusive ()) teeats in (key, (decls, vals, variants, tees, teeats'))

prepare :: SIR.SIR Unprepared -> PrepareState (SIR.SIR Prepared)
prepare (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        <$> Arena.transformM prepare_mod mods
        <*> Arena.transformM prepare_adt adts
        <*> Arena.transformM prepare_type_synonym type_synonyms
        <*> pure type_vars
        <*> Arena.transformM prepare_variable variables
        <*> pure (SIR.CU root_module main_function)

prepare_mod :: SIR.Module Unprepared -> PrepareState (SIR.Module Prepared)
prepare_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms

prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (SIR.ADT id name type_vars variants) = SIR.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (SIR.ADTVariant'Named name id fields) =
            SIR.ADTVariant'Named name id
                <$> mapM
                    -- (\(id, name, (ty, as_type)) -> prepare_type_expr ty >>= \ty -> new_iden_resolved_key >>= \as_type -> pure (id, name, (ty, as_type)))
                    (\(id, name, ty) -> prepare_type_expr ty >>= \ty -> pure (id, name, ty))
                    fields
        prepare_variant (SIR.ADTVariant'Anon name id fields) =
            SIR.ADTVariant'Anon name id
                -- <$> mapM (\(id, (ty, as_type)) -> prepare_type_expr ty >>= \ty -> new_iden_resolved_key >>= \as_type -> pure (id, (ty, as_type))) fields
                <$> mapM (\(id, ty) -> prepare_type_expr ty >>= \ty -> pure (id, ty)) fields

prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (SIR.TypeSynonym id name expansion) = do
    expansion <- prepare_type_expr expansion
    -- as_type <- new_iden_resolved_key
    pure $ SIR.TypeSynonym id name expansion

prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable varid tyinfo n) = pure $ SIR.Variable varid tyinfo n

prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr

prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer () sp name_maps id (Const ())) = SIR.TypeExpr'Refer <$> new_type_expr_evaled_key <*> pure sp <*> pure name_maps <*> pure id <*> new_decl_iden_resolved_key
prepare_type_expr (SIR.TypeExpr'Get () sp parent name) = SIR.TypeExpr'Get <$> new_type_expr_evaled_key <*> pure sp <*> prepare_type_expr parent <*> pure name
prepare_type_expr (SIR.TypeExpr'Tuple () sp a b) = SIR.TypeExpr'Tuple <$> new_type_expr_evaled_key <*> pure sp <*> prepare_type_expr a <*> prepare_type_expr b
prepare_type_expr (SIR.TypeExpr'Hole () () sp hid) = SIR.TypeExpr'Hole <$> new_type_expr_evaled_key <*> new_type_expr_evaled_as_type_key <*> pure sp <*> pure hid
prepare_type_expr (SIR.TypeExpr'Function () sp arg res) = SIR.TypeExpr'Function <$> new_type_expr_evaled_key <*> pure sp <*> prepare_type_expr arg <*> prepare_type_expr res
prepare_type_expr (SIR.TypeExpr'Forall () sp name_maps vars ty) = SIR.TypeExpr'Forall <$> new_type_expr_evaled_key <*> pure sp <*> pure name_maps <*> pure vars <*> prepare_type_expr ty
prepare_type_expr (SIR.TypeExpr'Apply () sp ty args) = SIR.TypeExpr'Apply <$> new_type_expr_evaled_key <*> pure sp <*> prepare_type_expr ty <*> prepare_type_expr args
prepare_type_expr (SIR.TypeExpr'Wild () sp) = SIR.TypeExpr'Wild <$> new_type_expr_evaled_key <*> pure sp
prepare_type_expr (SIR.TypeExpr'Poison () sp) = SIR.TypeExpr'Poison <$> new_type_expr_evaled_key <*> pure sp

prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
prepare_expr (SIR.Expr'Refer id type_info sp iden (Const ())) = SIR.Expr'Refer id type_info sp <$> prepare_split_iden new_val_iden_resolved_key iden <*> new_val_iden_resolved_key
prepare_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
prepare_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
prepare_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
prepare_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
prepare_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
prepare_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> prepare_expr a <*> prepare_expr b
prepare_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> prepare_pat param <*> prepare_expr body
prepare_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'BinaryOps id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps id allowed type_info sp
        <$> prepare_expr first
        <*> mapM (\(sp, iden, Const (), rhs) -> (sp,,,) <$> prepare_split_iden new_val_iden_resolved_key iden <*> new_val_iden_resolved_key <*> prepare_expr rhs) ops
prepare_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    prepare_type_expr ty >>= \ty -> new_type_expr_evaled_as_type_key >>= \ty_resolved -> SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) <$> prepare_expr e
prepare_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) =
    prepare_type_expr arg >>= \arg -> new_type_expr_evaled_as_type_key >>= \arg_resolved -> SIR.Expr'TypeApply id type_info sp <$> prepare_expr e <*> pure (arg, arg_resolved)
prepare_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
prepare_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
prepare_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden (Const ()) tyargs subpat) =
    SIR.Pattern'AnonADTVariant type_info sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> new_variant_iden_resolved_key
        <*> pure tyargs
        <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden (Const ()) tyargs subpat) =
    SIR.Pattern'NamedADTVariant
        type_info
        sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> new_variant_iden_resolved_key
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

prepare_split_iden ::
    PrepareState (IdenResolvedKey resolved) -> SIR.SplitIdentifier resolved Unprepared -> PrepareState (SIR.SplitIdentifier resolved Prepared)
prepare_split_iden new_key (SIR.SplitIdentifier'Get texpr next (Const ())) = SIR.SplitIdentifier'Get <$> prepare_type_expr texpr <*> pure next <*> new_key
prepare_split_iden new_key (SIR.SplitIdentifier'Single name_maps i (Const ())) = SIR.SplitIdentifier'Single name_maps i <$> new_key
