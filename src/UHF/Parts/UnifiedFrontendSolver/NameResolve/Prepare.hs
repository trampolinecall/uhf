module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

import Data.Functor.Const (Const (Const))
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( IdenResolvedArena
    , IdenResolvedKey
    , TypeExprEvaledArena
    , TypeExprEvaledAsTypeArena
    , TypeExprEvaledAsTypeKey
    , TypeExprEvaledKey
    )
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalAsTypeTask (..), TypeExprEvalTask (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena

type Unprepared = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())
type Prepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())

-- TODO: make someday this will turn into a RWST?
type PrepareState =
    WriterT
        ( [IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)]
        , [IdenResolveTask SIR.ValueRef]
        , [IdenResolveTask Type.ADT.VariantIndex]
        , [TypeExprEvalTask]
        , [TypeExprEvalAsTypeTask]
        )
        ( State
            ( IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
            , IdenResolvedArena SIR.ValueRef
            , IdenResolvedArena Type.ADT.VariantIndex
            , TypeExprEvaledArena
            , TypeExprEvaledAsTypeArena
            )
        )

new_decl_iden_resolved_key ::
    (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type) -> IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)) ->
    PrepareState (IdenResolvedKey (SIR.DeclRef TypeWithInferVar.Type))
new_decl_iden_resolved_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, decls') = Arena.put (Inconclusive Nothing) decls in (key, (decls', vals, variants, tees, teeats))
    writer ((), ([make_task key], [], [], [], []))
    pure key

new_val_iden_resolved_key :: (IdenResolvedKey SIR.ValueRef -> IdenResolveTask SIR.ValueRef) -> PrepareState (IdenResolvedKey SIR.ValueRef)
new_val_iden_resolved_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, vals') = Arena.put (Inconclusive Nothing) vals in (key, (decls, vals', variants, tees, teeats))
    writer ((), ([], [make_task key], [], [], []))
    pure key

new_variant_iden_resolved_key ::
    (IdenResolvedKey Type.ADT.VariantIndex -> IdenResolveTask Type.ADT.VariantIndex) -> PrepareState (IdenResolvedKey Type.ADT.VariantIndex)
new_variant_iden_resolved_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, variants') = Arena.put (Inconclusive Nothing) variants in (key, (decls, vals, variants', tees, teeats))
    writer ((), ([], [], [make_task key], [], []))
    pure key

new_type_expr_evaled_key :: (TypeExprEvaledKey -> TypeExprEvalTask) -> PrepareState TypeExprEvaledKey
new_type_expr_evaled_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, tees') = Arena.put (Inconclusive Nothing) tees in (key, (decls, vals, variants, tees', teeats))
    writer ((), ([], [], [], [make_task key], []))
    pure key

new_type_expr_evaled_as_type_key :: (TypeExprEvaledAsTypeKey -> TypeExprEvalAsTypeTask) -> PrepareState TypeExprEvaledAsTypeKey
new_type_expr_evaled_as_type_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, teeats') = Arena.put (Inconclusive Nothing) teeats in (key, (decls, vals, variants, tees, teeats'))
    writer ((), ([], [], [], [], [make_task key]))
    pure key

prepare ::
    SIR.SIR Unprepared ->
    ( SIR.SIR Prepared
    , ( IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type)
      , IdenResolvedArena SIR.ValueRef
      , IdenResolvedArena Type.ADT.VariantIndex
      , TypeExprEvaledArena
      , TypeExprEvaledAsTypeArena
      )
    , ( [IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)]
      , [IdenResolveTask SIR.ValueRef]
      , [IdenResolveTask Type.ADT.VariantIndex]
      , [TypeExprEvalTask]
      , [TypeExprEvalAsTypeTask]
      )
    )
prepare (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    let ((sir, tasks), arenas) =
            runState
                ( runWriterT
                    ( SIR.SIR
                        <$> Arena.transformM prepare_mod mods
                        <*> Arena.transformM prepare_adt adts
                        <*> Arena.transformM prepare_type_synonym type_synonyms
                        <*> pure type_vars
                        <*> Arena.transformM prepare_variable variables
                        <*> pure (SIR.CU root_module main_function)
                    )
                )
                (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new)
    in (sir, arenas, tasks)
prepare_mod :: SIR.Module Unprepared -> PrepareState (SIR.Module Prepared)
prepare_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms
prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    ( \(id, name, (ty, ())) ->
                        prepare_type_expr ty >>= \ty -> new_type_expr_evaled_as_type_key (EvalAsType $ SIR.type_expr_evaled ty) >>= \as_type -> pure (id, name, (ty, as_type))
                    )
                    fields
        prepare_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM
                    ( \(id, (ty, ())) -> prepare_type_expr ty >>= \ty -> new_type_expr_evaled_as_type_key (EvalAsType $ SIR.type_expr_evaled ty) >>= \as_type -> pure (id, (ty, as_type))
                    )
                    fields
prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (Type.TypeSynonym id name (expansion, ())) = do
    expansion <- prepare_type_expr expansion
    as_type <- new_type_expr_evaled_as_type_key (EvalAsType $ SIR.type_expr_evaled expansion)
    pure $ Type.TypeSynonym id name (expansion, as_type)
prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable varid tyinfo n) = pure $ SIR.Variable varid tyinfo n
prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr
prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer () (Const ()) sp name_maps id) = do
    resolved_key <- new_decl_iden_resolved_key $ ResolveRoot name_maps id
    evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
    pure $ SIR.TypeExpr'Refer evaled_key resolved_key sp name_maps id
prepare_type_expr (SIR.TypeExpr'Get () (Const ()) sp parent name) = do
    parent <- prepare_type_expr parent
    resolved_key <- new_decl_iden_resolved_key $ ResolveGet (SIR.type_expr_evaled parent) name
    evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
    pure $ SIR.TypeExpr'Get evaled_key resolved_key sp parent name
prepare_type_expr (SIR.TypeExpr'Tuple () sp a b) = do
    a <- prepare_type_expr a
    b <- prepare_type_expr b
    evaled_key <- new_type_expr_evaled_key $ MakeTuple (SIR.type_expr_evaled a) (SIR.type_expr_evaled b)
    pure $ SIR.TypeExpr'Tuple evaled_key sp a b
prepare_type_expr (SIR.TypeExpr'Hole () () sp hid) = do
    evaled_key <- new_type_expr_evaled_key MakeInferVar
    evaled_as_type_key <- new_type_expr_evaled_as_type_key (EvalAsType evaled_key)
    pure $ SIR.TypeExpr'Hole evaled_key evaled_as_type_key sp hid
prepare_type_expr (SIR.TypeExpr'Function () sp arg res) = do
    arg <- prepare_type_expr arg
    res <- prepare_type_expr res
    evaled_key <- new_type_expr_evaled_key $ MakeFunction (SIR.type_expr_evaled arg) (SIR.type_expr_evaled res)
    pure $ SIR.TypeExpr'Function evaled_key sp arg res
prepare_type_expr (SIR.TypeExpr'Forall () sp name_maps vars ty) = do
    ty <- prepare_type_expr ty
    evaled_key <- new_type_expr_evaled_key $ MakeForall vars (SIR.type_expr_evaled ty)
    pure $ SIR.TypeExpr'Forall evaled_key sp name_maps vars ty
prepare_type_expr (SIR.TypeExpr'Apply () sp ty args) = do
    ty <- prepare_type_expr ty
    args <- prepare_type_expr args
    evaled_key <- new_type_expr_evaled_key $ MakeApply (SIR.type_expr_evaled ty) (SIR.type_expr_evaled args)
    pure $ SIR.TypeExpr'Apply evaled_key sp ty args
prepare_type_expr (SIR.TypeExpr'Wild () sp) = do
    evaled_key <- new_type_expr_evaled_key MakeInferVar
    pure $ SIR.TypeExpr'Wild evaled_key sp
prepare_type_expr (SIR.TypeExpr'Poison () sp) = do
    evaled_key <- new_type_expr_evaled_key MakeInferVar
    pure $ SIR.TypeExpr'Poison evaled_key sp
prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
prepare_expr (SIR.Expr'Refer id type_info sp iden) = SIR.Expr'Refer id type_info sp <$> prepare_split_iden new_val_iden_resolved_key iden
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
        <*> mapM
            (\(sp, iden, rhs) -> (sp,,) <$> prepare_split_iden new_val_iden_resolved_key iden <*> prepare_expr rhs)
            ops
prepare_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ()) e) =
    prepare_type_expr ty >>= \ty ->
        new_type_expr_evaled_as_type_key (EvalAsType $ SIR.type_expr_evaled ty) >>= \ty_resolved -> SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved) <$> prepare_expr e
prepare_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply id type_info sp e (arg, ())) =
    prepare_type_expr arg >>= \arg ->
        new_type_expr_evaled_as_type_key (EvalAsType $ SIR.type_expr_evaled arg) >>= \arg_resolved -> SIR.Expr'TypeApply id type_info sp <$> prepare_expr e <*> pure (arg, arg_resolved)
prepare_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
prepare_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp
prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
prepare_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant type_info sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> pure tyargs
        <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant
        type_info
        sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp
prepare_split_iden ::
    ((IdenResolvedKey result -> IdenResolveTask result) -> PrepareState (IdenResolvedKey resolved)) ->
    SIR.SplitIdentifier resolved Unprepared ->
    PrepareState (SIR.SplitIdentifier resolved Prepared)
prepare_split_iden new_key (SIR.SplitIdentifier'Get texpr next (Const ())) = do
    texpr <- prepare_type_expr texpr
    resolved_key <- new_key (ResolveGet (SIR.type_expr_evaled texpr) next)
    pure $ SIR.SplitIdentifier'Get texpr next resolved_key
prepare_split_iden new_key (SIR.SplitIdentifier'Single name_maps i (Const ())) = SIR.SplitIdentifier'Single name_maps i <$> new_key (ResolveRoot name_maps i)
