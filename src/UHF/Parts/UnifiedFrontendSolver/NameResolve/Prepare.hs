module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

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
import UHF.Source.Located (Located (Located))
import qualified UHF.Util.Arena as Arena
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Refs (ValueRef, DeclRef)
import Data.Functor.Const (Const)

type Unprepared = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())
type Prepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())

-- TODO: make someday this will turn into a RWST?
type PrepareState =
    WriterT
        ( [IdenResolveTask (DeclRef TypeWithInferVar.Type)]
        , [IdenResolveTask ValueRef]
        , [IdenResolveTask Type.ADT.VariantIndex]
        , [TypeExprEvalTask]
        , [TypeExprEvalAsTypeTask]
        )
        ( State
            ( IdenResolvedArena (DeclRef TypeWithInferVar.Type)
            , IdenResolvedArena ValueRef
            , IdenResolvedArena Type.ADT.VariantIndex
            , TypeExprEvaledArena
            , TypeExprEvaledAsTypeArena
            )
        )

new_decl_iden_resolved_key ::
    (IdenResolvedKey (DeclRef TypeWithInferVar.Type) -> IdenResolveTask (DeclRef TypeWithInferVar.Type)) ->
    PrepareState (IdenResolvedKey (DeclRef TypeWithInferVar.Type))
new_decl_iden_resolved_key make_task = do
    key <- state $ \(decls, vals, variants, tees, teeats) -> let (key, decls') = Arena.put (Inconclusive Nothing) decls in (key, (decls', vals, variants, tees, teeats))
    writer ((), ([make_task key], [], [], [], []))
    pure key

new_val_iden_resolved_key :: (IdenResolvedKey ValueRef -> IdenResolveTask ValueRef) -> PrepareState (IdenResolvedKey ValueRef)
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
    , ( IdenResolvedArena (DeclRef TypeWithInferVar.Type)
      , IdenResolvedArena ValueRef
      , IdenResolvedArena Type.ADT.VariantIndex
      , TypeExprEvaledArena
      , TypeExprEvaledAsTypeArena
      )
    , ( [IdenResolveTask (DeclRef TypeWithInferVar.Type)]
      , [IdenResolveTask ValueRef]
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
prepare_mod (SIR.Module mid id name_map bindings adts type_synonyms) = SIR.Module mid id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms -- TODO: rename mid to id
prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    ( \(id, name, (ty, ())) ->
                        prepare_type_expr ty >>= \ty ->
                            new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) >>= \as_type -> pure (id, name, (ty, as_type))
                    )
                    fields
        prepare_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM
                    ( \(id, (ty, ())) ->
                        prepare_type_expr ty >>= \ty -> new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) >>= \as_type -> pure (id, (ty, as_type))
                    )
                    fields
prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (Type.TypeSynonym id name (expansion, ())) = do
    expansion <- prepare_type_expr expansion
    as_type <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span expansion) (SIR.type_expr_evaled expansion))
    pure $ Type.TypeSynonym id name (expansion, as_type)
prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable id varid tyinfo n) = pure $ SIR.Variable id varid tyinfo n
prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding id target eq_sp expr) = SIR.Binding id <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr
prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer id () sp name_maps iden) = do
    resolved_key <- new_decl_iden_resolved_key $ ResolveRoot name_maps iden
    evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
    pure $ SIR.TypeExpr'Refer id evaled_key sp name_maps iden
prepare_type_expr (SIR.TypeExpr'Get id () sp parent name) = do
    parent <- prepare_type_expr parent
    resolved_key <- new_decl_iden_resolved_key $ ResolveGet (SIR.type_expr_evaled parent) name
    evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
    pure $ SIR.TypeExpr'Get id evaled_key sp parent name
prepare_type_expr (SIR.TypeExpr'Tuple id () sp a b) = do
    a <- prepare_type_expr a
    b <- prepare_type_expr b
    evaled_key <-
        new_type_expr_evaled_key $
            MakeTuple (Located (SIR.type_expr_span a) (SIR.type_expr_evaled a)) (Located (SIR.type_expr_span b) (SIR.type_expr_evaled b))
    pure $ SIR.TypeExpr'Tuple id evaled_key sp a b
prepare_type_expr (SIR.TypeExpr'Hole id () () sp hid) = do
    evaled_key <- new_type_expr_evaled_key (MakeInferVar sp)
    evaled_as_type_key <- new_type_expr_evaled_as_type_key (EvalAsType $ Located sp evaled_key)
    pure $ SIR.TypeExpr'Hole id evaled_key evaled_as_type_key sp hid
prepare_type_expr (SIR.TypeExpr'Function id () sp arg res) = do
    arg <- prepare_type_expr arg
    res <- prepare_type_expr res
    evaled_key <-
        new_type_expr_evaled_key $
            MakeFunction (Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg)) (Located (SIR.type_expr_span res) (SIR.type_expr_evaled res))
    pure $ SIR.TypeExpr'Function id evaled_key sp arg res
prepare_type_expr (SIR.TypeExpr'Forall id () sp name_maps vars ty) = do
    ty <- prepare_type_expr ty
    evaled_key <- new_type_expr_evaled_key $ MakeForall vars (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
    pure $ SIR.TypeExpr'Forall id evaled_key sp name_maps vars ty
prepare_type_expr (SIR.TypeExpr'Apply id () sp ty args) = do
    ty <- prepare_type_expr ty
    args <- prepare_type_expr args
    evaled_key <-
        new_type_expr_evaled_key $
            MakeApply sp (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) (Located (SIR.type_expr_span args) (SIR.type_expr_evaled args))
    pure $ SIR.TypeExpr'Apply id evaled_key sp ty args
prepare_type_expr (SIR.TypeExpr'Wild id () sp) = do
    evaled_key <- new_type_expr_evaled_key $ MakeInferVar sp
    pure $ SIR.TypeExpr'Wild id evaled_key sp
prepare_type_expr (SIR.TypeExpr'Poison id () sp) = do
    evaled_key <- new_type_expr_evaled_key $ MakeInferVar sp
    pure $ SIR.TypeExpr'Poison id evaled_key sp
prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
-- TODO: rename all eid to id
prepare_expr (SIR.Expr'Refer eid id type_info sp iden) = SIR.Expr'Refer eid id type_info sp <$> prepare_split_iden new_val_iden_resolved_key iden
prepare_expr (SIR.Expr'Char eid id type_info sp c) = pure $ SIR.Expr'Char eid id type_info sp c
prepare_expr (SIR.Expr'String eid id type_info sp s) = pure $ SIR.Expr'String eid id type_info sp s
prepare_expr (SIR.Expr'Int eid id type_info sp i) = pure $ SIR.Expr'Int eid id type_info sp i
prepare_expr (SIR.Expr'Float eid id type_info sp f) = pure $ SIR.Expr'Float eid id type_info sp f
prepare_expr (SIR.Expr'Bool eid id type_info sp b) = pure $ SIR.Expr'Bool eid id type_info sp b
prepare_expr (SIR.Expr'Tuple eid id type_info sp a b) = SIR.Expr'Tuple eid id type_info sp <$> prepare_expr a <*> prepare_expr b
prepare_expr (SIR.Expr'Lambda eid id type_info sp param body) = SIR.Expr'Lambda eid id type_info sp <$> prepare_pat param <*> prepare_expr body
prepare_expr (SIR.Expr'Let eid id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let eid id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'LetRec eid id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec eid id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'BinaryOps eid id allowed type_info sp first ops) =
    SIR.Expr'BinaryOps eid id allowed type_info sp
        <$> prepare_expr first
        <*> mapM
            (\(sp, iden, rhs) -> (sp,,) <$> prepare_split_iden new_val_iden_resolved_key iden <*> prepare_expr rhs)
            ops
prepare_expr (SIR.Expr'Call eid id type_info sp callee arg) = SIR.Expr'Call eid id type_info sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If eid id type_info sp if_sp cond t f) = SIR.Expr'If eid id type_info sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match eid id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match eid id type_info sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation eid id type_info sp (ty, ()) e) =
    prepare_type_expr ty >>= \ty ->
        new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) >>= \ty_resolved -> SIR.Expr'TypeAnnotation eid id type_info sp (ty, ty_resolved) <$> prepare_expr e
prepare_expr (SIR.Expr'Forall eid id type_info sp ncs vars e) = SIR.Expr'Forall eid id type_info sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply eid id type_info sp e (arg, ())) =
    prepare_type_expr arg >>= \arg ->
        new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg)) >>= \arg_resolved -> SIR.Expr'TypeApply eid id type_info sp <$> prepare_expr e <*> pure (arg, arg_resolved)
prepare_expr (SIR.Expr'Hole eid id type_info sp hid) = pure $ SIR.Expr'Hole eid id type_info sp hid
prepare_expr (SIR.Expr'Poison eid id type_info sp) = pure $ SIR.Expr'Poison eid id type_info sp
prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable id type_info sp bnk) = pure $ SIR.Pattern'Variable id type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard id type_info sp) = pure $ SIR.Pattern'Wildcard id type_info sp
prepare_pat (SIR.Pattern'Tuple id type_info sp a b) = SIR.Pattern'Tuple id type_info sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named id type_info sp at_sp bnk subpat) = SIR.Pattern'Named id type_info sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant id type_info sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> pure tyargs
        <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant
        id
        type_info
        sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison id type_info sp) = pure $ SIR.Pattern'Poison id type_info sp
prepare_split_iden ::
    ((IdenResolvedKey result -> IdenResolveTask result) -> PrepareState (IdenResolvedKey resolved)) ->
    SIR.SplitIdentifier Unprepared ->
    PrepareState (SIR.SplitIdentifier Prepared)
prepare_split_iden new_key (SIR.SplitIdentifier'Get id texpr next) = do
    texpr <- prepare_type_expr texpr
    resolved_key <- new_key (ResolveGet (SIR.type_expr_evaled texpr) next)
    pure $ SIR.SplitIdentifier'Get id texpr next
prepare_split_iden new_key (SIR.SplitIdentifier'Single id name_maps i) =
    pure $ SIR.SplitIdentifier'Single id name_maps i -- TODO: <$> new_key (ResolveRoot name_maps i)
