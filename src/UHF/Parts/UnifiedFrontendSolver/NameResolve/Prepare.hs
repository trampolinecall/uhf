{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

import Data.Functor.Const (Const (Const))
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedArena, IdenResolvedKey, TypeExprEvaledArena, TypeExprEvaledAsTypeArena, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalAsTypeTask (..), TypeExprEvalTask (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import UHF.Source.Located (Located (Located))
import qualified UHF.Util.Arena as Arena
import Data.Data (Proxy (..))
import UHF.Data.SIR.Visitor

type Unprepared = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())
type Prepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())

-- TODO: make someday this will turn into a RWST?
type PrepareState =
    WriterT
        ([IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)], [IdenResolveTask SIR.ValueRef], [IdenResolveTask Type.ADT.VariantIndex], [TypeExprEvalTask], [TypeExprEvalAsTypeTask])
        (State (IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type), IdenResolvedArena SIR.ValueRef, IdenResolvedArena Type.ADT.VariantIndex, TypeExprEvaledArena, TypeExprEvaledAsTypeArena))

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
    , (IdenResolvedArena (SIR.DeclRef TypeWithInferVar.Type), IdenResolvedArena SIR.ValueRef, IdenResolvedArena Type.ADT.VariantIndex, TypeExprEvaledArena, TypeExprEvaledAsTypeArena)
    , ([IdenResolveTask (SIR.DeclRef TypeWithInferVar.Type)], [IdenResolveTask SIR.ValueRef], [IdenResolveTask Type.ADT.VariantIndex], [TypeExprEvalTask], [TypeExprEvalAsTypeTask])
    )
prepare sir =
    let ((sir', tasks), arenas) = runState (runWriterT (visit_sir' (Proxy :: Proxy PrepareVisitor) sir)) (Arena.new, Arena.new, Arena.new, Arena.new, Arena.new)
    in (sir', arenas, tasks)

data PrepareVisitor

instance TransformsNameMapIndex Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsTypeInRefer Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsTypeInfo Unprepared Prepared () PrepareState PrepareVisitor where
instance TransformsInfixGroupedKey Unprepared Prepared () PrepareState PrepareVisitor where

instance SIRVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance CUVisitor Unprepared Prepared () PrepareState () PrepareVisitor where

instance ADTVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_adt_variant_named proxy () name id fields = do
        fields <- mapM
                    ( \(id, name, (ty, ())) -> do
                        ty <- visit_type_expr' proxy ty
                        as_type_k <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
                        pure (id, name, (ty, as_type_k))
                    )
                    fields

        pure (Type.ADT.Variant'Named name id fields, ())

    visit_adt_variant_anon proxy () name id fields = do
        fields <- mapM
            (\(id, (ty, ())) -> do
                ty <- visit_type_expr' proxy ty
                as_type_k <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
                pure (id, (ty, as_type_k)))
            fields
        pure (Type.ADT.Variant'Anon name id fields, ())

instance TypeSynonymVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_type_synonym proxy () (Type.TypeSynonym id name (expansion, ())) = do
        expansion <- visit_type_expr' proxy expansion
        as_type_k <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span expansion) (SIR.type_expr_evaled expansion))
        pure (Type.TypeSynonym id name (expansion, as_type_k), ())

instance ModuleVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance VariableVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
instance BindingVisitor Unprepared Prepared () PrepareState () PrepareVisitor where

instance TypeExprVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_type_expr_refer Proxy () () (Const ()) sp name_maps id = do
        resolved_key <- new_decl_iden_resolved_key $ ResolveRoot name_maps id
        evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
        pure (SIR.TypeExpr'Refer evaled_key resolved_key sp name_maps id, ())

    visit_type_expr_get proxy () () (Const ()) sp parent name = do
        parent <- visit_type_expr' proxy parent
        resolved_key <- new_decl_iden_resolved_key $ ResolveGet (SIR.type_expr_evaled parent) name
        evaled_key <- new_type_expr_evaled_key $ GetFromDeclIdenResolved resolved_key
        pure (SIR.TypeExpr'Get evaled_key resolved_key sp parent name, ())

    visit_type_expr_tuple proxy () () sp a b = do
        a <- visit_type_expr' proxy a
        b <- visit_type_expr' proxy b

        evaled_key <- new_type_expr_evaled_key $ MakeTuple (Located (SIR.type_expr_span a) (SIR.type_expr_evaled a)) (Located (SIR.type_expr_span b) (SIR.type_expr_evaled b))

        pure (SIR.TypeExpr'Tuple evaled_key sp a b, ())

    visit_type_expr_hole Proxy () () () sp hid = do
        evaled_key <- new_type_expr_evaled_key (MakeInferVar sp)
        evaled_as_type_key <- new_type_expr_evaled_as_type_key (EvalAsType $ Located sp evaled_key)

        pure (SIR.TypeExpr'Hole evaled_key evaled_as_type_key sp hid, ())

    visit_type_expr_function proxy () () sp arg res = do
        arg <- visit_type_expr' proxy arg
        res <- visit_type_expr' proxy res

        evaled_key <- new_type_expr_evaled_key $ MakeFunction (Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg)) (Located (SIR.type_expr_span res) (SIR.type_expr_evaled res))

        pure (SIR.TypeExpr'Function evaled_key sp arg res, ())

    visit_type_expr_forall proxy () () sp name_maps vars ty = do
        ty <- visit_type_expr' proxy ty

        evaled_key <- new_type_expr_evaled_key $ MakeForall vars (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))

        pure (SIR.TypeExpr'Forall evaled_key sp name_maps vars ty, ())

    visit_type_expr_apply proxy () () sp ty arg = do
        ty <- visit_type_expr' proxy ty
        arg <- visit_type_expr' proxy arg

        evaled_key <- new_type_expr_evaled_key $ MakeApply sp (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) (Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg))

        pure (SIR.TypeExpr'Apply evaled_key sp ty arg, ())

    visit_type_expr_wild Proxy () () sp = do
        evaled_key <- new_type_expr_evaled_key $ MakeInferVar sp
        pure (SIR.TypeExpr'Wild evaled_key sp, ())

    visit_type_expr_poison Proxy () () sp = do
        evaled_key <- new_type_expr_evaled_key $ MakeInferVar sp
        pure (SIR.TypeExpr'Poison evaled_key sp, ())

instance ExprIdentifierRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_expr_identifier_ref_get Proxy () texpr next resolved_key  = (,()) <$> go_split_iden_get new_val_iden_resolved_key texpr next resolved_key
    visit_expr_identifier_ref_single Proxy () name_maps i resolved_key = (,()) <$> go_split_iden_single new_val_iden_resolved_key name_maps i resolved_key

instance OperatorRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_operator_ref_get Proxy () texpr next resolved_key = (,()) <$> go_split_iden_get new_val_iden_resolved_key texpr next resolved_key
    visit_operator_ref_single Proxy () name_maps i resolved_key = (,()) <$> go_split_iden_single new_val_iden_resolved_key name_maps i resolved_key

instance ExprVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_expr_type_annotation proxy () id type_info sp (ty, ()) e = do
        ty <- visit_type_expr' proxy ty
        ty_resolved_key <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))

        e <- visit_expr' proxy e

        pure (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_resolved_key) e, ())

    visit_expr_type_apply proxy () id type_info sp e (arg, ()) = do
        arg <- visit_type_expr' proxy arg
        arg_resolved_key <- new_type_expr_evaled_as_type_key (EvalAsType $ Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg))

        e <- visit_expr' proxy e

        pure (SIR.Expr'TypeApply id type_info sp e (arg, arg_resolved_key), ())

instance PatternADTVariantRefVisitor Unprepared Prepared () PrepareState () PrepareVisitor where
    visit_pattern_adt_variant_ref_get Proxy () texpr next resolved_key = (,()) <$> go_split_iden_get new_variant_iden_resolved_key texpr next resolved_key
    visit_pattern_adt_variant_ref_single Proxy () name_maps i resolved_key = (,()) <$> go_split_iden_single new_variant_iden_resolved_key name_maps i resolved_key

instance PatternVisitor Unprepared Prepared () PrepareState () PrepareVisitor where

go_split_iden_get :: ((IdenResolvedKey result -> IdenResolveTask result) -> PrepareState (IdenResolvedKey resolved)) -> SIR.TypeExpr Unprepared -> Located Text -> Const () resolved -> PrepareState (SIR.SplitIdentifier resolved Prepared)
go_split_iden_get make_key texpr next (Const ()) = do
    texpr <- visit_type_expr' (Proxy :: Proxy PrepareVisitor) texpr
    resolved_key <- make_key (ResolveGet (SIR.type_expr_evaled texpr) next)
    pure $ SIR.SplitIdentifier'Get texpr next resolved_key

go_split_iden_single :: ((IdenResolvedKey result -> IdenResolveTask result) -> PrepareState (IdenResolvedKey resolved)) -> NameMaps.NameContextKey -> Located Text -> Const () resolved -> PrepareState (SIR.SplitIdentifier resolved Prepared)
go_split_iden_single make_key name_maps i (Const ()) = SIR.SplitIdentifier'Single name_maps i <$> make_key (ResolveRoot name_maps i)
