{-# LANGUAGE DataKinds #-}

module UHF.Parts.UnifiedFrontendSolver.NameResolve.Prepare (prepare) where

import UHF.Prelude

import Data.Functor.Const (Const)
import qualified Data.Map as Map
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.SIR as SIR
import qualified UHF.Data.SIR.ID as SIR.ID
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result
    ( DeclIdenResults
    , TypeExprsEvaled
    , TypeExprsEvaledAsTypes
    , ValueIdenResults
    , VariantIdenResults
    )
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Task (IdenResolveTask (..), TypeExprEvalAsTypeTask (..), TypeExprEvalTask (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import UHF.Source.Located (Located (Located))
import qualified UHF.Util.Arena as Arena

type Unprepared = ((), Const () (), (), (), (), (), ())
type Prepared = ((), Const () (), (), (), (), (), ())

-- TODO: make someday this will turn into a RWST?
type PrepareState =
    ReaderT
        ( Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey
        , Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey
        )
        ( WriterT
            ( [IdenResolveTask (SIR.ID.ID "DeclIden")]
            , [IdenResolveTask (SIR.ID.ID "ValueIden")]
            , [IdenResolveTask (SIR.ID.ID "VariantIden")]
            , [TypeExprEvalTask]
            , [TypeExprEvalAsTypeTask]
            )
            ( State
                ( DeclIdenResults
                , ValueIdenResults
                , VariantIdenResults
                , TypeExprsEvaled
                , TypeExprsEvaledAsTypes
                )
            )
        )

new_decl_iden_resolved_key :: SIR.ID.ID "DeclIden" -> (SIR.ID.ID "DeclIden" -> IdenResolveTask (SIR.ID.ID "DeclIden")) -> PrepareState ()
new_decl_iden_resolved_key id make_task = do
    state $ \(decls, vals, variants, tees, teeats) -> let decls' = Map.insert id (Inconclusive Nothing) decls in ((), (decls', vals, variants, tees, teeats))
    writer ((), ([make_task id], [], [], [], []))

new_val_iden_resolved_key :: SIR.ID.ID "ValueIden" -> (SIR.ID.ID "ValueIden" -> IdenResolveTask (SIR.ID.ID "ValueIden")) -> PrepareState ()
new_val_iden_resolved_key id make_task = do
    state $ \(decls, vals, variants, tees, teeats) -> let vals' = Map.insert id (Inconclusive Nothing) vals in ((), (decls, vals', variants, tees, teeats))
    writer ((), ([], [make_task id], [], [], []))

new_variant_iden_resolved_key :: SIR.ID.ID "VariantIden" -> (SIR.ID.ID "VariantIden" -> IdenResolveTask (SIR.ID.ID "VariantIden")) -> PrepareState ()
new_variant_iden_resolved_key id make_task = do
    state $ \(decls, vals, variants, tees, teeats) -> let variants' = Map.insert id (Inconclusive Nothing) variants in ((), (decls, vals, variants', tees, teeats))
    writer ((), ([], [], [make_task id], [], []))

new_type_expr_evaled_key :: SIR.ID.ID "TypeExpr" -> (SIR.ID.ID "TypeExpr" -> TypeExprEvalTask) -> PrepareState ()
new_type_expr_evaled_key id make_task = do
    state $ \(decls, vals, variants, tees, teeats) -> let tees' = Map.insert id (Inconclusive Nothing) tees in ((), (decls, vals, variants, tees', teeats))
    writer ((), ([], [], [], [make_task id], []))

new_type_expr_evaled_as_type_key ::
    SIR.ID.ID "TypeExprEvaledAsType" -> (SIR.ID.ID "TypeExprEvaledAsType" -> TypeExprEvalAsTypeTask) -> PrepareState ()
new_type_expr_evaled_as_type_key id make_task = do
    state $ \(decls, vals, variants, tees, teeats) -> let teeats' = Map.insert id (Inconclusive Nothing) teeats in ((), (decls, vals, variants, tees, teeats'))
    writer ((), ([], [], [], [], [make_task id]))

prepare ::
    Map (SIR.ID.ID "HasChildNameContext") NameMaps.NameContextKey ->
    Map (SIR.ID.ID "HasEnclosingNameContext") NameMaps.NameContextKey ->
    SIR.SIR Unprepared ->
    ( SIR.SIR Prepared
    , ( DeclIdenResults
      , ValueIdenResults
      , VariantIdenResults
      , TypeExprsEvaled
      , TypeExprsEvaledAsTypes
      )
    , ( [IdenResolveTask (SIR.ID.ID "DeclIden")]
      , [IdenResolveTask (SIR.ID.ID "ValueIden")]
      , [IdenResolveTask (SIR.ID.ID "VariantIden")]
      , [TypeExprEvalTask]
      , [TypeExprEvalAsTypeTask]
      )
    )
prepare hcncid_map hencid_map (SIR.SIR mods adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    let ((sir, tasks), arenas) =
            runState
                ( runWriterT $
                    runReaderT
                        ( SIR.SIR
                            <$> Arena.transformM prepare_mod mods
                            <*> Arena.transformM prepare_adt adts
                            <*> Arena.transformM prepare_type_synonym type_synonyms
                            <*> pure type_vars
                            <*> Arena.transformM prepare_variable variables
                            <*> pure (SIR.CU root_module main_function)
                        )
                        (hcncid_map, hencid_map)
                )
                (Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)
    in (sir, arenas, tasks)
prepare_mod :: SIR.Module Unprepared -> PrepareState (SIR.Module Prepared)
prepare_mod (SIR.Module mid id name_map bindings adts type_synonyms) = SIR.Module mid id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms -- TODO: rename mid to id
prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    ( \(iden, name, (ty, teeatid)) -> do
                        ty <- prepare_type_expr ty
                        new_type_expr_evaled_as_type_key teeatid (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
                        pure (iden, name, (ty, teeatid))
                    )
                    fields
        prepare_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM
                    ( \(iden, (ty, teeatid)) -> do
                        ty <- prepare_type_expr ty
                        new_type_expr_evaled_as_type_key teeatid (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
                        pure (iden, (ty, teeatid))
                    )
                    fields
prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (Type.TypeSynonym id name (expansion, teeatid)) = do
    expansion <- prepare_type_expr expansion
    new_type_expr_evaled_as_type_key teeatid (EvalAsType $ Located (SIR.type_expr_span expansion) (SIR.type_expr_evaled expansion))
    pure $ Type.TypeSynonym id name (expansion, teeatid)
prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable id varid n) = pure $ SIR.Variable id varid n
prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding id target eq_sp expr) = SIR.Binding id <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr
prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer id nrid hencid sp iden) = do
    (_, hencid_map) <- ask
    new_decl_iden_resolved_key nrid $ ResolveRoot (hencid_map Map.! hencid) iden
    new_type_expr_evaled_key id $ GetFromDeclIdenResolved nrid
    pure $ SIR.TypeExpr'Refer id nrid hencid sp iden
prepare_type_expr (SIR.TypeExpr'Get id nrid sp parent name) = do
    parent <- prepare_type_expr parent
    new_decl_iden_resolved_key nrid $ ResolveGet (SIR.type_expr_evaled parent) name
    new_type_expr_evaled_key id $ GetFromDeclIdenResolved nrid
    pure $ SIR.TypeExpr'Get id nrid sp parent name
prepare_type_expr (SIR.TypeExpr'Tuple id sp a b) = do
    a <- prepare_type_expr a
    b <- prepare_type_expr b
    new_type_expr_evaled_key id $
        MakeTuple (Located (SIR.type_expr_span a) (SIR.type_expr_evaled a)) (Located (SIR.type_expr_span b) (SIR.type_expr_evaled b))
    pure $ SIR.TypeExpr'Tuple id sp a b
prepare_type_expr (SIR.TypeExpr'Hole id hid sp hiden) = do
    new_type_expr_evaled_key id (MakeInferVar sp)
    new_type_expr_evaled_as_type_key hid (EvalAsType $ Located sp id)
    pure $ SIR.TypeExpr'Hole id hid sp hiden
prepare_type_expr (SIR.TypeExpr'Function id sp arg res) = do
    arg <- prepare_type_expr arg
    res <- prepare_type_expr res
    new_type_expr_evaled_key id $
        MakeFunction (Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg)) (Located (SIR.type_expr_span res) (SIR.type_expr_evaled res))
    pure $ SIR.TypeExpr'Function id sp arg res
prepare_type_expr (SIR.TypeExpr'Forall id sp name_maps vars ty) = do
    ty <- prepare_type_expr ty
    new_type_expr_evaled_key id $ MakeForall vars (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
    pure $ SIR.TypeExpr'Forall id sp name_maps vars ty
prepare_type_expr (SIR.TypeExpr'Apply id sp ty args) = do
    ty <- prepare_type_expr ty
    args <- prepare_type_expr args
    new_type_expr_evaled_key id $
        MakeApply sp (Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty)) (Located (SIR.type_expr_span args) (SIR.type_expr_evaled args))
    pure $ SIR.TypeExpr'Apply id sp ty args
prepare_type_expr (SIR.TypeExpr'Wild id sp) = do
    new_type_expr_evaled_key id $ MakeInferVar sp
    pure $ SIR.TypeExpr'Wild id sp
prepare_type_expr (SIR.TypeExpr'Poison id sp) = do
    new_type_expr_evaled_key id $ MakeInferVar sp
    pure $ SIR.TypeExpr'Poison id sp
prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
-- TODO: rename all eid to id
prepare_expr (SIR.Expr'Refer eid id sp iden) = SIR.Expr'Refer eid id sp <$> prepare_split_iden new_val_iden_resolved_key iden
prepare_expr (SIR.Expr'Char eid id sp c) = pure $ SIR.Expr'Char eid id sp c
prepare_expr (SIR.Expr'String eid id sp s) = pure $ SIR.Expr'String eid id sp s
prepare_expr (SIR.Expr'Int eid id sp i) = pure $ SIR.Expr'Int eid id sp i
prepare_expr (SIR.Expr'Float eid id sp f) = pure $ SIR.Expr'Float eid id sp f
prepare_expr (SIR.Expr'Bool eid id sp b) = pure $ SIR.Expr'Bool eid id sp b
prepare_expr (SIR.Expr'Tuple eid id sp a b) = SIR.Expr'Tuple eid id sp <$> prepare_expr a <*> prepare_expr b
prepare_expr (SIR.Expr'Lambda eid id sp param body) = SIR.Expr'Lambda eid id sp <$> prepare_pat param <*> prepare_expr body
prepare_expr (SIR.Expr'Let eid id sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let eid id sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'LetRec eid id sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec eid id sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'BinaryOps eid id allowed sp first ops) =
    SIR.Expr'BinaryOps eid id allowed sp
        <$> prepare_expr first
        <*> mapM
            (\(sp, iden, rhs) -> (sp,,) <$> prepare_split_iden new_val_iden_resolved_key iden <*> prepare_expr rhs)
            ops
prepare_expr (SIR.Expr'Call eid id sp callee arg) = SIR.Expr'Call eid id sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If eid id sp if_sp cond t f) = SIR.Expr'If eid id sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match eid id sp match_tok_sp e arms) =
    SIR.Expr'Match eid id sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation eid id sp (ty, teeatid) e) = do
    ty <- prepare_type_expr ty
    new_type_expr_evaled_as_type_key teeatid (EvalAsType $ Located (SIR.type_expr_span ty) (SIR.type_expr_evaled ty))
    SIR.Expr'TypeAnnotation eid id sp (ty, teeatid) <$> prepare_expr e
prepare_expr (SIR.Expr'Forall eid id sp ncs vars e) = SIR.Expr'Forall eid id sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply eid id sp e (arg, aeeatid)) = do
    arg <- prepare_type_expr arg
    new_type_expr_evaled_as_type_key aeeatid (EvalAsType $ Located (SIR.type_expr_span arg) (SIR.type_expr_evaled arg))
    SIR.Expr'TypeApply eid id sp <$> prepare_expr e <*> pure (arg, aeeatid)
prepare_expr (SIR.Expr'Hole eid id sp hid) = pure $ SIR.Expr'Hole eid id sp hid
prepare_expr (SIR.Expr'Poison eid id sp) = pure $ SIR.Expr'Poison eid id sp
prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable id sp bnk) = pure $ SIR.Pattern'Variable id sp bnk
prepare_pat (SIR.Pattern'Wildcard id sp) = pure $ SIR.Pattern'Wildcard id sp
prepare_pat (SIR.Pattern'Tuple id sp a b) = SIR.Pattern'Tuple id sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named id sp at_sp bnk subpat) = SIR.Pattern'Named id sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant id variant_id sp variant_iden subpat) = SIR.Pattern'AnonADTVariant id variant_id sp <$> prepare_split_iden new_variant_iden_resolved_key variant_iden <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant id variant_id sp variant_iden subpat) =
    SIR.Pattern'NamedADTVariant id variant_id sp
        <$> prepare_split_iden new_variant_iden_resolved_key variant_iden
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison id sp) = pure $ SIR.Pattern'Poison id sp
prepare_split_iden ::
    (SIR.ID.ID id_name -> (SIR.ID.ID id_name -> IdenResolveTask (SIR.ID.ID id_name)) -> PrepareState ()) ->
    SIR.SplitIdentifier id_name Unprepared ->
    PrepareState (SIR.SplitIdentifier id_name Prepared)
prepare_split_iden new_key (SIR.SplitIdentifier'Get id texpr next) = do
    texpr <- prepare_type_expr texpr
    new_key id (ResolveGet (SIR.type_expr_evaled texpr) next)
    pure $ SIR.SplitIdentifier'Get id texpr next
prepare_split_iden new_key (SIR.SplitIdentifier'Single id hencid i) = do
    (_, hencid_map) <- ask
    new_key id (ResolveRoot (hencid_map Map.! hencid) i)
    pure $ SIR.SplitIdentifier'Single id hencid i
