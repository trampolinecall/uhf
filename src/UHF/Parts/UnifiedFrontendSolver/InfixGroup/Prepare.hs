module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Prepare (prepare) where

import UHF.Prelude

import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedArena, InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey, TypeExprEvaledAsTypeKey, TypeExprEvaledKey)
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena

type Unprepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), ())
type Prepared = (NameMaps.NameContextKey, IdenResolvedKey (), TypeWithInferVar.Type, TypeExprEvaledKey, TypeExprEvaledAsTypeKey, (), InfixGroupedKey)

type PrepareState = WriterT [InfixGroupTask] (State InfixGroupedArena)

new_infix_grouped_key :: (InfixGroupedKey -> InfixGroupTask) -> PrepareState InfixGroupedKey
new_infix_grouped_key task = do
    key <- state $ \arena -> let (key, arena') = Arena.put (Inconclusive ()) arena in (key, arena')
    tell [task key]
    pure key

prepare :: SIR.SIR Unprepared -> (SIR.SIR Prepared, InfixGroupedArena, [InfixGroupTask])
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
                Arena.new
    in (sir, arenas, tasks)

prepare_mod :: SIR.Module Unprepared -> PrepareState (SIR.Module Prepared)
prepare_mod (SIR.Module id name_map bindings adts type_synonyms) = SIR.Module id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms

prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (SIR.ADT id name type_vars variants) = SIR.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (SIR.ADTVariant'Named name id fields) =
            SIR.ADTVariant'Named name id
                <$> mapM
                    (\(id, name, ty, as_type) -> prepare_type_expr ty >>= \ty -> pure (id, name, ty, as_type))
                    -- (\(id, name, ty) -> prepare_type_expr ty >>= \ty -> pure (id, name, ty))
                    fields
        prepare_variant (SIR.ADTVariant'Anon name id fields) =
            SIR.ADTVariant'Anon name id
                <$> mapM (\(id, ty, as_type) -> prepare_type_expr ty >>= \ty -> pure (id, ty, as_type)) fields

prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (SIR.TypeSynonym id name expansion ex_as_type) = do
    expansion <- prepare_type_expr expansion
    pure $ SIR.TypeSynonym id name expansion ex_as_type

prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable varid tyinfo n) = pure $ SIR.Variable varid tyinfo n

prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding target eq_sp expr) = SIR.Binding <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr

prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer evaled resolved sp name_maps id) = pure $ SIR.TypeExpr'Refer evaled resolved sp name_maps id
prepare_type_expr (SIR.TypeExpr'Get evaled resolved sp parent name) = do
    parent <- prepare_type_expr parent
    pure $ SIR.TypeExpr'Get evaled resolved sp parent name
prepare_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = do
    a <- prepare_type_expr a
    b <- prepare_type_expr b
    pure $ SIR.TypeExpr'Tuple evaled sp a b
prepare_type_expr (SIR.TypeExpr'Hole evaled evaled_as_type sp hid) = pure $ SIR.TypeExpr'Hole evaled evaled_as_type sp hid
prepare_type_expr (SIR.TypeExpr'Function evaled sp arg res) = do
    arg <- prepare_type_expr arg
    res <- prepare_type_expr res

    pure $ SIR.TypeExpr'Function evaled sp arg res
prepare_type_expr (SIR.TypeExpr'Forall evaled sp name_maps vars ty) = do
    ty <- prepare_type_expr ty

    pure $ SIR.TypeExpr'Forall evaled sp name_maps vars ty
prepare_type_expr (SIR.TypeExpr'Apply evaled sp ty args) = do
    ty <- prepare_type_expr ty
    args <- prepare_type_expr args

    pure $ SIR.TypeExpr'Apply evaled sp ty args
prepare_type_expr (SIR.TypeExpr'Wild evaled sp) = pure $ SIR.TypeExpr'Wild evaled sp
prepare_type_expr (SIR.TypeExpr'Poison evaled sp) = pure $ SIR.TypeExpr'Poison evaled sp

prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
prepare_expr (SIR.Expr'Refer id type_info sp iden) = SIR.Expr'Refer id type_info sp <$> prepare_split_iden iden
prepare_expr (SIR.Expr'Char id type_info sp c) = pure $ SIR.Expr'Char id type_info sp c
prepare_expr (SIR.Expr'String id type_info sp s) = pure $ SIR.Expr'String id type_info sp s
prepare_expr (SIR.Expr'Int id type_info sp i) = pure $ SIR.Expr'Int id type_info sp i
prepare_expr (SIR.Expr'Float id type_info sp f) = pure $ SIR.Expr'Float id type_info sp f
prepare_expr (SIR.Expr'Bool id type_info sp b) = pure $ SIR.Expr'Bool id type_info sp b
prepare_expr (SIR.Expr'Tuple id type_info sp a b) = SIR.Expr'Tuple id type_info sp <$> prepare_expr a <*> prepare_expr b
prepare_expr (SIR.Expr'Lambda id type_info sp param body) = SIR.Expr'Lambda id type_info sp <$> prepare_pat param <*> prepare_expr body
prepare_expr (SIR.Expr'Let id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'LetRec id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'BinaryOps id () type_info sp first ops) = do
    first <- prepare_expr first
    ops <- mapM (\(sp, iden, rhs) -> (sp,,) <$> prepare_split_iden iden <*> prepare_expr rhs) ops

    infix_group_key <- new_infix_grouped_key $ InfixGroupTask (map (\(_, iden, _) -> SIR.split_identifier_resolved iden) ops)

    pure $ SIR.Expr'BinaryOps id infix_group_key type_info sp first ops
prepare_expr (SIR.Expr'Call id type_info sp callee arg) = SIR.Expr'Call id type_info sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If id type_info sp if_sp cond t f) = SIR.Expr'If id type_info sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match id type_info sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation id type_info sp (ty, ty_evaled_as_type) e) = do
    ty <- prepare_type_expr ty
    e <- prepare_expr e
    pure $ SIR.Expr'TypeAnnotation id type_info sp (ty, ty_evaled_as_type) e
prepare_expr (SIR.Expr'Forall id type_info sp ncs vars e) = SIR.Expr'Forall id type_info sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply id type_info sp e (arg, arg_evaled_as_type)) = do
    arg <- prepare_type_expr arg
    e <- prepare_expr e
    pure $ SIR.Expr'TypeApply id type_info sp e (arg, arg_evaled_as_type)
prepare_expr (SIR.Expr'Hole id type_info sp hid) = pure $ SIR.Expr'Hole id type_info sp hid
prepare_expr (SIR.Expr'Poison id type_info sp) = pure $ SIR.Expr'Poison id type_info sp

prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable type_info sp bnk) = pure $ SIR.Pattern'Variable type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard type_info sp) = pure $ SIR.Pattern'Wildcard type_info sp
prepare_pat (SIR.Pattern'Tuple type_info sp a b) = SIR.Pattern'Tuple type_info sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named type_info sp at_sp bnk subpat) = SIR.Pattern'Named type_info sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant type_info sp
        <$> prepare_split_iden variant_iden
        <*> pure tyargs
        <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant
        type_info
        sp
        <$> prepare_split_iden variant_iden
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison type_info sp) = pure $ SIR.Pattern'Poison type_info sp

prepare_split_iden ::
    SIR.SplitIdentifier resolved Unprepared ->
    PrepareState (SIR.SplitIdentifier resolved Prepared)
prepare_split_iden (SIR.SplitIdentifier'Get texpr next resolved) = do
    texpr <- prepare_type_expr texpr
    pure $ SIR.SplitIdentifier'Get texpr next resolved
prepare_split_iden (SIR.SplitIdentifier'Single name_maps i resolved) = pure $ SIR.SplitIdentifier'Single name_maps i resolved
