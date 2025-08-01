module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Prepare (prepare) where

import UHF.Prelude

import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.Type.ADT as Type.ADT
import qualified UHF.Data.IR.TypeWithInferVar as TypeWithInferVar
import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupedArena, InfixGroupedKey)
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import qualified UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.NameMaps as NameMaps
import UHF.Parts.UnifiedFrontendSolver.SolveResult
import qualified UHF.Util.Arena as Arena
import Data.Functor.Const (Const)

type Unprepared = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), ())
type Prepared = (NameMaps.NameContextKey, Const () (), TypeWithInferVar.Type, (), (), (), InfixGroupedKey)

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
-- TODO: rename mid to id
prepare_mod (SIR.Module mid id name_map bindings adts type_synonyms) = SIR.Module mid id name_map <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms

prepare_adt :: SIR.ADT Unprepared -> PrepareState (SIR.ADT Prepared)
prepare_adt (Type.ADT id name type_vars variants) = Type.ADT id name type_vars <$> mapM prepare_variant variants
    where
        prepare_variant (Type.ADT.Variant'Named name id fields) =
            Type.ADT.Variant'Named name id
                <$> mapM
                    (\(id, name, (ty, as_type)) -> prepare_type_expr ty >>= \ty -> pure (id, name, (ty, as_type)))
                    -- (\(id, name, ty) -> prepare_type_expr ty >>= \ty -> pure (id, name, ty))
                    fields
        prepare_variant (Type.ADT.Variant'Anon name id fields) =
            Type.ADT.Variant'Anon name id
                <$> mapM (\(id, (ty, as_type)) -> prepare_type_expr ty >>= \ty -> pure (id, (ty, as_type))) fields

prepare_type_synonym :: SIR.TypeSynonym Unprepared -> PrepareState (SIR.TypeSynonym Prepared)
prepare_type_synonym (Type.TypeSynonym id name (expansion, ex_as_type)) = do
    expansion <- prepare_type_expr expansion
    pure $ Type.TypeSynonym id name (expansion, ex_as_type)

prepare_variable :: SIR.Variable Unprepared -> PrepareState (SIR.Variable Prepared)
prepare_variable (SIR.Variable id varid tyinfo n) = pure $ SIR.Variable id varid tyinfo n

prepare_binding :: SIR.Binding Unprepared -> PrepareState (SIR.Binding Prepared)
prepare_binding (SIR.Binding id target eq_sp expr) = SIR.Binding id <$> prepare_pat target <*> pure eq_sp <*> prepare_expr expr

prepare_type_expr :: SIR.TypeExpr Unprepared -> PrepareState (SIR.TypeExpr Prepared)
prepare_type_expr (SIR.TypeExpr'Refer id nrid sp name_maps iden) = pure $ SIR.TypeExpr'Refer id nrid sp name_maps iden
prepare_type_expr (SIR.TypeExpr'Get id nrid sp parent name) = do
    parent <- prepare_type_expr parent
    pure $ SIR.TypeExpr'Get id nrid sp parent name
prepare_type_expr (SIR.TypeExpr'Tuple id sp a b) = do
    a <- prepare_type_expr a
    b <- prepare_type_expr b
    pure $ SIR.TypeExpr'Tuple id sp a b
prepare_type_expr (SIR.TypeExpr'Hole id evaled_as_type sp hid) = pure $ SIR.TypeExpr'Hole id evaled_as_type sp hid
prepare_type_expr (SIR.TypeExpr'Function id sp arg res) = do
    arg <- prepare_type_expr arg
    res <- prepare_type_expr res

    pure $ SIR.TypeExpr'Function id sp arg res
prepare_type_expr (SIR.TypeExpr'Forall id sp name_maps vars ty) = do
    ty <- prepare_type_expr ty

    pure $ SIR.TypeExpr'Forall id sp name_maps vars ty
prepare_type_expr (SIR.TypeExpr'Apply id sp ty args) = do
    ty <- prepare_type_expr ty
    args <- prepare_type_expr args

    pure $ SIR.TypeExpr'Apply id sp ty args
prepare_type_expr (SIR.TypeExpr'Wild id sp) = pure $ SIR.TypeExpr'Wild id sp
prepare_type_expr (SIR.TypeExpr'Poison id sp) = pure $ SIR.TypeExpr'Poison id sp

prepare_expr :: SIR.Expr Unprepared -> PrepareState (SIR.Expr Prepared)
prepare_expr (SIR.Expr'Refer eid id type_info sp iden) = SIR.Expr'Refer eid id type_info sp <$> prepare_split_iden iden
prepare_expr (SIR.Expr'Char eid id type_info sp c) = pure $ SIR.Expr'Char eid id type_info sp c
prepare_expr (SIR.Expr'String eid id type_info sp s) = pure $ SIR.Expr'String eid id type_info sp s
prepare_expr (SIR.Expr'Int eid id type_info sp i) = pure $ SIR.Expr'Int eid id type_info sp i
prepare_expr (SIR.Expr'Float eid id type_info sp f) = pure $ SIR.Expr'Float eid id type_info sp f
prepare_expr (SIR.Expr'Bool eid id type_info sp b) = pure $ SIR.Expr'Bool eid id type_info sp b
prepare_expr (SIR.Expr'Tuple eid id type_info sp a b) = SIR.Expr'Tuple eid id type_info sp <$> prepare_expr a <*> prepare_expr b
prepare_expr (SIR.Expr'Lambda eid id type_info sp param body) = SIR.Expr'Lambda eid id type_info sp <$> prepare_pat param <*> prepare_expr body
prepare_expr (SIR.Expr'Let eid id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'Let eid id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'LetRec eid id type_info sp name_maps bindings adts type_synonyms body) = SIR.Expr'LetRec eid id type_info sp name_maps <$> mapM prepare_binding bindings <*> pure adts <*> pure type_synonyms <*> prepare_expr body
prepare_expr (SIR.Expr'BinaryOps eid id () type_info sp first ops) = do
    first <- prepare_expr first
    ops <- mapM (\(sp, iden, rhs) -> (sp,,) <$> prepare_split_iden iden <*> prepare_expr rhs) ops

    infix_group_key <- new_infix_grouped_key $ InfixGroupTask (map (\(_, iden, _) -> SIR.split_identifier_id iden) ops)

    pure $ SIR.Expr'BinaryOps eid id infix_group_key type_info sp first ops
prepare_expr (SIR.Expr'Call eid id type_info sp callee arg) = SIR.Expr'Call eid id type_info sp <$> prepare_expr callee <*> prepare_expr arg
prepare_expr (SIR.Expr'If eid id type_info sp if_sp cond t f) = SIR.Expr'If eid id type_info sp if_sp <$> prepare_expr cond <*> prepare_expr t <*> prepare_expr f
prepare_expr (SIR.Expr'Match eid id type_info sp match_tok_sp e arms) =
    SIR.Expr'Match eid id type_info sp match_tok_sp
        <$> prepare_expr e
        <*> mapM (\(ncs, pat, expr) -> (ncs,,) <$> prepare_pat pat <*> prepare_expr expr) arms
prepare_expr (SIR.Expr'TypeAnnotation eid id type_info sp (ty, ty_evaled_as_type) e) = do
    ty <- prepare_type_expr ty
    e <- prepare_expr e
    pure $ SIR.Expr'TypeAnnotation eid id type_info sp (ty, ty_evaled_as_type) e
prepare_expr (SIR.Expr'Forall eid id type_info sp ncs vars e) = SIR.Expr'Forall eid id type_info sp ncs vars <$> prepare_expr e
prepare_expr (SIR.Expr'TypeApply eid id type_info sp e (arg, arg_evaled_as_type)) = do
    arg <- prepare_type_expr arg
    e <- prepare_expr e
    pure $ SIR.Expr'TypeApply eid id type_info sp e (arg, arg_evaled_as_type)
prepare_expr (SIR.Expr'Hole eid id type_info sp hid) = pure $ SIR.Expr'Hole eid id type_info sp hid
prepare_expr (SIR.Expr'Poison eid id type_info sp) = pure $ SIR.Expr'Poison eid id type_info sp

prepare_pat :: SIR.Pattern Unprepared -> PrepareState (SIR.Pattern Prepared)
prepare_pat (SIR.Pattern'Variable id type_info sp bnk) = pure $ SIR.Pattern'Variable id type_info sp bnk
prepare_pat (SIR.Pattern'Wildcard id type_info sp) = pure $ SIR.Pattern'Wildcard id type_info sp
prepare_pat (SIR.Pattern'Tuple id type_info sp a b) = SIR.Pattern'Tuple id type_info sp <$> prepare_pat a <*> prepare_pat b
prepare_pat (SIR.Pattern'Named id type_info sp at_sp bnk subpat) = SIR.Pattern'Named id type_info sp at_sp bnk <$> prepare_pat subpat
prepare_pat (SIR.Pattern'AnonADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'AnonADTVariant id type_info sp
        <$> prepare_split_iden variant_iden
        <*> pure tyargs
        <*> mapM prepare_pat subpat
prepare_pat (SIR.Pattern'NamedADTVariant id type_info sp variant_iden tyargs subpat) =
    SIR.Pattern'NamedADTVariant id
        type_info
        sp
        <$> prepare_split_iden variant_iden
        <*> pure tyargs
        <*> mapM (\(name, pat) -> (name,) <$> prepare_pat pat) subpat
prepare_pat (SIR.Pattern'Poison id type_info sp) = pure $ SIR.Pattern'Poison id type_info sp

prepare_split_iden ::
    SIR.SplitIdentifier id_name Unprepared ->
    PrepareState (SIR.SplitIdentifier id_name Prepared)
prepare_split_iden (SIR.SplitIdentifier'Get id texpr next) = do
    texpr <- prepare_type_expr texpr
    pure $ SIR.SplitIdentifier'Get id texpr next
prepare_split_iden (SIR.SplitIdentifier'Single id name_maps i) = pure $ SIR.SplitIdentifier'Single id name_maps i
