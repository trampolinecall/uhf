module UHF.Parts.UnifiedFrontendSolver.InfixGroup.Solve (group) where

import UHF.Prelude

import qualified UHF.Data.SIR as SIR
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Misc.Result (InfixGroupResult (..))
import UHF.Parts.UnifiedFrontendSolver.InfixGroup.Task (InfixGroupTask (..))
import UHF.Parts.UnifiedFrontendSolver.NameResolve.Misc.Result (IdenResolvedKey)
import UHF.Parts.UnifiedFrontendSolver.ProgressMade (ProgressMade (..))
import UHF.Parts.UnifiedFrontendSolver.SolveResult (SolveResult (..))
import UHF.Parts.UnifiedFrontendSolver.Solving (SolveMonad, get_value_iden_resolved)
import qualified UHF.Util.Arena as Arena

group :: InfixGroupTask -> SolveMonad (ProgressMade InfixGroupTask)
group (InfixGroupTask operators result_key) = do
    let first = Operand 0
        more = zip operators [1 ..]
    res <- go first more 0

    case res of
        Inconclusive _ -> pure Unsuccessful
        Errored () -> do
        -- TODO: this is very similar to put_result from NameResolve so maybe there is a general version that can be put into UnifiedFrontendSolver.Solving?
            modify $
                \(nr_things, result_arena, infer_vars) ->
                    ( nr_things
                    , Arena.modify
                        result_arena
                        result_key
                        ( \case
                            Inconclusive _ -> Errored ()
                            _ -> Errored () -- TODO: internal warning because there was already a result here and it was recomputed?
                        )
                    , infer_vars
                    )
            pure $ Successful []
        Solved (res, a) -> do
            when (not $ null a) $ error "internal error: still operations to group after grouping binary ops"

            modify $
                \(nr_things, result_arena, infer_vars) ->
                    ( nr_things
                    , Arena.modify
                        result_arena
                        result_key
                        ( \case
                            Inconclusive _ -> Solved res
                            _ -> Solved res -- TODO: internal warning because there was already a result here and it was recomputed?
                        )
                    , infer_vars
                    )
            pure $ Successful []
    where
        go ::
            InfixGroupResult ->
            [(IdenResolvedKey SIR.ValueRef, Int)] ->
            Int ->
            SolveMonad (SolveResult () () (InfixGroupResult, [(IdenResolvedKey SIR.ValueRef, Int)]))
        go left more@((first_op, first_rhs) : after_first_op) cur_precedence = do
            first_op <- get_value_iden_resolved first_op
            case first_op of
                Solved first_op -> do
                    let op_prec = get_op_prec first_op
                    -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
                    if op_prec > cur_precedence
                        then do
                            -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                            rhs_grouped <- go (Operand first_rhs) after_first_op op_prec -- TODO: associativity
                            case rhs_grouped of
                                Solved (rhs, after) -> do
                                    let left' = Call (Call (Operator first_op) left) rhs
                                    go left' after cur_precedence
                                Errored _ -> pure $ Errored ()
                                Inconclusive _ -> pure $ Inconclusive ()
                        else pure $ Solved (left, more)
                Errored _ -> pure $ Errored ()
                Inconclusive _ -> pure $ Inconclusive ()
        go left [] _ = pure $ Solved (left, [])

get_op_prec :: SIR.ValueRef -> Int
get_op_prec = const 1 -- TODO: precedence

-- type VIden = Maybe SIR.ValueRef
--
-- -- type IsUngrouped s = (SIR.VIdenResolved s ~ VIden, SIR.TypeInfo s ~ (), SIR.InfixGroupedKey s ~ ())
-- -- type IsGrouped s = (SIR.VIdenResolved s ~ VIden, SIR.TypeInfo s ~ (), SIR.InfixGroupedKey s ~ Void)
-- type IsUngrouped s = (SIR.IdenResolvedKey s ~ Maybe , SIR.TypeInfo s ~ (), SIR.InfixGroupedKey s ~ ())
-- type IsGrouped s = (SIR.IdenResolvedKey s ~ Maybe , SIR.TypeInfo s ~ (), SIR.InfixGroupedKey s ~ Void)
-- type Convertible ungrouped grouped =
--     ( IsUngrouped ungrouped
--     , IsGrouped grouped
--     -- , SIR.DIdenStart ungrouped ~ SIR.DIdenStart grouped
--     -- , SIR.VIdenStart ungrouped ~ SIR.VIdenStart grouped
--     , SIR.TypeExprEvaledKey ungrouped ~ SIR.TypeExprEvaledKey grouped
--     , SIR.TypeExprEvaledAsTypeKey ungrouped ~ SIR.TypeExprEvaledAsTypeKey grouped
--     , SIR.NameMapIndex ungrouped ~ SIR.NameMapIndex grouped
--     -- , SIR.PIdenStart ungrouped ~ SIR.PIdenStart grouped
--     -- , SIR.PIdenResolved ungrouped ~ SIR.PIdenResolved grouped
--     )
--
-- group :: Convertible ungrouped grouped => SIR.SIR ungrouped -> SIR.SIR grouped
-- group (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
--     SIR.SIR
--         (IDGen.run_id_gen ID.ExprID'InfixGroupGen (Arena.transformM group_module modules))
--         (Arena.transform convert_adt adts)
--         (Arena.transform convert_type_synonym type_synonyms)
--         type_vars
--         (Arena.transform convert_variable variables)
--         (SIR.CU root_module main_function)
--     where
--         -- TODO: automate these functions too?
--         convert_adt (SIR.ADT did name tyvars variants) = SIR.ADT did name tyvars (map convert_variant variants)
--             where
--                 convert_variant (SIR.ADTVariant'Anon name id fields) = SIR.ADTVariant'Anon name id (map (\ (i, t) -> (i, convert_type_expr t)) fields)
--                 convert_variant (SIR.ADTVariant'Named name id fields) = SIR.ADTVariant'Named name id (map (\ (i, n, t) -> (i, n, convert_type_expr t)) fields)
--         convert_type_synonym (SIR.TypeSynonym did name exp) = SIR.TypeSynonym did name (convert_type_expr exp)
--         convert_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n
--
-- group_module :: Convertible ungrouped grouped => SIR.Module ungrouped -> IDGen.IDGen ID.ExprID (SIR.Module grouped)
-- group_module (SIR.Module id name_map_index bindings adts syns) = SIR.Module id name_map_index <$> mapM group_binding bindings <*> pure adts <*> pure syns
--
-- group_binding :: Convertible ungrouped grouped => SIR.Binding ungrouped -> IDGen.IDGen ID.ExprID (SIR.Binding grouped)
-- group_binding (SIR.Binding pat eq_sp e) = SIR.Binding (convert_pattern pat) eq_sp <$> group_expr e
--
-- group_expr :: Convertible ungrouped grouped => SIR.Expr ungrouped -> IDGen.IDGen ID.ExprID (SIR.Expr grouped)
-- group_expr (SIR.Expr'Refer id () sp iden resolved) = pure $ SIR.Expr'Refer id () sp (convert_split_iden iden) resolved
-- group_expr (SIR.Expr'Char id () sp c) = pure $ SIR.Expr'Char id () sp c
-- group_expr (SIR.Expr'String id () sp t) = pure $ SIR.Expr'String id () sp t
-- group_expr (SIR.Expr'Int id () sp i) = pure $ SIR.Expr'Int id () sp i
-- group_expr (SIR.Expr'Float id () sp r) = pure $ SIR.Expr'Float id () sp r
-- group_expr (SIR.Expr'Bool id () sp b) = pure $ SIR.Expr'Bool id () sp b
--
-- group_expr (SIR.Expr'Tuple id () sp a b) = SIR.Expr'Tuple id () sp <$> group_expr a <*> group_expr b
--
-- group_expr (SIR.Expr'Lambda id () sp param body) = SIR.Expr'Lambda id () sp (convert_pattern param) <$> group_expr body
--
-- group_expr (SIR.Expr'Let id () sp name_map_index bindings adts type_synonyms body) = SIR.Expr'Let id () sp name_map_index <$> mapM group_binding bindings <*> pure adts <*> pure type_synonyms <*> group_expr body
-- group_expr (SIR.Expr'LetRec id () sp name_map_index bindings adts type_synonyms body) = SIR.Expr'LetRec id () sp name_map_index <$> mapM group_binding bindings <*> pure adts <*> pure type_synonyms <*> group_expr body
--
-- group_expr (SIR.Expr'BinaryOps _ () () _ first ops) =
--
-- group_expr (SIR.Expr'Call id () sp callee arg) = SIR.Expr'Call id () sp <$> group_expr callee <*> group_expr arg
--
-- group_expr (SIR.Expr'If id () sp if_sp cond true false) = SIR.Expr'If id () sp if_sp <$> group_expr cond <*> group_expr true <*> group_expr false
-- group_expr (SIR.Expr'Match id () sp match_tok_sp e arms) = SIR.Expr'Match id () sp match_tok_sp <$> group_expr e <*> mapM (\ (name_map_index, p, e) -> (name_map_index,convert_pattern p,) <$> group_expr e) arms
--
-- group_expr (SIR.Expr'Poison id () sp) = pure $ SIR.Expr'Poison id () sp
--
-- group_expr (SIR.Expr'Hole id () sp hid) = pure $ SIR.Expr'Hole id () sp hid
--
-- group_expr (SIR.Expr'TypeAnnotation id () sp annotation e) = SIR.Expr'TypeAnnotation id () sp (convert_type_expr_and_ty annotation) <$> group_expr e
--
-- group_expr (SIR.Expr'Forall id () sp name_map_index names e) = SIR.Expr'Forall id () sp name_map_index names <$> group_expr e
-- group_expr (SIR.Expr'TypeApply id () sp e args) = SIR.Expr'TypeApply id () sp <$> group_expr e <*> pure (convert_type_expr_and_ty args)
--
-- -- TODO: automate functions like this?
-- convert_type_expr_and_ty :: Convertible ungrouped grouped => (SIR.TypeExpr ungrouped, SIR.IdenResolvedKey grouped (SIR.TypeExprEvaledAsTypeKey ungrouped)) -> (SIR.TypeExpr grouped, SIR.IdenResolvedKey grouped (SIR.TypeExprEvaledAsTypeKey grouped))
-- convert_type_expr_and_ty (tye, ty) = (convert_type_expr tye, ty)
--
-- convert_split_iden :: Convertible ungrouped grouped => SIR.SplitIdentifier start ungrouped -> SIR.SplitIdentifier start grouped
-- convert_split_iden (SIR.SplitIdentifier'Get texpr next resolved) = SIR.SplitIdentifier'Get (convert_type_expr texpr) next resolved
-- convert_split_iden (SIR.SplitIdentifier'Single single name resolved) = SIR.SplitIdentifier'Single single name resolved
--
-- convert_type_expr :: Convertible ungrouped grouped => SIR.TypeExpr ungrouped -> SIR.TypeExpr grouped
-- convert_type_expr (SIR.TypeExpr'Refer evaled sp var_key name resolved) = SIR.TypeExpr'Refer evaled sp var_key name resolved
-- convert_type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp (convert_type_expr parent) name
-- convert_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp (convert_type_expr a) (convert_type_expr b)
-- convert_type_expr (SIR.TypeExpr'Hole evaled type_info sp hiden) = SIR.TypeExpr'Hole evaled type_info sp hiden
-- convert_type_expr (SIR.TypeExpr'Function evaled sp a b) = SIR.TypeExpr'Function evaled sp (convert_type_expr a) (convert_type_expr b)
-- convert_type_expr (SIR.TypeExpr'Forall evaled sp name_map_index tyvars res) = SIR.TypeExpr'Forall evaled sp name_map_index tyvars (convert_type_expr res)
-- convert_type_expr (SIR.TypeExpr'Apply evaled sp c a) = SIR.TypeExpr'Apply evaled sp (convert_type_expr c) (convert_type_expr a)
-- convert_type_expr (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild evaled sp
-- convert_type_expr (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison evaled sp
--
-- convert_pattern :: Convertible ungrouped grouped => SIR.Pattern ungrouped -> SIR.Pattern grouped
-- convert_pattern (SIR.Pattern'Variable tyinfo sp var_key) = SIR.Pattern'Variable tyinfo sp var_key
-- convert_pattern (SIR.Pattern'Wildcard tyinfo sp) = SIR.Pattern'Wildcard tyinfo sp
-- convert_pattern (SIR.Pattern'Tuple tyinfo sp a b) = SIR.Pattern'Tuple tyinfo sp (convert_pattern a) (convert_pattern b)
-- convert_pattern (SIR.Pattern'Named tyinfo sp at var_key sub) = SIR.Pattern'Named tyinfo sp at var_key (convert_pattern sub)
-- convert_pattern (SIR.Pattern'AnonADTVariant tyinfo sp variant_iden variant_resolved tyapps fields) = SIR.Pattern'AnonADTVariant tyinfo sp (convert_split_iden variant_iden) variant_resolved tyapps (map convert_pattern fields)
-- convert_pattern (SIR.Pattern'NamedADTVariant tyinfo sp variant_iden variant_resolved tyapps fields) = SIR.Pattern'NamedADTVariant tyinfo sp (convert_split_iden variant_iden) variant_resolved tyapps (map (\ (n, f) -> (n, convert_pattern f)) fields)
-- convert_pattern (SIR.Pattern'Poison tyinfo sp) = SIR.Pattern'Poison tyinfo sp
