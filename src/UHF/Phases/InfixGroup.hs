module UHF.Phases.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located)

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.IDGen as IDGen
import qualified UHF.Data.IR.ID as ID

type VIden = Located (Maybe SIR.BoundValueKey)
type UngroupedSIR d_iden p_iden = SIR.SIR d_iden VIden p_iden () ()
type UngroupedModule d_iden p_iden = SIR.Module d_iden VIden p_iden () ()
type UngroupedBinding d_iden p_iden = SIR.Binding d_iden VIden p_iden () ()
type UngroupedExpr d_iden p_iden = SIR.Expr d_iden VIden p_iden () ()

type GroupedSIR d_iden p_iden = SIR.SIR d_iden VIden p_iden () Void
type GroupedModule d_iden p_iden = SIR.Module d_iden VIden p_iden () Void
type GroupedBinding d_iden p_iden = SIR.Binding d_iden VIden p_iden () Void
type GroupedExpr d_iden p_iden = SIR.Expr d_iden VIden p_iden () Void

group :: UngroupedSIR d_iden p_iden -> GroupedSIR d_iden p_iden
group (SIR.SIR decls modules adts type_synonyms type_vars bound_values mod) = SIR.SIR decls (IDGen.run_id_gen ID.ExprID'InfixGroupGen (Arena.transformM group_module modules)) adts type_synonyms type_vars bound_values mod

group_module :: UngroupedModule d_iden p_iden -> IDGen.IDGen ID.ExprID (GroupedModule d_iden p_iden)
group_module (SIR.Module id bindings adts syns) = SIR.Module id <$> mapM group_binding bindings <*> pure adts <*> pure syns

group_binding :: UngroupedBinding d_iden p_iden -> IDGen.IDGen ID.ExprID (GroupedBinding d_iden p_iden)
group_binding (SIR.Binding pat eq_sp e) = SIR.Binding pat eq_sp <$> group_expr e
group_binding (SIR.Binding'ADTVariant sp bvk variant) = pure $ SIR.Binding'ADTVariant sp bvk variant

group_expr :: UngroupedExpr d_iden p_iden -> IDGen.IDGen ID.ExprID (GroupedExpr d_iden p_iden)
group_expr (SIR.Expr'Identifier id () sp iden) = pure $ SIR.Expr'Identifier id () sp iden
group_expr (SIR.Expr'Char id () sp c) = pure $ SIR.Expr'Char id () sp c
group_expr (SIR.Expr'String id () sp t) = pure $ SIR.Expr'String id () sp t
group_expr (SIR.Expr'Int id () sp i) = pure $ SIR.Expr'Int id () sp i
group_expr (SIR.Expr'Float id () sp r) = pure $ SIR.Expr'Float id () sp r
group_expr (SIR.Expr'Bool id () sp b) = pure $ SIR.Expr'Bool id () sp b

group_expr (SIR.Expr'Tuple id () sp a b) = SIR.Expr'Tuple id () sp <$> group_expr a <*> group_expr b

group_expr (SIR.Expr'Lambda id () sp param body) = SIR.Expr'Lambda id () sp param <$> group_expr body

group_expr (SIR.Expr'Let id () sp bindings body) = SIR.Expr'Let id () sp <$> mapM group_binding bindings <*> group_expr body
group_expr (SIR.Expr'LetRec id () sp bindings body) = SIR.Expr'LetRec id () sp <$> mapM group_binding bindings <*> group_expr body

group_expr (SIR.Expr'BinaryOps _ () () _ first ops) =
    group_expr first >>= \ first ->
    g first ops 0 >>= \ (r, a) ->
    if null a then pure r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: GroupedExpr d_iden p_iden -> [(Located (Maybe SIR.BoundValueKey), UngroupedExpr d_iden p_iden)] -> Int -> IDGen.IDGen ID.ExprID (GroupedExpr d_iden p_iden, [(Located (Maybe SIR.BoundValueKey), UngroupedExpr d_iden p_iden)])
        g left more@((first_op, first_rhs):after_first_op) cur_precedence =
            let op_prec = const 1 first_op -- TODO: precedence
            -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
            in if op_prec > cur_precedence
                then
                    -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                    group_expr first_rhs >>= \ first_rhs ->
                    g first_rhs after_first_op op_prec >>= \ (rhs, after) -> -- TODO: associativity
                    IDGen.gen_id >>= \ call_with_rhs_id ->
                    IDGen.gen_id >>= \ call_with_lhs_id ->
                    IDGen.gen_id >>= \ refer_to_op_id ->
                    let lhs_span = SIR.expr_span left
                        rhs_span = SIR.expr_span rhs
                        op_span = Located.just_span first_op
                        left' = SIR.Expr'Call call_with_rhs_id () (lhs_span <> rhs_span) (SIR.Expr'Call call_with_lhs_id () (lhs_span <> op_span) (SIR.Expr'Identifier refer_to_op_id () op_span first_op) left) rhs
                    in g left' after cur_precedence

                else pure (left, more)

        g left [] _ = pure (left, [])

group_expr (SIR.Expr'Call id () sp callee arg) = SIR.Expr'Call id () sp <$> group_expr callee <*> group_expr arg

group_expr (SIR.Expr'If id () sp if_sp cond true false) = SIR.Expr'If id () sp if_sp <$> group_expr cond <*> group_expr true <*> group_expr false
group_expr (SIR.Expr'Case id () sp case_sp e arms) = SIR.Expr'Case id () sp case_sp <$> group_expr e <*> mapM (\ (p, e) -> (p,) <$> group_expr e) arms

group_expr (SIR.Expr'Poison id () sp) = pure $ SIR.Expr'Poison id () sp

group_expr (SIR.Expr'Hole id () sp hid) = pure $ SIR.Expr'Hole id () sp hid

group_expr (SIR.Expr'TypeAnnotation id () sp annotation e) = SIR.Expr'TypeAnnotation id () sp annotation <$> group_expr e

group_expr (SIR.Expr'Forall id () sp names e) = SIR.Expr'Forall id () sp names <$> group_expr e
group_expr (SIR.Expr'TypeApply id () sp e args) = SIR.Expr'TypeApply id () sp <$> group_expr e <*> pure args
