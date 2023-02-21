module UHF.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located)

import qualified UHF.HIR as HIR

type UngroupedBinding typeannotation = HIR.Binding (Located (Maybe HIR.BoundValueKey)) typeannotation () ()
type UngroupedExpr typeannotation = HIR.Expr (Located (Maybe HIR.BoundValueKey)) typeannotation () ()

type GroupedBinding typeannotation = HIR.Binding (Located (Maybe HIR.BoundValueKey)) typeannotation () Void
type GroupedExpr typeannotation = HIR.Expr (Located (Maybe HIR.BoundValueKey)) typeannotation () Void

type UngroupedBindingArena typeannotation = Arena.Arena (UngroupedBinding typeannotation) HIR.BindingKey
type GroupedBindingArena typeannotation = Arena.Arena (GroupedBinding typeannotation) HIR.BindingKey

group :: UngroupedBindingArena typeannotation -> GroupedBindingArena typeannotation
group = Arena.transform group_binding

group_binding :: UngroupedBinding typeannotation -> GroupedBinding typeannotation
group_binding (HIR.Binding pat eq_sp e) = HIR.Binding pat eq_sp (group_expr e)

group_expr :: UngroupedExpr typeannotation -> GroupedExpr typeannotation
group_expr (HIR.Expr'Identifier () sp iden) = HIR.Expr'Identifier () sp iden
group_expr (HIR.Expr'Char () sp c) = HIR.Expr'Char () sp c
group_expr (HIR.Expr'String () sp t) = HIR.Expr'String () sp t
group_expr (HIR.Expr'Int () sp i) = HIR.Expr'Int () sp i
group_expr (HIR.Expr'Float () sp r) = HIR.Expr'Float () sp r
group_expr (HIR.Expr'Bool () sp b) = HIR.Expr'Bool () sp b

group_expr (HIR.Expr'Tuple () sp a b) = HIR.Expr'Tuple () sp (group_expr a) (group_expr b)

group_expr (HIR.Expr'Lambda () sp param body) = HIR.Expr'Lambda () sp param (group_expr body)

group_expr (HIR.Expr'Let () sp body) = HIR.Expr'Let () sp (group_expr body)
group_expr (HIR.Expr'LetRec () sp body) = HIR.Expr'LetRec () sp (group_expr body)

group_expr (HIR.Expr'BinaryOps () () _ first ops) =
    let (r, a) = g (group_expr first) ops 0
    in if null a then r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: GroupedExpr tya -> [(Located (Maybe HIR.BoundValueKey), UngroupedExpr tya)] -> Int -> (GroupedExpr tya, [(Located (Maybe HIR.BoundValueKey), UngroupedExpr tya)])
        g left more@((first_op, first_rhs):after_first_op) cur_precedence =
            let op_prec = const 1 first_op -- TODO: precedence
            -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
            in if op_prec > cur_precedence
                then
                    -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                    let (rhs, after) = g (group_expr first_rhs) after_first_op op_prec -- TODO: associativity
                        lhs_span = HIR.expr_span left
                        rhs_span = HIR.expr_span rhs
                        op_span = Located.just_span first_op
                        left' = HIR.Expr'Call () (lhs_span <> rhs_span) (HIR.Expr'Call () (lhs_span <> op_span) (HIR.Expr'Identifier () op_span first_op) left) rhs
                    in g left' after cur_precedence

                else (left, more)

        g left [] _ = (left, [])

group_expr (HIR.Expr'Call () sp callee arg) = HIR.Expr'Call () sp (group_expr callee) (group_expr arg)

group_expr (HIR.Expr'If () sp if_sp cond true false) = HIR.Expr'If () sp if_sp (group_expr cond) (group_expr true) (group_expr false)
group_expr (HIR.Expr'Case () sp case_sp e arms) = HIR.Expr'Case () sp case_sp (group_expr e) (map (\ (p, e) -> (p, group_expr e)) arms)

group_expr (HIR.Expr'Poison () sp) = HIR.Expr'Poison () sp

group_expr (HIR.Expr'TypeAnnotation () sp annotation e) = HIR.Expr'TypeAnnotation () sp annotation (group_expr e)
