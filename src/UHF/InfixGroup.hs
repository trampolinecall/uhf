module UHF.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import UHF.IO.Location (Located)
import qualified UHF.IO.Location as Location

import qualified UHF.IR as IR

type UngroupedBinding typeannotation = IR.Binding (Located (Maybe IR.BoundValueKey)) typeannotation () ()
type UngroupedExpr typeannotation = IR.Expr (Located (Maybe IR.BoundValueKey)) typeannotation () ()

type GroupedBinding typeannotation = IR.Binding (Located (Maybe IR.BoundValueKey)) typeannotation () Void
type GroupedExpr typeannotation = IR.Expr (Located (Maybe IR.BoundValueKey)) typeannotation () Void

type UngroupedBindingArena typeannotation = Arena.Arena (UngroupedBinding typeannotation) IR.BindingKey
type GroupedBindingArena typeannotation = Arena.Arena (GroupedBinding typeannotation) IR.BindingKey

group :: UngroupedBindingArena typeannotation -> GroupedBindingArena typeannotation
group = Arena.transform group_binding

group_binding :: UngroupedBinding typeannotation -> GroupedBinding typeannotation
group_binding (IR.Binding pat eq_sp e) = IR.Binding pat eq_sp (group_expr e)

group_expr :: UngroupedExpr typeannotation -> GroupedExpr typeannotation
group_expr (IR.Expr'Identifier () sp iden) = IR.Expr'Identifier () sp iden
group_expr (IR.Expr'Char () sp c) = IR.Expr'Char () sp c
group_expr (IR.Expr'String () sp t) = IR.Expr'String () sp t
group_expr (IR.Expr'Int () sp i) = IR.Expr'Int () sp i
group_expr (IR.Expr'Float () sp r) = IR.Expr'Float () sp r
group_expr (IR.Expr'Bool () sp b) = IR.Expr'Bool () sp b

group_expr (IR.Expr'Tuple () sp a b) = IR.Expr'Tuple () sp (group_expr a) (group_expr b)

group_expr (IR.Expr'Lambda () sp param body) = IR.Expr'Lambda () sp param (group_expr body)

group_expr (IR.Expr'Let () sp body) = IR.Expr'Let () sp (group_expr body)
group_expr (IR.Expr'LetRec () sp body) = IR.Expr'LetRec () sp (group_expr body)

group_expr (IR.Expr'BinaryOps () () _ first ops) =
    let (r, a) = g (group_expr first) ops 0
    in if null a then r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: GroupedExpr tya -> [(Located (Maybe IR.BoundValueKey), UngroupedExpr tya)] -> Int -> (GroupedExpr tya, [(Located (Maybe IR.BoundValueKey), UngroupedExpr tya)])
        g left more@((first_op, first_rhs):after_first_op) cur_precedence =
            let op_prec = const 1 first_op -- TODO: precedence
            -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
            in if op_prec > cur_precedence
                then
                    -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                    let (rhs, after) = g (group_expr first_rhs) after_first_op op_prec -- TODO: associativity
                        lhs_span = IR.expr_span left
                        rhs_span = IR.expr_span rhs
                        op_span = Location.just_span first_op
                        left' = IR.Expr'Call () (lhs_span `Location.join_span` rhs_span) (IR.Expr'Call () (lhs_span `Location.join_span` op_span) (IR.Expr'Identifier () op_span first_op) left) rhs
                    in g left' after cur_precedence

                else (left, more)

        g left [] _ = (left, [])

group_expr (IR.Expr'Call () sp callee arg) = IR.Expr'Call () sp (group_expr callee) (group_expr arg)

group_expr (IR.Expr'If () sp if_sp cond true false) = IR.Expr'If () sp if_sp (group_expr cond) (group_expr true) (group_expr false)
group_expr (IR.Expr'Case () sp case_sp e arms) = IR.Expr'Case () sp case_sp (group_expr e) (map (\ (p, e) -> (p, group_expr e)) arms)

group_expr (IR.Expr'Poison () sp) = IR.Expr'Poison () sp

group_expr (IR.Expr'TypeAnnotation () sp annotation e) = IR.Expr'TypeAnnotation () sp annotation (group_expr e)
