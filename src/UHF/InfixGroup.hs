module UHF.InfixGroup
    ( group
    ) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.Diagnostic as Diagnostic

import qualified UHF.IR as IR

type UngroupedBinding typeannotation = IR.Binding (Maybe IR.BoundNameKey) typeannotation () ()
type UngroupedExpr typeannotation = IR.Expr (Maybe IR.BoundNameKey) typeannotation () ()

type GroupedBinding typeannotation = IR.Binding (Maybe IR.BoundNameKey) typeannotation () Void
type GroupedExpr typeannotation = IR.Expr (Maybe IR.BoundNameKey) typeannotation () Void

type Pattern = IR.Pattern (Maybe IR.BoundNameKey)

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

group_expr (IR.Expr'BinaryOps () () sp first ops) = fst $ g (group_expr first) ops 0
    where
        -- TODO: test this
        g :: GroupedExpr tya -> [(Maybe IR.BoundNameKey, UngroupedExpr tya)] -> Int -> (GroupedExpr tya, [(Maybe IR.BoundNameKey, UngroupedExpr tya)])
        g left more@((first_op, first_rhs):after_first_op) cur_precedence =
            let op_prec = const 1 first_op -- TODO: precedence
            -- for example if the current precedence level is that for +, and first_op is *, this will consume the * and incorporate it into left
            in if op_prec > cur_precedence
                then
                    -- continuing the example from above, this will consume all the operators that bind tighetr than *, forming the right side of the * operation
                    let (rhs, after) = g (group_expr first_rhs) after_first_op op_prec -- TODO: associativity
                        left' = IR.Expr'Call () todo (IR.Expr'Call () todo (IR.Expr'Identifier () todo first_op) left) rhs
                    in g left' after cur_precedence
                else (left, more)

        g left [] _ = (left, [])

group_expr (IR.Expr'Call () sp callee arg) = IR.Expr'Call () sp (group_expr callee) (group_expr arg)

group_expr (IR.Expr'If () sp if_sp cond true false) = IR.Expr'If () sp if_sp (group_expr cond) (group_expr true) (group_expr false)
group_expr (IR.Expr'Case () sp case_sp e arms) = IR.Expr'Case () sp case_sp (group_expr e) (map (\ (p, e) -> (p, group_expr e)) arms)

group_expr (IR.Expr'Poison () sp) = IR.Expr'Poison () sp

group_expr (IR.Expr'TypeAnnotation () sp annotation e) = IR.Expr'TypeAnnotation () sp annotation (group_expr e)
