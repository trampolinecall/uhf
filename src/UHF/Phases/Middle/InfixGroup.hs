module UHF.Phases.Middle.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located)

import qualified UHF.Data.IR.HIR as HIR
import UHF.Data.IR.Keys

type UngroupedHIR type_annotation = HIR.HIR (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedDecl type_annotation = HIR.Decl (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedBinding type_annotation = HIR.Binding (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedExpr type_annotation = HIR.Expr (Located (Maybe BoundValueKey)) type_annotation () ()

type GroupedHIR type_annotation = HIR.HIR (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedDecl type_annotation = HIR.Decl (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedBinding type_annotation = HIR.Binding (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedExpr type_annotation = HIR.Expr (Located (Maybe BoundValueKey)) type_annotation () Void

group :: UngroupedHIR type_annotation -> GroupedHIR type_annotation
group (HIR.HIR decls adts type_synonyms bound_values mod) = HIR.HIR (Arena.transform group_decl decls) adts type_synonyms bound_values mod

group_decl :: UngroupedDecl type_annotation -> GroupedDecl type_annotation
group_decl (HIR.Decl'Module id nc bindings adts syns) = HIR.Decl'Module id nc (map group_binding bindings) adts syns
group_decl (HIR.Decl'Type ty) = HIR.Decl'Type ty

group_binding :: UngroupedBinding type_annotation -> GroupedBinding type_annotation
group_binding (HIR.Binding pat eq_sp e) = HIR.Binding pat eq_sp (group_expr e)

group_expr :: UngroupedExpr type_annotation -> GroupedExpr type_annotation
group_expr (HIR.Expr'Identifier id () sp iden) = HIR.Expr'Identifier id () sp iden
group_expr (HIR.Expr'Char id () sp c) = HIR.Expr'Char id () sp c
group_expr (HIR.Expr'String id () sp t) = HIR.Expr'String id () sp t
group_expr (HIR.Expr'Int id () sp i) = HIR.Expr'Int id () sp i
group_expr (HIR.Expr'Float id () sp r) = HIR.Expr'Float id () sp r
group_expr (HIR.Expr'Bool id () sp b) = HIR.Expr'Bool id () sp b

group_expr (HIR.Expr'Tuple id () sp a b) = HIR.Expr'Tuple id () sp (group_expr a) (group_expr b)

group_expr (HIR.Expr'Lambda id () sp param body) = HIR.Expr'Lambda id () sp param (group_expr body)

group_expr (HIR.Expr'Let id () sp bindings body) = HIR.Expr'Let id () sp (map group_binding bindings) (group_expr body)

group_expr (HIR.Expr'BinaryOps id () () _ first ops) =
    let (r, a) = g (group_expr first) ops 0
    in if null a then r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: GroupedExpr tya -> [(Located (Maybe BoundValueKey), UngroupedExpr tya)] -> Int -> (GroupedExpr tya, [(Located (Maybe BoundValueKey), UngroupedExpr tya)])
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
                        left' = HIR.Expr'Call id () (lhs_span <> rhs_span) (HIR.Expr'Call id () (lhs_span <> op_span) (HIR.Expr'Identifier id () op_span first_op) left) rhs
                    in g left' after cur_precedence

                else (left, more)

        g left [] _ = (left, [])

group_expr (HIR.Expr'Call id () sp callee arg) = HIR.Expr'Call id () sp (group_expr callee) (group_expr arg)

group_expr (HIR.Expr'If id () sp if_sp cond true false) = HIR.Expr'If id () sp if_sp (group_expr cond) (group_expr true) (group_expr false)
group_expr (HIR.Expr'Case id () sp case_sp e arms) = HIR.Expr'Case id () sp case_sp (group_expr e) (map (\ (p, e) -> (p, group_expr e)) arms)

group_expr (HIR.Expr'Poison id () sp) = HIR.Expr'Poison id () sp

group_expr (HIR.Expr'TypeAnnotation id () sp annotation e) = HIR.Expr'TypeAnnotation id () sp annotation (group_expr e)
