module UHF.Phases.Middle.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located)

import qualified UHF.Data.IR.SIR as SIR
import UHF.Data.IR.Keys

type UngroupedSIR type_annotation = SIR.SIR (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedDecl type_annotation = SIR.Decl (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedBinding type_annotation = SIR.Binding (Located (Maybe BoundValueKey)) type_annotation () ()
type UngroupedExpr type_annotation = SIR.Expr (Located (Maybe BoundValueKey)) type_annotation () ()

type GroupedSIR type_annotation = SIR.SIR (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedDecl type_annotation = SIR.Decl (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedBinding type_annotation = SIR.Binding (Located (Maybe BoundValueKey)) type_annotation () Void
type GroupedExpr type_annotation = SIR.Expr (Located (Maybe BoundValueKey)) type_annotation () Void

group :: UngroupedSIR type_annotation -> GroupedSIR type_annotation
group (SIR.SIR decls adts type_synonyms bound_values mod) = SIR.SIR (Arena.transform group_decl decls) adts type_synonyms bound_values mod

group_decl :: UngroupedDecl type_annotation -> GroupedDecl type_annotation
group_decl (SIR.Decl'Module id nc bindings adts syns) = SIR.Decl'Module id nc (map group_binding bindings) adts syns
group_decl (SIR.Decl'Type ty) = SIR.Decl'Type ty

group_binding :: UngroupedBinding type_annotation -> GroupedBinding type_annotation
group_binding (SIR.Binding pat eq_sp e) = SIR.Binding pat eq_sp (group_expr e)

group_expr :: UngroupedExpr type_annotation -> GroupedExpr type_annotation
group_expr (SIR.Expr'Identifier id () sp iden) = SIR.Expr'Identifier id () sp iden
group_expr (SIR.Expr'Char id () sp c) = SIR.Expr'Char id () sp c
group_expr (SIR.Expr'String id () sp t) = SIR.Expr'String id () sp t
group_expr (SIR.Expr'Int id () sp i) = SIR.Expr'Int id () sp i
group_expr (SIR.Expr'Float id () sp r) = SIR.Expr'Float id () sp r
group_expr (SIR.Expr'Bool id () sp b) = SIR.Expr'Bool id () sp b

group_expr (SIR.Expr'Tuple id () sp a b) = SIR.Expr'Tuple id () sp (group_expr a) (group_expr b)

group_expr (SIR.Expr'Lambda id () sp param body) = SIR.Expr'Lambda id () sp param (group_expr body)

group_expr (SIR.Expr'Let id () sp bindings body) = SIR.Expr'Let id () sp (map group_binding bindings) (group_expr body)

group_expr (SIR.Expr'BinaryOps id () () _ first ops) =
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
                        lhs_span = SIR.expr_span left
                        rhs_span = SIR.expr_span rhs
                        op_span = Located.just_span first_op
                        left' = SIR.Expr'Call id () (lhs_span <> rhs_span) (SIR.Expr'Call id () (lhs_span <> op_span) (SIR.Expr'Identifier id () op_span first_op) left) rhs
                    in g left' after cur_precedence

                else (left, more)

        g left [] _ = (left, [])

group_expr (SIR.Expr'Call id () sp callee arg) = SIR.Expr'Call id () sp (group_expr callee) (group_expr arg)

group_expr (SIR.Expr'If id () sp if_sp cond true false) = SIR.Expr'If id () sp if_sp (group_expr cond) (group_expr true) (group_expr false)
group_expr (SIR.Expr'Case id () sp case_sp e arms) = SIR.Expr'Case id () sp case_sp (group_expr e) (map (\ (p, e) -> (p, group_expr e)) arms)

group_expr (SIR.Expr'Poison id () sp) = SIR.Expr'Poison id () sp

group_expr (SIR.Expr'TypeAnnotation id () sp annotation e) = SIR.Expr'TypeAnnotation id () sp annotation (group_expr e)
