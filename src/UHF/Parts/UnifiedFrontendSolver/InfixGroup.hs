{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module UHF.Parts.UnifiedFrontendSolver.InfixGroup (group) where

import UHF.Prelude

import UHF.Source.Span (Span)
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.SIR as SIR
import qualified UHF.Util.Arena as Arena
import qualified UHF.Util.IDGen as IDGen

type VIden = Maybe SIR.ValueRef

type IsUngrouped s = (SIR.VIdenResolved s ~ VIden, SIR.TypeInfo s ~ (), SIR.BinaryOpsAllowed s ~ ())
type IsGrouped s = (SIR.VIdenResolved s ~ VIden, SIR.TypeInfo s ~ (), SIR.BinaryOpsAllowed s ~ Void)
type Convertible ungrouped grouped =
    ( IsUngrouped ungrouped
    , IsGrouped grouped
    , SIR.DIdenStart ungrouped ~ SIR.DIdenStart grouped
    , SIR.VIdenStart ungrouped ~ SIR.VIdenStart grouped
    , SIR.TypeExprEvaled ungrouped ~ SIR.TypeExprEvaled grouped
    , SIR.TypeExprEvaledAsType ungrouped ~ SIR.TypeExprEvaledAsType grouped
    , SIR.PIdenStart ungrouped ~ SIR.PIdenStart grouped
    , SIR.PIdenResolved ungrouped ~ SIR.PIdenResolved grouped
    )

group :: Convertible ungrouped grouped => SIR.SIR ungrouped -> SIR.SIR grouped
group (SIR.SIR modules adts type_synonyms type_vars variables (SIR.CU root_module main_function)) =
    SIR.SIR
        (IDGen.run_id_gen ID.ExprID'InfixGroupGen (Arena.transformM group_module modules))
        (Arena.transform convert_adt adts)
        (Arena.transform convert_type_synonym type_synonyms)
        type_vars
        (Arena.transform convert_variable variables)
        (SIR.CU root_module main_function)
    where
        -- TODO: automate these functions too?
        convert_adt (SIR.ADT did name tyvars variants) = SIR.ADT did name tyvars (map convert_variant variants)
            where
                convert_variant (SIR.ADTVariant'Anon name id fields) = SIR.ADTVariant'Anon name id (map (\ (i, t) -> (i, convert_type_expr t)) fields)
                convert_variant (SIR.ADTVariant'Named name id fields) = SIR.ADTVariant'Named name id (map (\ (i, n, t) -> (i, n, convert_type_expr t)) fields)
        convert_type_synonym (SIR.TypeSynonym did name exp) = SIR.TypeSynonym did name (convert_type_expr exp)
        convert_variable (SIR.Variable varid tyinfo n) = SIR.Variable varid tyinfo n

group_module :: Convertible ungrouped grouped => SIR.Module ungrouped -> IDGen.IDGen ID.ExprID (SIR.Module grouped)
group_module (SIR.Module id bindings adts syns) = SIR.Module id <$> mapM group_binding bindings <*> pure adts <*> pure syns

group_binding :: Convertible ungrouped grouped => SIR.Binding ungrouped -> IDGen.IDGen ID.ExprID (SIR.Binding grouped)
group_binding (SIR.Binding pat eq_sp e) = SIR.Binding (convert_pattern pat) eq_sp <$> group_expr e

group_expr :: Convertible ungrouped grouped => SIR.Expr ungrouped -> IDGen.IDGen ID.ExprID (SIR.Expr grouped)
group_expr (SIR.Expr'Refer id () sp iden resolved) = pure $ SIR.Expr'Refer id () sp (convert_split_iden iden) resolved
group_expr (SIR.Expr'Char id () sp c) = pure $ SIR.Expr'Char id () sp c
group_expr (SIR.Expr'String id () sp t) = pure $ SIR.Expr'String id () sp t
group_expr (SIR.Expr'Int id () sp i) = pure $ SIR.Expr'Int id () sp i
group_expr (SIR.Expr'Float id () sp r) = pure $ SIR.Expr'Float id () sp r
group_expr (SIR.Expr'Bool id () sp b) = pure $ SIR.Expr'Bool id () sp b

group_expr (SIR.Expr'Tuple id () sp a b) = SIR.Expr'Tuple id () sp <$> group_expr a <*> group_expr b

group_expr (SIR.Expr'Lambda id () sp param body) = SIR.Expr'Lambda id () sp (convert_pattern param) <$> group_expr body

group_expr (SIR.Expr'Let id () sp bindings adts type_synonyms body) = SIR.Expr'Let id () sp <$> mapM group_binding bindings <*> pure adts <*> pure type_synonyms <*> group_expr body
group_expr (SIR.Expr'LetRec id () sp bindings adts type_synonyms body) = SIR.Expr'LetRec id () sp <$> mapM group_binding bindings <*> pure adts <*> pure type_synonyms <*> group_expr body

group_expr (SIR.Expr'BinaryOps _ () () _ first ops) =
    group_expr first >>= \ first ->
    g first ops 0 >>= \ (r, a) ->
    if null a then pure r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: Convertible ungrouped grouped => SIR.Expr grouped -> [(Span, SIR.OperatorRef ungrouped, Maybe SIR.ValueRef, SIR.Expr ungrouped)] -> Int -> IDGen.IDGen ID.ExprID (SIR.Expr grouped, [(Span, SIR.OperatorRef ungrouped, Maybe SIR.ValueRef, SIR.Expr ungrouped)])
        g left more@((first_op_span, first_op_iden, first_op, first_rhs):after_first_op) cur_precedence =
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
                        left' = SIR.Expr'Call call_with_rhs_id () (lhs_span <> rhs_span) (SIR.Expr'Call call_with_lhs_id () (lhs_span <> first_op_span) (SIR.Expr'Refer refer_to_op_id () first_op_span (convert_split_iden first_op_iden) first_op) left) rhs
                    in g left' after cur_precedence

                else pure (left, more)

        g left [] _ = pure (left, [])

group_expr (SIR.Expr'Call id () sp callee arg) = SIR.Expr'Call id () sp <$> group_expr callee <*> group_expr arg

group_expr (SIR.Expr'If id () sp if_sp cond true false) = SIR.Expr'If id () sp if_sp <$> group_expr cond <*> group_expr true <*> group_expr false
group_expr (SIR.Expr'Match id () sp match_tok_sp e arms) = SIR.Expr'Match id () sp match_tok_sp <$> group_expr e <*> mapM (\ (p, e) -> (convert_pattern p,) <$> group_expr e) arms

group_expr (SIR.Expr'Poison id () sp) = pure $ SIR.Expr'Poison id () sp

group_expr (SIR.Expr'Hole id () sp hid) = pure $ SIR.Expr'Hole id () sp hid

group_expr (SIR.Expr'TypeAnnotation id () sp annotation e) = SIR.Expr'TypeAnnotation id () sp (convert_type_expr_and_ty annotation) <$> group_expr e

group_expr (SIR.Expr'Forall id () sp names e) = SIR.Expr'Forall id () sp names <$> group_expr e
group_expr (SIR.Expr'TypeApply id () sp e args) = SIR.Expr'TypeApply id () sp <$> group_expr e <*> pure (convert_type_expr_and_ty args)

-- TODO: automate functions like this?
convert_type_expr_and_ty :: Convertible ungrouped grouped => (SIR.TypeExpr ungrouped, SIR.TypeExprEvaledAsType ungrouped) -> (SIR.TypeExpr grouped, SIR.TypeExprEvaledAsType grouped)
convert_type_expr_and_ty (tye, ty) = (convert_type_expr tye, ty)

convert_split_iden :: Convertible ungrouped grouped => SIR.SplitIdentifier start ungrouped -> SIR.SplitIdentifier start grouped
convert_split_iden (SIR.SplitIdentifier'Get texpr next) = SIR.SplitIdentifier'Get (convert_type_expr texpr) next
convert_split_iden (SIR.SplitIdentifier'Single single) = SIR.SplitIdentifier'Single single

convert_type_expr :: Convertible ungrouped grouped => SIR.TypeExpr ungrouped -> SIR.TypeExpr grouped
convert_type_expr (SIR.TypeExpr'Refer evaled sp var_key) = SIR.TypeExpr'Refer evaled sp var_key
convert_type_expr (SIR.TypeExpr'Get evaled sp parent name) = SIR.TypeExpr'Get evaled sp (convert_type_expr parent) name
convert_type_expr (SIR.TypeExpr'Tuple evaled sp a b) = SIR.TypeExpr'Tuple evaled sp (convert_type_expr a) (convert_type_expr b)
convert_type_expr (SIR.TypeExpr'Hole evaled type_info sp hiden) = SIR.TypeExpr'Hole evaled type_info sp hiden
convert_type_expr (SIR.TypeExpr'Function evaled sp a b) = SIR.TypeExpr'Function evaled sp (convert_type_expr a) (convert_type_expr b)
convert_type_expr (SIR.TypeExpr'Forall evaled sp tyvars res) = SIR.TypeExpr'Forall evaled sp tyvars (convert_type_expr res)
convert_type_expr (SIR.TypeExpr'Apply evaled sp c a) = SIR.TypeExpr'Apply evaled sp (convert_type_expr c) (convert_type_expr a)
convert_type_expr (SIR.TypeExpr'Wild evaled sp) = SIR.TypeExpr'Wild evaled sp
convert_type_expr (SIR.TypeExpr'Poison evaled sp) = SIR.TypeExpr'Poison evaled sp

convert_pattern :: Convertible ungrouped grouped => SIR.Pattern ungrouped -> SIR.Pattern grouped
convert_pattern (SIR.Pattern'Variable tyinfo sp var_key) = SIR.Pattern'Variable tyinfo sp var_key
convert_pattern (SIR.Pattern'Wildcard tyinfo sp) = SIR.Pattern'Wildcard tyinfo sp
convert_pattern (SIR.Pattern'Tuple tyinfo sp a b) = SIR.Pattern'Tuple tyinfo sp (convert_pattern a) (convert_pattern b)
convert_pattern (SIR.Pattern'Named tyinfo sp at var_key sub) = SIR.Pattern'Named tyinfo sp at var_key (convert_pattern sub)
convert_pattern (SIR.Pattern'AnonADTVariant tyinfo sp variant_iden variant_resolved tyapps fields) = SIR.Pattern'AnonADTVariant tyinfo sp (convert_split_iden variant_iden) variant_resolved tyapps (map convert_pattern fields)
convert_pattern (SIR.Pattern'NamedADTVariant tyinfo sp variant_iden variant_resolved tyapps fields) = SIR.Pattern'NamedADTVariant tyinfo sp (convert_split_iden variant_iden) variant_resolved tyapps (map (\ (n, f) -> (n, convert_pattern f)) fields)
convert_pattern (SIR.Pattern'Poison tyinfo sp) = SIR.Pattern'Poison tyinfo sp
