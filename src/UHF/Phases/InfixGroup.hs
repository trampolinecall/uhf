{-# LANGUAGE ConstraintKinds #-}

module UHF.Phases.InfixGroup (group) where

import UHF.Util.Prelude

import qualified Arena

import qualified UHF.IO.Located as Located
import UHF.IO.Located (Located)

import qualified UHF.Data.IR.SIR as SIR
import qualified UHF.Data.IR.Type as Type
import qualified UHF.Data.IR.IDGen as IDGen
import qualified UHF.Data.IR.ID as ID

type VIden = Located (Maybe SIR.BoundValueKey)

type IsUngrouped s = (SIR.VIden s ~ VIden, SIR.TypeInfo s ~ (), SIR.BinaryOpsAllowed s ~ ())
type IsGrouped s = (SIR.VIden s ~ VIden, SIR.TypeInfo s ~ (), SIR.BinaryOpsAllowed s ~ Void)
type Convertible ungrouped grouped = (IsUngrouped ungrouped, IsGrouped grouped, SIR.DIden ungrouped ~ SIR.DIden grouped, SIR.PIden ungrouped ~ SIR.PIden grouped, SIR.TypeExprTypeInfo ungrouped ~ SIR.TypeExprTypeInfo grouped)

group :: Convertible ungrouped grouped => SIR.SIR ungrouped -> SIR.SIR grouped
group (SIR.SIR decls modules adts type_synonyms type_vars bound_values mod) =
    SIR.SIR
        decls
        (IDGen.run_id_gen ID.ExprID'InfixGroupGen (Arena.transformM group_module modules))
        (Arena.transform convert_adt adts)
        (Arena.transform convert_type_synonym type_synonyms)
        type_vars
        (Arena.transform convert_bound_value bound_values)
        mod
    where
        -- TODO: automate these functions too?
        convert_adt (Type.ADT did name tyvars variants) = Type.ADT did name tyvars (map convert_variant variants)
            where
                convert_variant (Type.ADTVariant'Anon name id fields) = Type.ADTVariant'Anon name id (map (\ (i, t) -> (i, convert_type_expr t)) fields)
                convert_variant (Type.ADTVariant'Named name id fields) = Type.ADTVariant'Named name id (map (\ (i, n, t) -> (i, n, convert_type_expr t)) fields)
        convert_type_synonym (Type.TypeSynonym did name exp) = Type.TypeSynonym did name (convert_type_expr exp)
        convert_bound_value (SIR.BoundValue bvid tyinfo n) = SIR.BoundValue bvid tyinfo n
        convert_bound_value (SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp) = SIR.BoundValue'ADTVariant bvid id tyvars tyinfo sp

group_module :: Convertible ungrouped grouped => SIR.Module ungrouped -> IDGen.IDGen ID.ExprID (SIR.Module grouped)
group_module (SIR.Module id bindings adts syns) = SIR.Module id <$> mapM group_binding bindings <*> pure adts <*> pure syns

group_binding :: Convertible ungrouped grouped => SIR.Binding ungrouped -> IDGen.IDGen ID.ExprID (SIR.Binding grouped)
group_binding (SIR.Binding pat eq_sp e) = SIR.Binding (convert_pattern pat) eq_sp <$> group_expr e
group_binding (SIR.Binding'ADTVariant sp bvk tparams variant) = pure $ SIR.Binding'ADTVariant sp bvk tparams variant

group_expr :: Convertible ungrouped grouped => SIR.Expr ungrouped -> IDGen.IDGen ID.ExprID (SIR.Expr grouped)
group_expr (SIR.Expr'Identifier id () sp iden) = pure $ SIR.Expr'Identifier id () sp iden
group_expr (SIR.Expr'Char id () sp c) = pure $ SIR.Expr'Char id () sp c
group_expr (SIR.Expr'String id () sp t) = pure $ SIR.Expr'String id () sp t
group_expr (SIR.Expr'Int id () sp i) = pure $ SIR.Expr'Int id () sp i
group_expr (SIR.Expr'Float id () sp r) = pure $ SIR.Expr'Float id () sp r
group_expr (SIR.Expr'Bool id () sp b) = pure $ SIR.Expr'Bool id () sp b

group_expr (SIR.Expr'Tuple id () sp a b) = SIR.Expr'Tuple id () sp <$> group_expr a <*> group_expr b

group_expr (SIR.Expr'Lambda id () sp param body) = SIR.Expr'Lambda id () sp (convert_pattern param) <$> group_expr body

group_expr (SIR.Expr'Let id () sp bindings body) = SIR.Expr'Let id () sp <$> mapM group_binding bindings <*> group_expr body
group_expr (SIR.Expr'LetRec id () sp bindings body) = SIR.Expr'LetRec id () sp <$> mapM group_binding bindings <*> group_expr body

group_expr (SIR.Expr'BinaryOps _ () () _ first ops) =
    group_expr first >>= \ first ->
    g first ops 0 >>= \ (r, a) ->
    if null a then pure r else error "internal error: still operations to group after grouping binary ops"
    where
        -- TODO: test this
        g :: Convertible ungrouped grouped => SIR.Expr grouped -> [(Located (Maybe SIR.BoundValueKey), SIR.Expr ungrouped)] -> Int -> IDGen.IDGen ID.ExprID (SIR.Expr grouped, [(Located (Maybe SIR.BoundValueKey), SIR.Expr ungrouped)])
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
group_expr (SIR.Expr'Match id () sp match_tok_sp e arms) = SIR.Expr'Match id () sp match_tok_sp <$> group_expr e <*> mapM (\ (p, e) -> (convert_pattern p,) <$> group_expr e) arms

group_expr (SIR.Expr'Poison id () sp) = pure $ SIR.Expr'Poison id () sp

group_expr (SIR.Expr'Hole id () sp hid) = pure $ SIR.Expr'Hole id () sp hid

group_expr (SIR.Expr'TypeAnnotation id () sp annotation e) = SIR.Expr'TypeAnnotation id () sp (convert_type_expr annotation) <$> group_expr e

group_expr (SIR.Expr'Forall id () sp names e) = SIR.Expr'Forall id () sp names <$> group_expr e
group_expr (SIR.Expr'TypeApply id () sp e args) = SIR.Expr'TypeApply id () sp <$> group_expr e <*> pure (convert_type_expr args)

-- TODO: automate functions like this?
convert_type_expr :: Convertible ungrouped grouped => SIR.TypeExpr ungrouped -> SIR.TypeExpr grouped
convert_type_expr (SIR.TypeExpr'Identifier tyinfo sp bvk) = SIR.TypeExpr'Identifier tyinfo sp bvk
convert_type_expr (SIR.TypeExpr'Tuple tyinfo a b) = SIR.TypeExpr'Tuple tyinfo (convert_type_expr a) (convert_type_expr b)
convert_type_expr (SIR.TypeExpr'Hole tyinfo sp hiden) = SIR.TypeExpr'Hole tyinfo sp hiden
convert_type_expr (SIR.TypeExpr'Function tyinfo sp a b) = SIR.TypeExpr'Function tyinfo sp (convert_type_expr a) (convert_type_expr b)
convert_type_expr (SIR.TypeExpr'Forall tyinfo tyvars res) = SIR.TypeExpr'Forall tyinfo tyvars (convert_type_expr res)
convert_type_expr (SIR.TypeExpr'Apply tyinfo sp c a) = SIR.TypeExpr'Apply tyinfo sp (convert_type_expr c) (convert_type_expr a)
convert_type_expr (SIR.TypeExpr'Wild tyinfo sp) = SIR.TypeExpr'Wild tyinfo sp
convert_type_expr (SIR.TypeExpr'Poison tyinfo sp) = SIR.TypeExpr'Poison tyinfo sp

convert_pattern :: Convertible ungrouped grouped => SIR.Pattern ungrouped -> SIR.Pattern grouped
convert_pattern (SIR.Pattern'Identifier tyinfo sp bvk) = SIR.Pattern'Identifier tyinfo sp bvk
convert_pattern (SIR.Pattern'Wildcard tyinfo sp) = SIR.Pattern'Wildcard tyinfo sp
convert_pattern (SIR.Pattern'Tuple tyinfo sp a b) = SIR.Pattern'Tuple tyinfo sp (convert_pattern a) (convert_pattern b)
convert_pattern (SIR.Pattern'Named tyinfo sp at bvk sub) = SIR.Pattern'Named tyinfo sp at bvk (convert_pattern sub)
convert_pattern (SIR.Pattern'AnonADTVariant tyinfo sp variant tyapps fields) = SIR.Pattern'AnonADTVariant tyinfo sp variant tyapps (map convert_pattern fields)
convert_pattern (SIR.Pattern'NamedADTVariant tyinfo sp variant tyapps fields) = SIR.Pattern'NamedADTVariant tyinfo sp variant tyapps (map (\ (n, f) -> (n, convert_pattern f)) fields)
convert_pattern (SIR.Pattern'Poison tyinfo sp) = SIR.Pattern'Poison tyinfo sp
