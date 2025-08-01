module UHF.Data.SIR.Visitor (ExprVisitor) where

import UHF.Prelude

import qualified UHF.Data.IR.ID as ID
import qualified UHF.Data.SIR as Stage
import UHF.Source.Span (Span)
import UHF.Data.SIR (ExprIdentifierRef, Expr, Pattern, OperatorRef, TypeExpr, HoleIdentifier, Binding)
import UHF.Data.IR.Keys (ADTKey, QuantVarKey)
import UHF.Data.IR.Type (TypeSynonymKey)

-- TODO: somehow make an overall SIR visitor that can cover all use cases

data ExprVisitor stage stage2 cx m res
    = ExprVisitor
    { visit_expr_refer :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> ExprIdentifierRef stage -> m (Expr stage2, res)
    , visit_expr_char :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> Char -> m (Expr stage2, res)
    , visit_expr_string :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> Text -> m (Expr stage2, res)
    , visit_expr_int :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> Integer -> m (Expr stage2, res)
    , visit_expr_float :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> Rational -> m (Expr stage2, res)
    , visit_expr_bool :: cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> Bool  -> m (Expr stage2, res)
    , visit_expr_tuple :: (cx -> cx -> m (Expr stage2, Expr stage2, res, res)) -> cx -> ID.ExprID -> Stage.TypeInfo stage -> Span -> m (Expr stage2, res)
    , visit_expr_lambda :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (Pattern stage) -> m (Expr stage2, res)
    , visit_expr_let :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (Stage.NameMapIndex stage) -> [Binding stage] -> [ADTKey] -> [TypeSynonymKey] -> m (Expr stage2, res)
    , visit_expr_let_rec :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (Stage.NameMapIndex stage) -> [Binding stage] -> [ADTKey] -> [TypeSynonymKey] -> m (Expr stage2, res)
    , visit_expr_binary_ops :: cx -> ID.ExprID -> (Stage.InfixGroupedKey stage) -> (Stage.TypeInfo stage) -> Span -> (Expr stage) -> [(Span, OperatorRef stage, Expr stage)] -> m (Expr stage2, res)
    , visit_expr_call :: (cx -> cx -> m (Expr stage2, Expr stage2, res, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> m (Expr stage2, res)
    , visit_expr_if :: (cx -> cx -> cx -> m (Expr stage2, Expr stage2, Expr stage2, res, res, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> Span -> m (Expr stage2, res)
    , visit_expr_match :: cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> Span -> (Expr stage) -> [(Stage.NameMapIndex stage, Pattern stage, Expr stage)] -> m (Expr stage2, res)
    , visit_expr_forall :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (Stage.NameMapIndex stage) -> (NonEmpty QuantVarKey) -> m (Expr stage2, res)
    , visit_expr_type_apply :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (TypeExpr stage, Stage.TypeExprEvaledAsTypeKey stage) -> m (Expr stage2, res)
    , visit_expr_type_annotation :: (cx -> m (Expr stage2, res)) -> cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> (TypeExpr stage, Stage.TypeExprEvaledAsTypeKey stage) -> m (Expr stage2, res)
    , visit_expr_hole :: cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> HoleIdentifier -> m (Expr stage2, res)
    , visit_expr_poison :: cx -> ID.ExprID -> (Stage.TypeInfo stage) -> Span -> m (Expr stage2, res)
    }
