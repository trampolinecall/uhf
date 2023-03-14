{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module UHF.Data.IR.ID
    ( ModuleID (..)
    , DeclID (..)
    , DeclParent (..)
    , ExprParent (..)
    , ExprID (..)
    , ExprIDSegment (..)
    , PatParent (..)
    , PatID (..)
    , PatIDSegment (..)
    , BoundValueParent (..)
    , BoundValueID (..)

    , add

    , stringify
    , mangle
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text

data ModuleID = ModuleID [Text] deriving Show

-- TODO: remove unused things

data DeclID = DeclID DeclParent Text deriving Show
data DeclParent = DeclParent'Module ModuleID | DeclParent'Expr ExprID deriving Show

data ExprParent = ExprParent'Binding DeclParent Int | ExprParent'CaseArm ExprID Int deriving Show
data ExprID = ExprID ExprParent [ExprIDSegment] deriving Show
data ExprIDSegment
    = ExprTupleItem Int
    | LambdaParam
    | LambdaBody
    | LetResult
    | BinaryOpsArg Int
    | CallCallee
    | CallArg Int
    | IfCond
    | IfTrue
    | IfFalse
    | CaseTest
    | CaseArm Int
    | TypeAnnotationExpr
    | ExprTupleRight
    | LambdaLet
    | LambdaRefParam
    deriving Show

data PatParent = PatParent'Binding DeclParent Int | PatParent'CaseArm ExprID Int | PatParent'LambdaParam ExprID Int deriving Show
data PatID = PatID PatParent [PatIDSegment] deriving Show
data PatIDSegment = PatTupleItem Int | PatTupleRight | NamedPatOther deriving Show

data BoundValueParent = BVParent'Module ModuleID | BVParent'LambdaParam ExprID | BVParent'Let ExprID | BVParent'CaseArm ExprID Int deriving Show
data BoundValueID = BoundValueID BoundValueParent Text | BoundValueID'MadeUp PatID | BoundValueID'MadeUpTupleLeft PatID | BoundValueID'MadeUpTupleRight PatID | BoundValueID'MadeUpLambdaParam ExprID deriving Show

data GeneralID
    = GM ModuleID
    | GD DeclID
    | GE ExprID
    | GP PatID
    | GBV BoundValueID

class Add i seg where
    add :: seg -> i -> i

instance Add ExprID ExprIDSegment where
    add seg (ExprID bindings segs) = ExprID bindings (segs <> [seg])
instance Add PatID PatIDSegment where
    add seg (PatID bindings segs) = PatID bindings (segs <> [seg])

class ID i where
    to_general_id :: i -> GeneralID

instance ID ModuleID where
    to_general_id = GM
instance ID DeclID where
    to_general_id = GD
instance ID ExprID where
    to_general_id = GE
instance ID PatID where
    to_general_id = GP
instance ID BoundValueID where
    to_general_id = GBV

stringify :: ID i => i -> Text
stringify = stringify' . to_general_id
    where
        stringify' (GM (ModuleID segments)) = Text.intercalate "::" segments
        stringify' (GD (DeclID parent name)) = stringify_decl_parent parent <> "::" <> name
        stringify' (GE (ExprID parent segments)) = stringify_expr_parent parent <> Text.concat (map (("::"<>) . stringify_expr_segment) segments)
        stringify' (GP (PatID parent segments)) = stringify_pat_parent parent <> Text.concat (map (("::"<>) . stringify_pat_segment) segments)
        stringify' (GBV (BoundValueID bv_parent t)) = stringify_bv_parent bv_parent <> "::" <> t
        stringify' (GBV (BoundValueID'MadeUp pat)) = "bv" <> stringify' (GP pat)
        stringify' (GBV (BoundValueID'MadeUpTupleLeft pat)) = "bv" <> stringify' (GP pat) <> "::left"
        stringify' (GBV (BoundValueID'MadeUpTupleRight pat)) = "bv" <> stringify' (GP pat) <> "::right"
        stringify' (GBV (BoundValueID'MadeUpLambdaParam ex)) = "bv_e" <> stringify' (GE ex) <> "::param"

        stringify_bv_parent (BVParent'Module mod) = stringify' (GM mod)
        stringify_bv_parent (BVParent'Let e) = stringify' (GE e)
        stringify_bv_parent (BVParent'LambdaParam e) = stringify' (GE e)
        stringify_bv_parent (BVParent'CaseArm e i) = stringify' (GE e) <> "::arm" <> show i

        stringify_decl_parent (DeclParent'Module mod) = stringify' (GM mod)
        stringify_decl_parent (DeclParent'Expr e) = stringify' (GE e)

        stringify_expr_parent (ExprParent'Binding decl_parent ind) = stringify_decl_parent decl_parent <> "::binding" <> show ind
        stringify_expr_parent (ExprParent'CaseArm expr ind) = stringify' (GE expr) <> "::arm" <> show ind

        stringify_pat_parent (PatParent'Binding decl_parent ind) = stringify_decl_parent decl_parent <> "::binding" <> show ind
        stringify_pat_parent (PatParent'CaseArm expr ind) = stringify' (GE expr) <> "::arm" <> show ind
        stringify_pat_parent (PatParent'LambdaParam expr ind) = stringify' (GE expr) <> "::param" <> show ind

        stringify_expr_segment (ExprTupleItem i) = "tuple_item" <> show i
        stringify_expr_segment (LambdaParam) = "param"
        stringify_expr_segment (LambdaBody) = "body"
        stringify_expr_segment (LetResult) = "result"
        stringify_expr_segment (BinaryOpsArg i) = "arg" <> show i
        stringify_expr_segment (CallCallee) = "callee"
        stringify_expr_segment (CallArg i) = "arg" <> show i
        stringify_expr_segment (IfCond) = "cond"
        stringify_expr_segment (IfTrue) = "true_branch"
        stringify_expr_segment (IfFalse) = "false_branch"
        stringify_expr_segment (CaseTest) = "test"
        stringify_expr_segment (CaseArm i) = "arm" <> show i
        stringify_expr_segment (TypeAnnotationExpr) = "expr"
        stringify_expr_segment (ExprTupleRight) = "right"
        stringify_expr_segment (LambdaLet) = "let"
        stringify_expr_segment (LambdaRefParam) = "param"

        stringify_pat_segment (PatTupleItem i) = "item" <> show i
        stringify_pat_segment (PatTupleRight) = "right"
        stringify_pat_segment (NamedPatOther) = "other"

mangle :: ID i => i -> Text
mangle = mangle' . to_general_id
    where
        mangle' = todo
