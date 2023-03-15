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
import qualified Data.Char as Char
import qualified Numeric

data ModuleID = ModuleID [Text] deriving Show

-- TODO: remove unused things

data DeclID = DeclID DeclParent Text deriving Show
data DeclParent = DeclParent'Module ModuleID | DeclParent'Expr ExprID deriving Show

data ExprParent = ExprParent'Binding DeclParent Int | ExprParent'CaseArm ExprID Int | ExprParent'TupleDestructureL PatID | ExprParent'TupleDestructureR PatID | ExprParent'NamedRefer PatID deriving Show
data ExprID
    = ExprID ExprParent [ExprIDSegment]
    | ExprID'ANFIRGen Int
    | ExprID'RIRGen Int
    deriving Show
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
    | LambdaReferParam
    deriving Show

data PatParent = PatParent'Binding DeclParent Int | PatParent'CaseArm ExprID Int | PatParent'LambdaParam ExprID Int deriving Show
data PatID = PatID PatParent [PatIDSegment] deriving Show
data PatIDSegment = PatTupleItem Int | PatTupleRight | NamedPatOther deriving Show

data BoundValueParent = BVParent'Module ModuleID | BVParent'LambdaParam ExprID | BVParent'Let ExprID | BVParent'CaseArm ExprID Int deriving Show
data BoundValueID = BoundValueID BoundValueParent Text | BoundValueID'MadeUp Int | BoundValueID'MadeUpTupleLeft PatID | BoundValueID'MadeUpTupleRight PatID | BoundValueID'MadeUpLambdaParam ExprID deriving Show

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
        stringify' (GM (ModuleID [])) = "root"
        stringify' (GM (ModuleID segments)) = Text.intercalate "::" segments
        stringify' (GD (DeclID parent name)) = stringify_decl_parent parent <> "::" <> name
        stringify' (GE (ExprID parent segments)) = stringify_expr_parent parent <> Text.concat (map (("::"<>) . stringify_expr_segment) segments)
        stringify' (GP (PatID parent segments)) = stringify_pat_parent parent <> Text.concat (map (("::"<>) . stringify_pat_segment) segments)
        stringify' (GBV (BoundValueID bv_parent t)) = stringify_bv_parent bv_parent <> "::" <> t
        stringify' (GBV (BoundValueID'MadeUp i)) = "made_up_" <> show i
        stringify' (GBV (BoundValueID'MadeUpTupleLeft pat)) = stringify' (GP pat) <> "_left"
        stringify' (GBV (BoundValueID'MadeUpTupleRight pat)) = stringify' (GP pat) <> "_right"
        stringify' (GBV (BoundValueID'MadeUpLambdaParam ex)) = stringify' (GE ex) <> "_param"

        stringify_bv_parent (BVParent'Module mod) = stringify' (GM mod)
        stringify_bv_parent (BVParent'Let e) = stringify' (GE e)
        stringify_bv_parent (BVParent'LambdaParam e) = stringify' (GE e)
        stringify_bv_parent (BVParent'CaseArm e i) = stringify' (GE e) <> "::arm" <> show i

        stringify_decl_parent (DeclParent'Module mod) = stringify' (GM mod)
        stringify_decl_parent (DeclParent'Expr e) = stringify' (GE e)

        stringify_expr_parent (ExprParent'Binding decl_parent ind) = stringify_decl_parent decl_parent <> "::binding" <> show ind
        stringify_expr_parent (ExprParent'CaseArm expr ind) = stringify' (GE expr) <> "::arm" <> show ind
        stringify_expr_parent (ExprParent'TupleDestructureL pat) = stringify' (GP pat) <> "::destructure_l"
        stringify_expr_parent (ExprParent'TupleDestructureR pat) = stringify' (GP pat) <> "::destructure_r"
        stringify_expr_parent (ExprParent'NamedRefer pat) = stringify' (GP pat)

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
        stringify_expr_segment (LambdaReferParam) = "param"

        stringify_pat_segment (PatTupleItem i) = "item" <> show i
        stringify_pat_segment (PatTupleRight) = "right"
        stringify_pat_segment (NamedPatOther) = "other"

class Mangle m where
    mangle' :: m -> Text

instance Mangle GeneralID where
    mangle' (GM m) = "m" <> mangle' m
    mangle' (GD d) = "d" <> mangle' d
    mangle' (GE e) = "e" <> mangle' e
    mangle' (GP p) = "p" <> mangle' p
    mangle' (GBV bv) = "bv" <> mangle' bv

instance Mangle ModuleID where
    mangle' (ModuleID path) = mangle' path

instance Mangle DeclID where
    mangle' (DeclID parent name) = mangle' parent <> mangle' name
instance Mangle DeclParent where
    mangle' (DeclParent'Module m) = "m" <> mangle' m
    mangle' (DeclParent'Expr e) = "e" <> mangle' e

instance Mangle ExprID where
    mangle' (ExprID parent pieces) = "n" <> mangle' parent <> mangle' pieces
    mangle' (ExprID'ANFIRGen i) = "a" <> mangle' i
    mangle' (ExprID'RIRGen i) = "r" <> mangle' i
instance Mangle ExprParent where
    mangle' (ExprParent'Binding db ind) = "b" <> mangle' db <> mangle' ind
    mangle' (ExprParent'CaseArm e ind) = "c" <> mangle' e <> mangle' ind
    mangle' (ExprParent'TupleDestructureL p) = "l" <> mangle' p
    mangle' (ExprParent'TupleDestructureR p) = "r" <> mangle' p
    mangle' (ExprParent'NamedRefer p) = "n" <> mangle' p
instance Mangle ExprIDSegment where
    mangle' (ExprTupleItem i) = "t" <> mangle' i
    mangle' (LambdaParam) = "p"
    mangle' (LambdaBody) = "b"
    mangle' (LetResult) = "r"
    mangle' (BinaryOpsArg i) = "o" <> mangle' i
    mangle' (CallCallee) = "c"
    mangle' (CallArg i) = "a" <> mangle' i
    mangle' (IfCond) = "d" -- d is the letter after c
    mangle' (IfTrue) = "u" -- u is the letter after t
    mangle' (IfFalse) = "f"
    mangle' (CaseTest) = "v" -- v is the second letter after t
    mangle' (CaseArm i) = "e" <> mangle' i -- e is the fourth letter after a
    mangle' (TypeAnnotationExpr) = "w" -- w is the third letter after t
    mangle' (ExprTupleRight) = "s" -- s is the letter after r
    mangle' (LambdaReferParam) = "q" -- q is the letter after p

instance Mangle PatID where
    mangle' (PatID parent pieces) = mangle' parent <> mangle' pieces
instance Mangle PatParent where
    mangle' (PatParent'Binding db ind) = "b" <> mangle' db <> mangle' ind
    mangle' (PatParent'CaseArm e ind) = "c" <> mangle' e <> mangle' ind
    mangle' (PatParent'LambdaParam e ind) = "l" <> mangle' e <> mangle' ind
instance Mangle PatIDSegment where
    mangle' (PatTupleItem i) = "t" <> mangle' i
    mangle' (PatTupleRight) = "r"
    mangle' (NamedPatOther) = "o"

instance Mangle BoundValueID where
    mangle' (BoundValueID parent pieces) = "b" <> mangle' parent <> mangle' pieces
    mangle' (BoundValueID'MadeUp i) = "p" <> mangle' i
    mangle' (BoundValueID'MadeUpTupleLeft t) = "l" <> mangle' t
    mangle' (BoundValueID'MadeUpTupleRight t) = "r" <> mangle' t
    mangle' (BoundValueID'MadeUpLambdaParam e) = "m" <> mangle' e -- m is the letter after l

instance Mangle BoundValueParent where
    mangle' (BVParent'Module mod) = "m" <> mangle' mod
    mangle' (BVParent'LambdaParam lam) = "l" <> mangle' lam
    mangle' (BVParent'Let e) = "m" <> mangle' e -- m is the letter after l
    mangle' (BVParent'CaseArm e ind) = "c" <> mangle' e <> mangle' ind

instance Mangle a => Mangle [a] where
    mangle' things = show (length things) <> "_" <> Text.concat (map mangle' things)

instance Mangle Text where
    mangle' t = show (Text.length t) <> "_" <> Text.concatMap encode_char t
        where
            encode_char '_' = "__"
            encode_char c
                | Char.isAscii c = Text.singleton c
                | otherwise =
                    let code = Text.pack $ Numeric.showHex (Char.ord c) ""
                    in "_" <> Text.replicate (4 - Text.length code) "0" <> code

instance Mangle Int where
    mangle' i = mangle' (show i :: Text)

mangle :: ID i => i -> Text
mangle = mangle' . to_general_id
