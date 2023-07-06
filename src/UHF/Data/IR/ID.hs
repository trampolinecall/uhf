{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.IR.ID
    ( ModuleID (..)
    , DeclID (..)
    , DeclParent (..)
    , ExprID (..)
    , BoundValueParent (..)
    , BoundValueID (..)

    , stringify
    , mangle
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text
import qualified Data.Char as Char
import qualified Numeric

newtype ModuleID = ModuleID [Text] deriving Show

data DeclID = DeclID DeclParent Text deriving Show
data DeclParent = DeclParent'Module ModuleID | DeclParent'Let ExprID deriving Show

data ExprID
    = ExprID'InitializerOf DeclParent Int
    | ExprID'BinaryOperand ExprID Int
    | ExprID'CallCalleeIn ExprID
    | ExprID'CallArgOf ExprID
    | ExprID'CallEnclosing ExprID
    | ExprID'CaseArm ExprID Int
    | ExprID'CaseScrutinee ExprID
    | ExprID'ForallResult ExprID
    | ExprID'IfCond ExprID
    | ExprID'IfFalse ExprID
    | ExprID'IfTrue ExprID
    | ExprID'LambdaBodyOf ExprID
    | ExprID'LetResultOf ExprID
    | ExprID'TupleFirstOf ExprID
    | ExprID'TupleSecondOf ExprID
    | ExprID'TypeAnnotationSubject ExprID
    | ExprID'TypeApplyOn ExprID
    | ExprID'TypeApplyFirst ExprID
    | ExprID'InfixGroupGen Int
    | ExprID'RIRGen Int
    | ExprID'ANFIRGen Int
    deriving Show

data BoundValueParent = BVParent'Module ModuleID | BVParent'LambdaParam ExprID | BVParent'Let ExprID | BVParent'CaseArm ExprID Int deriving Show
data BoundValueID = BoundValueID BoundValueParent Text | BoundValueID'RIRMadeUp Int deriving Show

data GeneralID
    = GM ModuleID
    | GD DeclID
    | GE ExprID
    | GBV BoundValueID

class ID i where
    to_general_id :: i -> GeneralID

instance ID ModuleID where
    to_general_id = GM
instance ID DeclID where
    to_general_id = GD
instance ID ExprID where
    to_general_id = GE
instance ID BoundValueID where
    to_general_id = GBV

stringify :: ID i => i -> Text
stringify = stringify' . to_general_id
    where
        stringify' (GM (ModuleID [])) = "root"
        stringify' (GM (ModuleID segments)) = Text.intercalate "::" segments
        stringify' (GD (DeclID parent name)) = stringify_decl_parent parent <> "::" <> name
        stringify' (GE e) = "expr_" <> stringify_expr_id e
        stringify' (GBV (BoundValueID bv_parent t)) = stringify_bv_parent bv_parent <> "::" <> t
        stringify' (GBV (BoundValueID'RIRMadeUp i)) = "rir_" <> show i

        stringify_expr_id (ExprID'InitializerOf parent ind) = "initializer_of_" <> stringify_decl_parent parent <> "_" <> show ind

        stringify_expr_id (ExprID'BinaryOperand e i) = "binop" <> show i <> "_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallArgOf e) = "callarg_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallCalleeIn e) = "callcallee_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallEnclosing e) = "callenc_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CaseArm e i) = "case_arm" <> show i <> "_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CaseScrutinee e) = "cscru_" <> stringify_expr_id e
        stringify_expr_id (ExprID'ForallResult e) = "forallres_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfCond e) = "ifcond_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfFalse e) = "iffalse_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfTrue e) = "iftrue_" <> stringify_expr_id e
        stringify_expr_id (ExprID'LambdaBodyOf e) = "lbody_" <> stringify_expr_id e
        stringify_expr_id (ExprID'LetResultOf e) = "letres_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TupleFirstOf e) = "tuple1_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TupleSecondOf e) = "tuple2_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeAnnotationSubject e) = "tann_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeApplyOn e) = "tapp_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeApplyFirst e) = "tappf_" <> stringify_expr_id e

        stringify_expr_id (ExprID'RIRGen i) = "r" <> show i
        stringify_expr_id (ExprID'ANFIRGen i) = "a" <> show i
        stringify_expr_id (ExprID'InfixGroupGen i) = "i" <> show i

        stringify_bv_parent (BVParent'Module mod) = stringify' (GM mod)
        stringify_bv_parent (BVParent'Let e) = stringify' (GE e)
        stringify_bv_parent (BVParent'LambdaParam e) = stringify' (GE e)
        stringify_bv_parent (BVParent'CaseArm e i) = stringify' (GE e) <> "::arm" <> show i

        stringify_decl_parent (DeclParent'Module mod) = stringify' (GM mod)
        stringify_decl_parent (DeclParent'Let e) = stringify' (GE e)

class Mangle m where
    mangle' :: m -> Text

instance Mangle GeneralID where
    mangle' (GM m) = "m" <> mangle' m
    mangle' (GD d) = "d" <> mangle' d
    mangle' (GE e) = "e" <> mangle' e
    mangle' (GBV bv) = "b" <> mangle' bv

instance Mangle ModuleID where
    mangle' (ModuleID path) = mangle' path

instance Mangle DeclID where
    mangle' (DeclID parent name) = mangle' parent <> mangle' name
instance Mangle DeclParent where
    mangle' (DeclParent'Module m) = "m" <> mangle' m
    mangle' (DeclParent'Let e) = "e" <> mangle' e

instance Mangle ExprID where
    -- TODO: decide on better leters
    mangle' (ExprID'ANFIRGen i) = "a" <> mangle' i
    mangle' (ExprID'BinaryOperand e i) = "b" <> mangle' i <> mangle' e
    mangle' (ExprID'CallEnclosing e) = "c" <> mangle' e
    mangle' (ExprID'CaseArm e i) = "d" <> mangle' e <> mangle' i
    mangle' (ExprID'CallArgOf e) = "e" <> mangle' e
    mangle' (ExprID'ForallResult e) = "f" <> mangle' e
    mangle' (ExprID'CaseScrutinee e) = "g" <> mangle' e
    mangle' (ExprID'CallCalleeIn e) = "h" <> mangle' e
    mangle' (ExprID'InfixGroupGen i) = "i" <> mangle' i
    mangle' (ExprID'InitializerOf parent ind) = "j" <> mangle' parent <> mangle' ind
    mangle' (ExprID'IfCond e) = "k" <> mangle' e
    mangle' (ExprID'LambdaBodyOf e) = "l" <> mangle' e
    mangle' (ExprID'LetResultOf e) = "m" <> mangle' e
    mangle' (ExprID'IfTrue e) = "n" <> mangle' e
    mangle' (ExprID'IfFalse e) = "o" <> mangle' e
    mangle' (ExprID'RIRGen i) = "r" <> mangle' i
    mangle' (ExprID'TupleFirstOf e) = "t" <> mangle' e
    mangle' (ExprID'TupleSecondOf e) = "u" <> mangle' e
    mangle' (ExprID'TypeApplyOn e) = "v" <> mangle' e
    mangle' (ExprID'TypeAnnotationSubject e) = "w" <> mangle' e
    mangle' (ExprID'TypeApplyFirst e) = "x" <> mangle' e

instance Mangle BoundValueID where
    mangle' (BoundValueID parent pieces) = "b" <> mangle' parent <> mangle' pieces
    mangle' (BoundValueID'RIRMadeUp i) = "r" <> mangle' i

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
    mangle' i = show i <> "_"

mangle :: ID i => i -> Text
mangle = mangle' . to_general_id
