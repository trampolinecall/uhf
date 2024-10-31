{-# LANGUAGE FlexibleContexts #-}

module UHF.Data.IR.ID
    ( ModuleID (..)
    , DeclID (..)
    , DeclParent (..)
    , ExprID (..)
    , IntrinsicBVID (..)
    , VariableParent (..)
    , VariableID (..)
    , ADTVariantID (..)
    , ADTFieldID (..)

    , stringify
    , mangle
    ) where

import UHF.Prelude

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Numeric

data ModuleID = ModuleID [Text] | ModuleID'Root deriving Show

data DeclID = DeclID DeclParent Text deriving Show
data DeclParent = DeclParent'Module ModuleID | DeclParent'Let ExprID | DeclParent'Where ExprID deriving Show

data ExprID
    = ExprID'ANFIRGen Int
    | ExprID'BinaryOperand ExprID Int
    | ExprID'CallArgOf ExprID
    | ExprID'CallCalleeIn ExprID
    | ExprID'CallEnclosing ExprID
    | ExprID'ForallResult ExprID
    | ExprID'IfCond ExprID
    | ExprID'IfFalse ExprID
    | ExprID'IfTrue ExprID
    | ExprID'InfixGroupGen Int
    | ExprID'InitializerOf DeclParent Int
    | ExprID'LambdaBodyOf ExprID
    | ExprID'LetResultOf ExprID
    | ExprID'WhereResultOf ExprID
    | ExprID'MatchArm ExprID Int
    | ExprID'MatchScrutinee ExprID
    | ExprID'RIRGen Int
    | ExprID'TupleFirstOf ExprID
    | ExprID'TupleSecondOf ExprID
    | ExprID'TypeAnnotationSubject ExprID
    | ExprID'TypeApplyFirst ExprID
    | ExprID'TypeApplyOn ExprID
    deriving Show

data IntrinsicBVID = IntrinsicBVID Text

data VariableParent = VarParent'Module ModuleID | VarParent'LambdaParam ExprID | VarParent'Let ExprID | VarParent'Where ExprID | VarParent'MatchArm ExprID Int deriving Show
data VariableID = VariableID VariableParent Text | VariableID'RIRMadeUp Int deriving Show

data ADTVariantID = ADTVariantID DeclID Text deriving Show
data ADTFieldID = ADTFieldID ADTVariantID Text deriving Show

data GeneralID
    = GM ModuleID
    | GD DeclID
    | GE ExprID
    | GV VariableID
    | GADTV ADTVariantID
    | GADTF ADTFieldID
    | GIBVID IntrinsicBVID

class ID i where
    to_general_id :: i -> GeneralID

instance ID ModuleID where
    to_general_id = GM
instance ID DeclID where
    to_general_id = GD
instance ID ExprID where
    to_general_id = GE
instance ID VariableID where
    to_general_id = GV
instance ID ADTVariantID where
    to_general_id = GADTV
instance ID ADTFieldID where
    to_general_id = GADTF
instance ID IntrinsicBVID where
    to_general_id = GIBVID

stringify :: ID i => i -> Text
stringify = stringify' . to_general_id
    where
        stringify' (GM ModuleID'Root) = "root"
        stringify' (GM (ModuleID segments)) = Text.intercalate "::" segments
        stringify' (GD (DeclID parent name)) = stringify_decl_parent parent <> "::" <> name
        stringify' (GE e) = "expr_" <> stringify_expr_id e
        stringify' (GV (VariableID var_parent t)) = stringify_var_parent var_parent <> "::" <> t
        stringify' (GV (VariableID'RIRMadeUp i)) = "rir_" <> show i
        stringify' (GADTV (ADTVariantID adt_decl name)) = stringify' (GD adt_decl) <> "::" <> name
        stringify' (GADTF (ADTFieldID variant_id name)) = stringify' (GADTV variant_id) <> "::" <> name
        stringify' (GIBVID (IntrinsicBVID name)) = "uhf_intrinsics::" <> name

        stringify_expr_id (ExprID'InitializerOf parent ind) = "initializer_of_" <> stringify_decl_parent parent <> "_" <> show ind

        stringify_expr_id (ExprID'BinaryOperand e i) = "binop" <> show i <> "_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallArgOf e) = "callarg_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallCalleeIn e) = "callcallee_" <> stringify_expr_id e
        stringify_expr_id (ExprID'CallEnclosing e) = "callenc_" <> stringify_expr_id e
        stringify_expr_id (ExprID'MatchArm e i) = "match_arm" <> show i <> "_" <> stringify_expr_id e
        stringify_expr_id (ExprID'MatchScrutinee e) = "mscru_" <> stringify_expr_id e
        stringify_expr_id (ExprID'ForallResult e) = "forallres_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfCond e) = "ifcond_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfFalse e) = "iffalse_" <> stringify_expr_id e
        stringify_expr_id (ExprID'IfTrue e) = "iftrue_" <> stringify_expr_id e
        stringify_expr_id (ExprID'LambdaBodyOf e) = "lbody_" <> stringify_expr_id e
        stringify_expr_id (ExprID'LetResultOf e) = "letres_" <> stringify_expr_id e
        stringify_expr_id (ExprID'WhereResultOf e) = "whereres_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TupleFirstOf e) = "tuple1_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TupleSecondOf e) = "tuple2_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeAnnotationSubject e) = "tann_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeApplyOn e) = "tapp_" <> stringify_expr_id e
        stringify_expr_id (ExprID'TypeApplyFirst e) = "tappf_" <> stringify_expr_id e

        stringify_expr_id (ExprID'RIRGen i) = "r" <> show i
        stringify_expr_id (ExprID'ANFIRGen i) = "a" <> show i
        stringify_expr_id (ExprID'InfixGroupGen i) = "i" <> show i

        stringify_var_parent (VarParent'Module mod) = stringify' (GM mod)
        stringify_var_parent (VarParent'Let e) = stringify' (GE e)
        stringify_var_parent (VarParent'Where e) = stringify' (GE e)
        stringify_var_parent (VarParent'LambdaParam e) = stringify' (GE e)
        stringify_var_parent (VarParent'MatchArm e i) = stringify' (GE e) <> "::arm" <> show i

        stringify_decl_parent (DeclParent'Module mod) = stringify' (GM mod)
        stringify_decl_parent (DeclParent'Let e) = stringify' (GE e)
        stringify_decl_parent (DeclParent'Where e) = stringify' (GE e)

class Mangle m where
    mangle' :: m -> Text

instance Mangle GeneralID where
    mangle' (GM m) = "m" <> mangle' m
    mangle' (GD d) = "d" <> mangle' d
    mangle' (GE e) = "e" <> mangle' e
    mangle' (GV var) = "v" <> mangle' var
    mangle' (GADTV adtv) = "a" <> mangle' adtv
    mangle' (GADTF adtf) = "f" <> mangle' adtf
    mangle' (GIBVID i) = "__uhf_intrinsic_" <> mangle' i

instance Mangle ModuleID where
    mangle' ModuleID'Root = "r"
    mangle' (ModuleID path) = "m" <> mangle' path

instance Mangle DeclID where
    mangle' (DeclID parent name) = mangle' parent <> mangle' name
instance Mangle DeclParent where
    mangle' (DeclParent'Module m) = "m" <> mangle' m
    mangle' (DeclParent'Let e) = "l" <> mangle' e
    mangle' (DeclParent'Where e) = "w" <> mangle' e

instance Mangle ExprID where
    mangle' (ExprID'ANFIRGen i) = "a" <> mangle' i
    mangle' (ExprID'BinaryOperand e i) = "b" <> mangle' i <> mangle' e
    mangle' (ExprID'CallArgOf e) = "c" <> mangle' e
    mangle' (ExprID'CallCalleeIn e) = "d" <> mangle' e
    mangle' (ExprID'CallEnclosing e) = "e" <> mangle' e
    mangle' (ExprID'ForallResult e) = "f" <> mangle' e
    mangle' (ExprID'IfCond e) = "i" <> mangle' e
    mangle' (ExprID'IfFalse e) = "j" <> mangle' e
    mangle' (ExprID'IfTrue e) = "k" <> mangle' e
    mangle' (ExprID'InfixGroupGen i) = "l" <> mangle' i
    mangle' (ExprID'InitializerOf parent ind) = "m" <> mangle' parent <> mangle' ind
    mangle' (ExprID'LambdaBodyOf e) = "n" <> mangle' e
    mangle' (ExprID'LetResultOf e) = "o" <> mangle' e
    mangle' (ExprID'MatchArm e i) = "p" <> mangle' e <> mangle' i
    mangle' (ExprID'MatchScrutinee e) = "q" <> mangle' e
    mangle' (ExprID'RIRGen i) = "r" <> mangle' i
    mangle' (ExprID'TupleFirstOf e) = "t" <> mangle' e
    mangle' (ExprID'TupleSecondOf e) = "u" <> mangle' e
    mangle' (ExprID'TypeAnnotationSubject e) = "v" <> mangle' e
    mangle' (ExprID'TypeApplyFirst e) = "w" <> mangle' e
    mangle' (ExprID'TypeApplyOn e) = "x" <> mangle' e
    mangle' (ExprID'WhereResultOf e) = "y" <> mangle' e

instance Mangle IntrinsicBVID where
    mangle' (IntrinsicBVID name) = name -- this cannot be ambiguous because there are only a number of hardcoded names for intrinsics

instance Mangle VariableID where
    mangle' (VariableID parent pieces) = "b" <> mangle' parent <> mangle' pieces
    mangle' (VariableID'RIRMadeUp i) = "r" <> mangle' i

instance Mangle VariableParent where
    mangle' (VarParent'LambdaParam lam) = "l" <> mangle' lam
    mangle' (VarParent'Let e) = "m" <> mangle' e
    mangle' (VarParent'MatchArm e ind) = "n" <> mangle' e <> mangle' ind
    mangle' (VarParent'Module mod) = "o" <> mangle' mod
    mangle' (VarParent'Where e) = "w" <> mangle' e

instance Mangle ADTVariantID where
    mangle' (ADTVariantID adt_decl variant_name) = mangle' adt_decl <> mangle' variant_name

instance Mangle ADTFieldID where
    mangle' (ADTFieldID variant field_name) = mangle' variant <> mangle' field_name

instance Mangle a => Mangle [a] where
    mangle' things = mangle' (length things) <> Text.concat (map mangle' things)

instance Mangle Text where
    mangle' t = mangle' (Text.length t) <> Text.concatMap mangle' t

instance Mangle Char where
    mangle' '_' = "__"
    mangle' c
        | Char.isAscii c = Text.singleton c
        | otherwise =
            let code = Text.pack $ Numeric.showHex (Char.ord c) ""
            in "_" <> Text.replicate (4 - Text.length code) "0" <> code

instance Mangle Int where
    mangle' i = show i <> "_"

mangle :: ID i => i -> Text
mangle = mangle' . to_general_id
