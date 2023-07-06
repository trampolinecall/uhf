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

data ExprID = ExprID'SIRGen Int | ExprID'InfixGroupGen Int | ExprID'RIRGen Int | ExprID'ANFIRGen Int deriving Show

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
        stringify' (GE (ExprID'SIRGen i)) = "s" <> show i
        stringify' (GE (ExprID'RIRGen i)) = "r" <> show i
        stringify' (GE (ExprID'ANFIRGen i)) = "a" <> show i
        stringify' (GE (ExprID'InfixGroupGen i)) = "i" <> show i
        stringify' (GBV (BoundValueID bv_parent t)) = stringify_bv_parent bv_parent <> "::" <> t
        stringify' (GBV (BoundValueID'RIRMadeUp i)) = "rir_" <> show i

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
    mangle' (ExprID'SIRGen i) = "s" <> mangle' i
    mangle' (ExprID'InfixGroupGen i) = "i" <> mangle' i
    mangle' (ExprID'RIRGen i) = "r" <> mangle' i
    mangle' (ExprID'ANFIRGen i) = "a" <> mangle' i

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
    mangle' i = mangle' (show i :: Text)

mangle :: ID i => i -> Text
mangle = mangle' . to_general_id
