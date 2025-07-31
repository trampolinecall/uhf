{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Data.SIR.ID
    ( GenT
    , Gen
    , run_t
    , run
    , ADTID
    , TypeSynonymID
    , ModuleID
    , VariableID
    , BindingID
    , TypeExprID
    , SplitIdentifierID
    , ExprID
    , PatternID
    , gen_adt_id
    , gen_type_synonym_id
    , gen_module_id
    , gen_variable_id
    , gen_binding_id
    , gen_type_expr_id
    , gen_split_identifier_id
    , gen_expr_id
    , gen_pattern_id
    ) where

import UHF.Prelude

import qualified Control.Monad.Fix as Fix
import qualified Data.Functor.Identity as Identity
import qualified UHF.Util.IDGen as UIDG

newtype GenT m r
    = GenT
        ( UIDG.IDGenT
            ADTID
            ( UIDG.IDGenT
                TypeSynonymID
                ( UIDG.IDGenT
                    ModuleID
                    ( UIDG.IDGenT VariableID (UIDG.IDGenT BindingID (UIDG.IDGenT TypeExprID (UIDG.IDGenT SplitIdentifierID (UIDG.IDGenT ExprID (UIDG.IDGenT PatternID m)))))
                    )
                )
            )
            r
        )
    deriving (Functor, Applicative, Monad, Fix.MonadFix)
type Gen = GenT Identity.Identity

instance MonadTrans GenT where
    lift = GenT . lift . lift . lift . lift . lift . lift . lift . lift . lift

run_t :: Monad m => GenT m r -> m r
run_t (GenT s) =
    UIDG.run_id_gen_t PatternID
        . UIDG.run_id_gen_t ExprID
        . UIDG.run_id_gen_t SplitIdentifierID
        . UIDG.run_id_gen_t TypeExprID
        . UIDG.run_id_gen_t BindingID
        . UIDG.run_id_gen_t VariableID
        . UIDG.run_id_gen_t ModuleID
        . UIDG.run_id_gen_t TypeSynonymID
        . UIDG.run_id_gen_t ADTID
        $ s
run :: Gen r -> r
run = Identity.runIdentity . run_t

gen_adt_id :: Monad m => GenT m ADTID
gen_adt_id = GenT UIDG.gen_id
gen_type_synonym_id :: Monad m => GenT m TypeSynonymID
gen_type_synonym_id = GenT $ lift UIDG.gen_id
gen_module_id :: Monad m => GenT m ModuleID
gen_module_id = GenT $ lift $ lift UIDG.gen_id
gen_variable_id :: Monad m => GenT m VariableID
gen_variable_id = GenT $ lift $ lift $ lift UIDG.gen_id
gen_binding_id :: Monad m => GenT m BindingID
gen_binding_id = GenT $ lift $ lift $ lift $ lift UIDG.gen_id
gen_type_expr_id :: Monad m => GenT m TypeExprID
gen_type_expr_id = GenT $ lift $ lift $ lift $ lift $ lift UIDG.gen_id
gen_split_identifier_id :: Monad m => GenT m SplitIdentifierID
gen_split_identifier_id = GenT $ lift $ lift $ lift $ lift $ lift $ lift UIDG.gen_id
gen_expr_id :: Monad m => GenT m ExprID
gen_expr_id = GenT $ lift $ lift $ lift $ lift $ lift $ lift $ lift UIDG.gen_id
gen_pattern_id :: Monad m => GenT m PatternID
gen_pattern_id = GenT $ lift $ lift $ lift $ lift $ lift $ lift $ lift $ lift UIDG.gen_id

newtype ADTID = ADTID Int deriving (Show, Eq, Ord)
newtype TypeSynonymID = TypeSynonymID Int deriving (Show, Eq, Ord)
newtype ModuleID = ModuleID Int deriving (Show, Eq, Ord)
newtype VariableID = VariableID Int deriving (Show, Eq, Ord)
newtype BindingID = BindingID Int deriving (Show, Eq, Ord)
newtype TypeExprID = TypeExprID Int deriving (Show, Eq, Ord)
newtype SplitIdentifierID = SplitIdentifierID Int deriving (Show, Eq, Ord)
newtype ExprID = ExprID Int deriving (Show, Eq, Ord)
newtype PatternID = PatternID Int deriving (Show, Eq, Ord)
