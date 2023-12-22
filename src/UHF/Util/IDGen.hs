{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Util.IDGen
    ( IDGen
    , IDGenT
    , gen_id
    , run_id_gen_t
    , run_id_gen
    )
    where

import UHF.Prelude

import qualified Control.Monad.Fix as Fix
import qualified Data.Functor.Identity as Identity

newtype Unique = Unique Int deriving (Show, Eq, Ord)
newtype IDGenT id m r = IDGenT (StateT (Int, Int -> id) m r) deriving (Functor, Applicative, Monad, MonadTrans, Fix.MonadFix)
type IDGen id = IDGenT id Identity.Identity

gen_id :: Monad m => IDGenT id m id
gen_id = IDGenT $ StateT $ \ (i, make) -> pure (make i, (i + 1, make))

run_id_gen_t :: Monad m => (Int -> id) -> IDGenT id m r -> m r
run_id_gen_t make (IDGenT s) = evalStateT s (0, make)
run_id_gen :: (Int -> id) -> IDGen id r -> r
run_id_gen make = Identity.runIdentity . run_id_gen_t make
