{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UHF.Data.IR.IDGen
    ( IDGen
    , IDGenT
    , gen_id
    , run_id_gen_t
    , run_id_gen
    )
    where

import UHF.Util.Prelude

import qualified Data.Functor.Identity as Identity
import qualified Control.Monad.Trans.Class as Trans

newtype Unique = Unique Int deriving (Show, Eq, Ord)
newtype IDGenT id m r = IDGenT (StateT (Int, Int -> id) m r) deriving (Functor, Applicative, Monad, Trans.MonadTrans)
type IDGen id = IDGenT id Identity.Identity

gen_id :: Monad m => IDGenT id m id
gen_id = IDGenT $ StateT $ \ (i, make) -> pure (make i, (i + 1, make))

run_id_gen_t :: Monad m => (Int -> id) -> IDGenT id m r -> m r
run_id_gen_t make (IDGenT s) = evalStateT s (0, make)
run_id_gen :: (Int -> id) -> IDGen id r -> r
run_id_gen make = Identity.runIdentity . run_id_gen_t make
