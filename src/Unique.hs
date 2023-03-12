{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Unique
    ( Unique
    , UniqueMaker
    , UniqueMakerT
    , make_unique
    , run_unique_maker_t
    , run_unique_maker
    )
    where

import UHF.Util.Prelude

import qualified Data.Functor.Identity as Identity
import qualified Control.Monad.Trans.Class as Trans

newtype Unique = Unique Int deriving (Show, Eq, Ord)
newtype UniqueMakerT m r = UniqueMakerT { un_unique_maker :: StateT Int m r } deriving (Functor, Applicative, Monad, Trans.MonadTrans)
type UniqueMaker = UniqueMakerT Identity.Identity

make_unique :: Monad m => UniqueMakerT m Unique
make_unique = UniqueMakerT $ StateT $ \ u -> pure (Unique u, (u + 1) :: Int)

run_unique_maker_t :: Monad m => UniqueMakerT m r -> m r
run_unique_maker_t (UniqueMakerT s) = evalStateT s 0
run_unique_maker :: UniqueMaker r -> r
run_unique_maker = Identity.runIdentity . run_unique_maker_t
