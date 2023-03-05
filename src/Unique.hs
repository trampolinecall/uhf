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
newtype UniqueMakerT m r = UniqueMakerT { un_unique_maker :: StateT Int m r }
type UniqueMaker = UniqueMakerT Identity.Identity

instance Functor m => Functor (UniqueMakerT m) where
    fmap f (UniqueMakerT s) = UniqueMakerT $ f <$> s

instance (Applicative m, Monad m) => Applicative (UniqueMakerT m) where
    pure = UniqueMakerT . pure
    (UniqueMakerT a) <*> (UniqueMakerT b) = UniqueMakerT $ a <*> b

instance Monad m => Monad (UniqueMakerT m) where
    (UniqueMakerT a) >>= f = UniqueMakerT $ a >>= un_unique_maker . f

instance Trans.MonadTrans (UniqueMakerT) where
    lift other = UniqueMakerT $ lift other

make_unique :: Monad m => UniqueMakerT m Unique
make_unique = UniqueMakerT $ StateT $ \ u -> pure (Unique u, (u + 1) :: Int)

run_unique_maker_t :: Monad m => UniqueMakerT m r -> m r
run_unique_maker_t (UniqueMakerT s) = evalStateT s 0
run_unique_maker :: UniqueMaker r -> r
run_unique_maker = Identity.runIdentity . run_unique_maker_t
