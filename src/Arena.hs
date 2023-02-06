module Arena
    ( Arena
    , Key(..)
    , new
    , Arena.put
    , Arena.get

    , transform
    , transformM

    , tests
    ) where

import Test.Tasty
import UHF.Util.Prelude

import qualified Data.List ((!!))

newtype Arena a k = Arena [a] deriving (Show, Eq)

class Key k where
    make_key :: Int -> k
    unmake_key :: k -> Int

new :: Arena a k
new = Arena []

put :: Key k => a -> Arena a k -> (k, Arena a k)
put item (Arena items) =
    let index = length items
    in (make_key index, Arena $ item : items) -- indexes count from end

get :: Key k => Arena a k -> k -> a
get (Arena items) key = items Data.List.!! (length items - unmake_key key - 1)

transform :: (a -> b) -> Arena a k -> Arena b k
transform t (Arena items) = Arena $ map t items

transformM :: Monad m => (a -> m b) -> Arena a k -> m (Arena b k)
transformM t (Arena items) = Arena <$> mapM t items

newtype TestKey = TestKey Int deriving (Show, Eq)
instance Key TestKey where
    make_key = TestKey
    unmake_key (TestKey k) = k

case_put :: Assertion
case_put =
    Arena.put (0 :: Int) new @?= (TestKey 0, Arena [0])

case_put_more :: Assertion
case_put_more =
    Arena.put (1 :: Int) (Arena [0]) @?= (TestKey 1, Arena [1, 0])

case_get :: Assertion
case_get =
    let a1 :: Arena Int TestKey
        (k0, a1) = Arena.put 0 new
        (k1, a2) = Arena.put 1 a1
    in (Arena.get a2 k0 @?= 0) >> (Arena.get a2 k1 @?= 1)

case_transform :: Assertion
case_transform =
    let a :: Arena Int TestKey
        a = Arena [0, 1, 2]
    in transform (+2) a @?= Arena [2, 3, 4]

case_transformM :: Assertion
case_transformM =
    let a :: Arena Int TestKey
        a = Arena [0, 1, 2]
    in runWriter (transformM (\ x -> tell [x] >> pure (x + 2)) a) @?= (Arena [2, 3, 4], [0, 1, 2])

tests :: TestTree
tests = $(testGroupGenerator)
