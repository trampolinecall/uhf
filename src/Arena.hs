{-# LANGUAGE OverloadedLists #-}

module Arena
    ( Arena
    , Key(..)
    , new
    , Arena.put
    , Arena.get
    , Arena.modify

    , transform
    , transformM
    , transform_with_keyM

    , tests
    ) where

import Test.Tasty
import UHF.Util.Prelude

import qualified Data.Sequence as Sequence

newtype Arena a k = Arena (Seq a) deriving (Show, Eq)

class Key k where
    make_key :: Int -> k
    unmake_key :: k -> Int

new :: Arena a k
new = Arena Sequence.empty

put :: Key k => a -> Arena a k -> (k, Arena a k)
put item (Arena items) =
    let index = length items
    in (make_key index, Arena $ items Sequence.|> item)

get :: Key k => Arena a k -> k -> a
get (Arena items) key = items `Sequence.index` (unmake_key key)

modify :: Key k => Arena a k -> k -> (a -> a) -> Arena a k
modify (Arena items) key change =
    let (before, old Sequence.:<| after) = Sequence.splitAt (unmake_key key) items
    in Arena $ before <> (change old Sequence.<| after)

transform :: Key k => (a -> b) -> Arena a k -> Arena b k
transform t (Arena items) = Arena $ fmap t items

transformM :: (Key k, Monad m) => (a -> m b) -> Arena a k -> m (Arena b k)
transformM t (Arena items) = Arena <$> mapM t items

transform_with_keyM :: (Key k, Monad m) => (k -> a -> m b) -> Arena a k -> m (Arena b k)
transform_with_keyM t (Arena items) = Arena <$> mapM (uncurry t) (Sequence.zip (fmap make_key [0 .. Sequence.length items - 1]) items)

newtype TestKey = TestKey Int deriving (Show, Eq)
instance Key TestKey where
    make_key = TestKey
    unmake_key (TestKey k) = k

case_put :: Assertion
case_put =
    Arena.put (0 :: Int) new @?= (TestKey 0, Arena [0])

case_put_more :: Assertion
case_put_more =
    Arena.put (1 :: Int) (Arena [0]) @?= (TestKey 1, Arena [0, 1])

case_get :: Assertion
case_get =
    let a1 :: Arena Int TestKey
        (k0, a1) = Arena.put 0 new
        (k1, a2) = Arena.put 1 a1
    in (Arena.get a2 k0 @?= 0) >> (Arena.get a2 k1 @?= 1)

case_modify :: Assertion
case_modify =
    let a1 :: Arena Int TestKey
        (k0, a0) = Arena.put 0 new
        (k1, a1) = Arena.put 1 a0
        (k2, a2) = Arena.put 2 a1
    in
        (Arena.modify a2 k0 (const 100) @?= Arena [100, 1, 2]) >>
        (Arena.modify a2 k1 (const 100) @?= Arena [0, 100, 2]) >>
        (Arena.modify a2 k2 (const 100) @?= Arena [0, 1, 100])

case_transform :: Assertion
case_transform =
    let a :: Arena Int TestKey
        a = Arena [0, 1, 2]
    in transform (+2) a @?= Arena [2, 3, 4]

case_transformM :: Assertion
case_transformM =
    let a :: Arena Int TestKey
        a = Arena [0, 1, 2]
    in runWriter (transformM (\ x -> tell (x:[]) >> pure (x + 2)) a) @?= (Arena [2, 3, 4], [0, 1, 2])

tests :: TestTree
tests = $(testGroupGenerator)
