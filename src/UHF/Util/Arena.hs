{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module UHF.Util.Arena
    ( Arena
    , Key(..)
    , KeyData
    , new
    , put
    , get
    , modify
    , modifyM

    , transform
    , transform_with_key
    , transformM
    , transform_with_keyM

    , tests
    ) where

import UHF.Prelude hiding (put, get, modify)

import qualified Data.Sequence as Sequence

newtype Arena a k = Arena (Seq a) deriving (Show, Eq)
newtype KeyData = KeyData Int deriving (Show, Eq, Ord) -- TODO: dont have show?

class Key k where
    make_key :: KeyData -> k
    unmake_key :: k -> KeyData

new :: Arena a k
new = Arena Sequence.empty

put :: Key k => a -> Arena a k -> (k, Arena a k)
put item (Arena items) =
    let index = length items
    in (make_key (KeyData index), Arena $ items Sequence.|> item)

get :: Key k => Arena a k -> k -> a
get (Arena items) key =
    let (KeyData index) = unmake_key key
    in items `Sequence.index` index

modify :: Key k => Arena a k -> k -> (a -> a) -> Arena a k
modify (Arena items) key change =
    let (KeyData index) = unmake_key key
    in case Sequence.splitAt index items of
        (before, old Sequence.:<| after) -> Arena $ before <> (change old Sequence.<| after)
        (_, Sequence.Empty) -> unreachable -- because the key should always be valid

modifyM :: (Monad m, Key k) => Arena a k -> k -> (a -> m a) -> m (Arena a k)
modifyM (Arena items) key change =
    let (KeyData index) = unmake_key key
    in case Sequence.splitAt index items of
        (before, old Sequence.:<| after) -> change old >>= \ changed -> pure (Arena $ before <> (changed Sequence.<| after))
        (_, Sequence.Empty) -> unreachable -- because the key should always be valid

transform :: Key k => (a -> b) -> Arena a k -> Arena b k
transform t (Arena items) = Arena $ fmap t items

transform_with_key :: Key k => (k -> a -> b) -> Arena a k -> Arena b k
transform_with_key t (Arena items) = Arena $ fmap (uncurry t) (Sequence.zip (fmap (make_key . KeyData) [0 .. Sequence.length items - 1]) items)

transformM :: (Key k, Monad m) => (a -> m b) -> Arena a k -> m (Arena b k)
transformM t (Arena items) = Arena <$> mapM t items

transform_with_keyM :: (Key k, Monad m) => (k -> a -> m b) -> Arena a k -> m (Arena b k)
transform_with_keyM t (Arena items) = Arena <$> mapM (uncurry t) (Sequence.zip (fmap (make_key . KeyData) [0 .. Sequence.length items - 1]) items)

newtype TestKey = TestKey KeyData deriving (Show, Eq)
instance Key TestKey where
    make_key = TestKey
    unmake_key (TestKey k) = k

case_put :: Assertion
case_put =
    put (0 :: Int) new @?= (TestKey $ KeyData 0, Arena [0])

case_put_more :: Assertion
case_put_more =
    put (1 :: Int) (Arena [0]) @?= (TestKey $ KeyData 1, Arena [0, 1])

case_get :: Assertion
case_get =
    let a1 :: Arena Int TestKey
        (k0, a1) = put 0 new
        (k1, a2) = put 1 a1
    in (get a2 k0 @?= 0) >> (get a2 k1 @?= 1)

case_modify :: Assertion
case_modify =
    let a1 :: Arena Int TestKey
        (k0, a0) = put 0 new
        (k1, a1) = put 1 a0
        (k2, a2) = put 2 a1
    in
        (modify a2 k0 (const 100) @?= Arena [100, 1, 2]) >>
        (modify a2 k1 (const 100) @?= Arena [0, 100, 2]) >>
        (modify a2 k2 (const 100) @?= Arena [0, 1, 100])

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
