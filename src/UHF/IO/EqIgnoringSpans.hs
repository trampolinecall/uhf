{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module UHF.IO.EqIgnoringSpans (EqIgnoringSpans (..), expected_assert_eqis, assert_eqis_expected) where

import UHF.Util.Prelude

import qualified UHF.IO.Span as Span

import qualified GHC.Generics as Generics

-- 'is' is short for 'ignoring spans'
class EqIgnoringSpans a where
    eqis :: a -> a -> Bool

    default eqis :: (Generic a, GEqIS (Generics.Rep a)) => a -> a -> Bool
    eqis a b = Generics.from a `geqis` Generics.from b

class GEqIS f where
    geqis :: f a -> f a -> Bool

instance GEqIS Generics.U1 where
    geqis Generics.U1 Generics.U1 = True

instance (GEqIS a, GEqIS b) => GEqIS (a Generics.:*: b) where
    geqis (a1 Generics.:*: b1) (a2 Generics.:*: b2) = a1 `geqis` a2 && b1 `geqis` b2

instance (GEqIS a, GEqIS b) => GEqIS (a Generics.:+: b) where
    geqis (Generics.L1 a1) (Generics.L1 a2) = a1 `geqis` a2
    geqis (Generics.R1 b1) (Generics.R1 b2) = b1 `geqis` b2
    geqis _ _ = False

instance (GEqIS a) => GEqIS (Generics.M1 i c a) where
    geqis (Generics.M1 a1) (Generics.M1 a2) = a1 `geqis` a2

instance (EqIgnoringSpans a) => GEqIS (Generics.K1 i a) where
    geqis (Generics.K1 a1) (Generics.K1 a2) = a1 `eqis` a2

instance EqIgnoringSpans Span.Span where
    _ `eqis` _ = True

instance EqIgnoringSpans a => EqIgnoringSpans [a] where
    [] `eqis` [] = True
    (a:as) `eqis` (b:bs) = a `eqis` b && as `eqis` bs
    _ `eqis` _ = False

instance (EqIgnoringSpans a, EqIgnoringSpans b) => EqIgnoringSpans (a, b) where
    (a1, b1) `eqis` (a2, b2) = a1 `eqis` a2 && b1 `eqis` b2

instance EqIgnoringSpans a => EqIgnoringSpans (Ratio a) where
    (n1 :% d1) `eqis` (n2 :% d2) = n1 `eqis` n2 && d1 `eqis` d2

instance EqIgnoringSpans () where a `eqis` b = a == b
instance EqIgnoringSpans Int where a `eqis` b = a == b
instance EqIgnoringSpans Integer where a `eqis` b = a == b
instance EqIgnoringSpans Char where a `eqis` b = a == b
instance EqIgnoringSpans Bool where a `eqis` b = a == b
instance EqIgnoringSpans Text where a `eqis` b = a == b

assert_eqis_expected :: (EqIgnoringSpans a, Show a, HasCallStack) => a -> a -> Assertion
assert_eqis_expected got expected = expected_assert_eqis expected got
expected_assert_eqis :: (EqIgnoringSpans a, Show a, HasCallStack) => a -> a -> Assertion
expected_assert_eqis expected got =
    assertBool msg (expected `eqis` got)
    where
        msg = "expected equal ignoring spans:\nexpected: " ++ show expected ++ "\n but got: " ++ show got
