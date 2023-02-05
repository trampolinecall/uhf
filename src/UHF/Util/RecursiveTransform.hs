module UHF.Util.RecursiveTransform
    ( Result (..)
    , UHF.Util.RecursiveTransform.get
    ) where

import UHF.Util.Prelude

import qualified Data.Map as Map
import qualified Data.List as List

data Result o e r
    = Ok r
    | Err e
    | Loop [o]

type RecursiveTransformState a e b = (a -> RecursiveTransform a e b b, [a], Map.Map a (Result a e b))
newtype RecursiveTransform a e b r = RecursiveTransform (State (RecursiveTransformState a e b) (Result a e r))

instance Functor (Result o e) where
    fmap f (Ok r) = Ok $ f r
    fmap _ (Err e) = Err e
    fmap _ (Loop l) = Loop l

instance Applicative (Result o e) where
    pure = Ok

    (Ok f) <*> a = f <$> a
    (Err e) <*> _ = Err e
    (Loop l) <*> _ = Loop l

instance Monad (Result o e) where
    (Ok r) >>= op = op r
    (Err e) >>= _ = Err e
    (Loop l) >>= _ = Loop l

instance Functor (RecursiveTransform a e b) where
    fmap f (RecursiveTransform s) =
        RecursiveTransform $ s >>= \ result -> pure (f <$> result)

instance Applicative (RecursiveTransform a e b) where
    pure a = RecursiveTransform $ pure $ pure a
    (RecursiveTransform a) <*> (RecursiveTransform b) = RecursiveTransform $
        a >>= \ a' ->
        b >>= \ b' ->
        pure (a' <*> b')

instance Monad (RecursiveTransform a e b) where
    (RecursiveTransform a) >>= op = RecursiveTransform $
        a >>= \ a' ->
        case a' of
            Ok r ->
                let (RecursiveTransform r') = op r
                in r'
            Err e -> pure $ Err $ e
            Loop o -> pure $ Loop $ o

-- run a computation on an item, retrieving it from the cache if present, and checking not to make infinite recursion
get :: Ord a => a -> RecursiveTransform a e b b
get a = RecursiveTransform $
    UHF.Util.Prelude.get >>= \ (compute, stack, cache) ->
        case Map.lookup a cache of
            Just result -> pure result
            Nothing ->
                case List.findIndex (==a) stack of
                    Just ind -> pure $ Loop (drop ind stack)
                    Nothing ->
                        -- append the current item to the recursion stack
                        put (compute, (a:stack), cache) >>

                        -- run the computation
                        let (RecursiveTransform compute_result) = compute a
                        in compute_result >>= \ result ->

                        -- remove the curreent item from the recursion stack
                        put (compute, stack, cache) >>

                        -- add the result to the cache
                        put (compute, stack, Map.insert a result cache) >>

                        pure result

-- TODO: tests
