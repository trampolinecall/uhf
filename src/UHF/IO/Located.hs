{-# LANGUAGE DeriveAnyClass #-}

module UHF.IO.Located
    ( Located(..)
    , dummy_locate -- TODO: use conditional compilation? to make sure this only compiles in tests
    ) where

import UHF.Util.Prelude

import UHF.IO.EqIgnoringSpans
import UHF.IO.Span (Span)
import qualified UHF.IO.Span as Span

data Located a = Located { just_span :: Span, unlocate :: a } deriving (Show, Generic, Eq, EqIgnoringSpans)

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

dummy_locate :: a -> IO (Located a)
dummy_locate a = Span.dummy >>= \ sp -> pure (Located sp a)
