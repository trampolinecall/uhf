module UHF.Phases.NameResolve.IdenKeys
    ( DIdenStartKey
    , VIdenStartKey
    , PIdenStartKey
    , DIdenKey
    , VIdenKey
    , PIdenKey
    ) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena

newtype DIdenStartKey = DIdenStartKey Int deriving Show
instance Arena.Key DIdenStartKey where
    make_key = DIdenStartKey
    unmake_key (DIdenStartKey i) = i

newtype VIdenStartKey = VIdenStartKey Int deriving Show
instance Arena.Key VIdenStartKey where
    make_key = VIdenStartKey
    unmake_key (VIdenStartKey i) = i

newtype PIdenStartKey = PIdenStartKey Int deriving Show
instance Arena.Key PIdenStartKey where
    make_key = PIdenStartKey
    unmake_key (PIdenStartKey i) = i

newtype DIdenKey = DIdenKey Int deriving Show
instance Arena.Key DIdenKey where
    make_key = DIdenKey
    unmake_key (DIdenKey i) = i

newtype VIdenKey = VIdenKey Int deriving Show
instance Arena.Key VIdenKey where
    make_key = VIdenKey
    unmake_key (VIdenKey i) = i

newtype PIdenKey = PIdenKey Int deriving Show
instance Arena.Key PIdenKey where
    make_key = PIdenKey
    unmake_key (PIdenKey i) = i
