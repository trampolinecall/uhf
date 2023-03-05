module UHF.Data.IR.Keys
    ( DeclKey
    , ADTKey
    , TypeSynonymKey
    , BoundValueKey
    , BindingKey
    , ParamKey
    ) where

import UHF.Util.Prelude

import qualified Arena

-- TODO: remove any Ord

newtype DeclKey = DeclKey Int deriving Show
instance Arena.Key DeclKey where
    make_key = DeclKey
    unmake_key (DeclKey i) = i

newtype ADTKey = ADTKey Int deriving (Show, Eq)
instance Arena.Key ADTKey where
    make_key = ADTKey
    unmake_key (ADTKey i) = i

newtype TypeSynonymKey = TypeSynonymKey Int deriving (Show, Eq)
instance Arena.Key TypeSynonymKey where
    make_key = TypeSynonymKey
    unmake_key (TypeSynonymKey i) = i

newtype BoundValueKey = BoundValueKey Int deriving (Show, Eq, Ord) -- TODO: remove Eq and Ord when BoundValues store their graph nodes
instance Arena.Key BoundValueKey where
    make_key = BoundValueKey
    unmake_key (BoundValueKey i) = i

newtype BindingKey = BindingKey Int deriving (Show, Eq, Ord) -- TODO: figure out better solution in ts backend than to use ord instance
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i

newtype ParamKey = ParamKey Int deriving (Show, Eq)
instance Arena.Key ParamKey where
    make_key = ParamKey
    unmake_key (ParamKey i) = i

