module UHF.Data.IR.Keys
    ( ADTKey
    , TypeSynonymKey
    , ModuleKey
    , TypeVarKey
    , VariableKey
    , BindingKey
    , ParamKey
    ) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena

-- TODO: remove any Ord

newtype ADTKey = ADTKey Int deriving (Show, Eq, Ord)
instance Arena.Key ADTKey where
    make_key = ADTKey
    unmake_key (ADTKey i) = i

newtype TypeSynonymKey = TypeSynonymKey Int deriving (Show, Eq, Ord)
instance Arena.Key TypeSynonymKey where
    make_key = TypeSynonymKey
    unmake_key (TypeSynonymKey i) = i

newtype ModuleKey = ModuleKey Int deriving (Show, Eq)
instance Arena.Key ModuleKey where
    make_key = ModuleKey
    unmake_key (ModuleKey i) = i

newtype TypeVarKey = TypeVarKey Int deriving (Show, Eq, Ord)
instance Arena.Key TypeVarKey where
    make_key = TypeVarKey
    unmake_key (TypeVarKey i) = i

newtype VariableKey = VariableKey Int deriving (Show, Eq, Ord) -- TODO: remove Eq and Ord when Variables store their graph nodes
instance Arena.Key VariableKey where
    make_key = VariableKey
    unmake_key (VariableKey i) = i

newtype BindingKey = BindingKey Int deriving (Show, Eq, Ord) -- TODO: figure out better solution in ts backend than to use ord instance
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i

newtype ParamKey = ParamKey Int deriving (Show, Eq)
instance Arena.Key ParamKey where
    make_key = ParamKey
    unmake_key (ParamKey i) = i

