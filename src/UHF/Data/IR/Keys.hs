module UHF.Data.IR.Keys
    ( ADTKey
    , TypeSynonymKey
    , ClassKey
    , InstanceKey
    , ModuleKey
    , QuantVarKey
    , VariableKey
    , BindingKey
    , ParamKey
    ) where

import UHF.Prelude

import qualified UHF.Util.Arena as Arena

-- TODO: remove any Ord

newtype ADTKey = ADTKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key ADTKey where
    make_key = ADTKey
    unmake_key (ADTKey i) = i

newtype TypeSynonymKey = TypeSynonymKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key TypeSynonymKey where
    make_key = TypeSynonymKey
    unmake_key (TypeSynonymKey i) = i

newtype ClassKey = ClassKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key ClassKey where
    make_key = ClassKey
    unmake_key (ClassKey i) = i

newtype InstanceKey = InstanceKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key InstanceKey where
    make_key = InstanceKey
    unmake_key (InstanceKey i) = i

newtype ModuleKey = ModuleKey Arena.KeyData deriving (Show, Eq)
instance Arena.Key ModuleKey where
    make_key = ModuleKey
    unmake_key (ModuleKey i) = i

newtype QuantVarKey = QuantVarKey Arena.KeyData deriving (Show, Eq, Ord)
instance Arena.Key QuantVarKey where
    make_key = QuantVarKey
    unmake_key (QuantVarKey i) = i

newtype VariableKey = VariableKey Arena.KeyData deriving (Show, Eq, Ord) -- TODO: remove Eq and Ord when Variables store their graph nodes
instance Arena.Key VariableKey where
    make_key = VariableKey
    unmake_key (VariableKey i) = i

newtype BindingKey = BindingKey Arena.KeyData deriving (Show, Eq, Ord) -- TODO: figure out better solution in ts backend than to use ord instance
instance Arena.Key BindingKey where
    make_key = BindingKey
    unmake_key (BindingKey i) = i

newtype ParamKey = ParamKey Arena.KeyData deriving (Show, Eq)
instance Arena.Key ParamKey where
    make_key = ParamKey
    unmake_key (ParamKey i) = i

