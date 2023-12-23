module UHF.Data.IR.Type.Class
    ( Class (..)
    , ClassKey
    , Instance (..)
    , InstanceKey
    ) where

import UHF.Prelude

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.ID as ID

data Class = Class ID.DeclID (Located Text) [QuantVarKey] [()] deriving Show -- TODO: figure out how class members are going to work
data Instance cl ty = Instance [QuantVarKey] cl [ty] [()] deriving Show -- TODO: instance ids
