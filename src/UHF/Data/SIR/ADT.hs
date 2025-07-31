module UHF.Data.SIR.ADT
    ( ADTKey
    , VariantIndex (..)
    , FieldIndex (..)
    ) where

import UHF.Prelude

import qualified Data.List as List

import UHF.Data.IR.Keys
import UHF.Source.Located (Located)
import qualified UHF.Data.IR.ID as ID
import qualified UHF.Util.Arena as Arena
import qualified UHF.Data.SIR as SIR

data DoNotConstruct = DoNotConstruct deriving (Show, Eq, Ord)

data VariantIndex = VariantIndex DoNotConstruct ADTKey Int deriving (Show, Eq, Ord)
data FieldIndex = FieldIndex DoNotConstruct VariantIndex Int deriving (Show, Eq, Ord)
