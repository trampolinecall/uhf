module UHF.IO.Location where

import qualified UHF.IO.File as File

data Location = Location { file :: File.File, row :: Int, col :: Int } deriving (Eq, Show)
data Span = Span { start :: Location, before_end :: Location, end :: Location } deriving (Eq, Show)

data Located a = Located Span a

line :: Location -> Int
line = row
