module UHF.IO.Location where

import qualified UHF.IO.File as File

data Location = Location { file :: File.File, row :: Int, col :: Int }
data Span = Span { start :: Location, end :: Location }

data Located a = Located Span a

line :: Location -> Int
line = row
