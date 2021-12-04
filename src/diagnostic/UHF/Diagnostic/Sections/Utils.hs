{-# LANGUAGE TupleSections #-}

module UHF.Diagnostic.Sections.Utils where

import qualified UHF.IO.File as File

import qualified Data.Text as Text

context_lines :: File.File -> Int -> [(File.File, Int)]
context_lines f n = filter (uncurry line_exists) $ map (f,) [n-2..n+2]

line_exists :: File.File -> Int -> Bool
line_exists fl nr =
    nr > 0 && nr <= length (Text.lines $ File.contents fl)

flnr_comparator :: (File.File, Int) -> (File.File, Int) -> Ordering
flnr_comparator (f1, n1) (f2, n2)
    | f1 == f2 = n1 `compare` n2
    | otherwise = EQ

