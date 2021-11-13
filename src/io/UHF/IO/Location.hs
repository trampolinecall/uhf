module UHF.IO.Location where

import Test.HUnit (Test(TestList))

import qualified UHF.IO.File as File

data Location = Location { file :: File.File, row :: Int, col :: Int } deriving (Show, Eq)
data Span = Span { start :: Location, before_end :: Location, end :: Location } deriving (Show, Eq)

data Located a = Located { just_span :: Span, unlocate :: a } deriving Show

line :: Location -> Int
line = row

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

instance Eq a => Eq (Located a) where
    (Located _ a) == (Located _ b) = a == b

fmt_location :: Location -> String
fmt_location (Location f r c) = File.path f ++ ":" ++ show r ++ ":" ++ show c

fmt_span :: Span -> String
fmt_span (Span (Location f1 r1 c1) _ (Location _ r2 c2)) = File.path f1 ++ ":" ++ show r1 ++ ":" ++ show c1 ++ ":" ++ show r2 ++ ":" ++ show c2

fmt_located :: Show a => Located a -> String
fmt_located (Located sp a) = "<at " ++ fmt_span sp ++ "> " ++ show a

tests :: Test
tests = TestList []
