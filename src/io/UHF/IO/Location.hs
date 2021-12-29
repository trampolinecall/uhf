module UHF.IO.Location where

import qualified UHF.IO.File as File

import qualified Data.Text as Text
import Data.List (minimumBy, maximumBy)
import Data.Function (on)

data Location = Location { file :: File.File, ind :: Int, row :: Int, col :: Int } deriving Eq
-- TODO: make span constructor function that does not allow locations to be in different files
data Span = Span { start :: Location, before_end :: Location, end :: Location } deriving (Show, Eq)

data Located a = Located { just_span :: Span, unlocate :: a } deriving (Show, Eq)

line :: Location -> Int
line = row

instance Show Location where
    show (Location f i r c) =
        let snippet = Text.drop (i - 3) $ File.contents f

            before = Text.take 2 snippet
            after = Text.take 2 $ Text.drop 3 snippet
            ch = Text.take 1 $ Text.drop 2 snippet

        in (File.path f) ++ ":" ++ show r ++ ":" ++ show c ++ ": " ++ "'" ++ Text.unpack before ++ "\"" ++ Text.unpack ch ++ "\"" ++ Text.unpack after ++ "'"

instance Functor Located where
    fmap f (Located sp v) = Located sp (f v)

join_span :: Span -> Span -> Span
join_span (Span s1 b1 e1) (Span s2 b2 e2) =
    if all ((file s1 ==) . file) [s1, b1, e1, s2, b2, e2]
        then
            let comparator = compare `on` ind
                minstart = minimumBy comparator [s1, s2]
                maxbefore =  maximumBy comparator [b1, b2]
                maxend = maximumBy comparator [e1, e2]
            in Span minstart maxbefore maxend

        else error "join two spans where some locations have different files"

fmt_location :: Location -> String
fmt_location (Location f _ r c) = File.path f ++ ":" ++ show r ++ ":" ++ show c

fmt_span :: Span -> String
fmt_span (Span (Location f1 _ r1 c1) _ (Location _ _ r2 c2)) = File.path f1 ++ ":" ++ show r1 ++ ":" ++ show c1 ++ ":" ++ show r2 ++ ":" ++ show c2

fmt_located :: Show a => Located a -> String
fmt_located (Located sp a) = "<at " ++ fmt_span sp ++ "> " ++ show a

is_single_line :: Span -> Bool
is_single_line (Span s be _) = row s == row be
