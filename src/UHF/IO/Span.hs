module UHF.IO.Span
    ( Span

    , file, start, before_end, end
    , start_ind, before_end_ind, end_ind
    , start_row, before_end_row, end_row
    , start_col, before_end_col, end_col

    , new
    , dummy -- TODO: use conditional compilation? to make sure this only compiles in tests
    , start_of_file
    , end_of_file

    , is_single_line
    ) where

import UHF.Util.Prelude

import qualified UHF.IO.File as File
import UHF.IO.File (File)
import qualified UHF.IO.Location as Location
import UHF.IO.Location (Location)

import Data.List (maximumBy, minimumBy)
import qualified Data.Text as Text

data Span = Span Location.Location Location.Location Location.Location deriving (Show, Eq) -- TODO: remove Eq

instance Format Span where
    format (Span l1 _ l2) = format (Location.loc_file l1) <> ":" <> format (Location.lc_row $ Location.lc l1) <> ":" <> format (Location.lc_col $ Location.lc l1) <> ":" <> format (Location.lc_row $ Location.lc l2) <> ":" <> format (Location.lc_col $ Location.lc l2)

file :: Span -> File
file (Span l1 _ _) = Location.loc_file l1

start, before_end, end :: Span -> Location
start (Span start _ _) = start
before_end (Span _ before_end _) = before_end
end (Span _ _ end) = end

start_ind, before_end_ind, end_ind, start_row, before_end_row, end_row, start_col, before_end_col, end_col :: Span -> Int
start_ind = Location.loc_ind . start
before_end_ind = Location.loc_ind . before_end
end_ind = Location.loc_ind . end
start_row = Location.loc_row . start
before_end_row = Location.loc_row . before_end
end_row = Location.loc_row . end
start_col = Location.loc_col . start
before_end_col = Location.loc_col . before_end
end_col = Location.loc_col . end

new :: Location -> Int -> Int -> Span
new loc start_i len =
    let start_l = Location.seek start_i loc
        b_l = Location.seek (len - 1) start_l
        end_l = Location.seek len start_l
    in Span start_l b_l end_l

start_of_file :: File -> Span
start_of_file f = new (Location.new f) 0 1

end_of_file :: File -> Span
end_of_file f = new (Location.seek (File.length f) $ Location.new f) 0 1

dummy :: IO Span
dummy = -- TODO: this cannot be joined with any other spans
    File.new "" "" >>= \ f ->
    let l = Location.new f
    in pure $ Span l l l

instance Semigroup Span where
    (Span s1 b1 e1) <> (Span s2 b2 e2) =
        if Location.loc_file s1 == Location.loc_file s2
            then
                let comparator = compare `on` Location.loc_ind
                    minstart = minimumBy comparator [s1, s2]
                    maxbefore = maximumBy comparator [b1, b2]
                    maxend = maximumBy comparator [e1, e2]
                in Span minstart maxbefore maxend

            else error "join two spans where some locations have different files"

is_single_line :: Span -> Bool
is_single_line (Span start before_end _) = Location.loc_row start == Location.loc_row before_end
