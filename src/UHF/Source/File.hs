module UHF.Source.File
    ( File
    , path
    , contents
    , UHF.Source.File.length

    , new
    , open
    ) where

import UHF.Prelude hiding (show)

import Data.Unique
import GHC.Show (show)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data File = File { uniq :: Unique, path :: FilePath, contents :: Text, length :: Int }

instance Show File where
    show (File _ path _ _) = "File { path = " <> path <> ", ... }"

instance Eq File where
    (==) = (==) `on` uniq

instance Format File where
    format f = Text.pack $ path f

new :: FilePath -> Text -> IO File
new path contents =
    newUnique >>= \ u ->
    pure (File u path contents (Text.length contents))

open :: FilePath -> IO File
open path = Text.IO.readFile path >>= \ contents -> new path contents
