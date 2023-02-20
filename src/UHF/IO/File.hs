module UHF.IO.File
    ( File
    , path
    , contents

    , new
    , open
    , write -- TODO: use when writing output file
    ) where

import UHF.Util.Prelude hiding (show)

import GHC.Show (show)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import Data.Unique

data File = File { uniq :: Unique, path :: FilePath, contents :: Text }

instance Show File where
    show (File _ path _) = "File { path = " <> path <> ", ... }"

instance Eq File where
    (==) = (==) `on` uniq

instance Format File where
    format f = Text.pack $ path f

new :: FilePath -> Text -> IO File
new path contents =
    newUnique >>= \ u ->
    pure (File u path contents)

open :: FilePath -> IO File
open path = Text.IO.readFile path >>= \ contents -> new path contents

write :: File -> IO ()
write f = Text.IO.writeFile (path f) (contents f)

