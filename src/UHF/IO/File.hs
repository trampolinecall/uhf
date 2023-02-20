module UHF.IO.File
    ( File
    , path
    , contents

    , new
    , open
    , write -- TODO: use when writing output file
    ) where

import UHF.Util.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data File = File { path :: FilePath, contents :: Text } deriving (Show)
instance Eq File where
    (==) = (==) `on` path -- TODO: remove this entirely / replace with comparison on Unique

instance Format File where
    format f = Text.pack $ path f

new :: FilePath -> Text -> File
new path contents = File path contents

open :: FilePath -> IO File
open path = Text.IO.readFile path >>= \ contents -> pure $ new path contents

write :: File -> IO ()
write f = Text.IO.writeFile (path f) (contents f)

