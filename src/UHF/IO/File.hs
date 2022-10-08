module UHF.IO.File where

import UHF.Util.Prelude

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data File = File { path :: FilePath, contents :: Text.Text } deriving (Show, Eq)

open_file :: FilePath -> IO File
open_file path' = File path' <$> Text.IO.readFile path'

write_file :: File -> IO ()
write_file f = Text.IO.writeFile (path f) (contents f)
