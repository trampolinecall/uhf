module UHF.IO.File where

import UHF.Util.Prelude

import qualified UHF.Diagnostic.FormattedString as FormattedString
import qualified UHF.Diagnostic.Colors as Colors

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data File = File { path :: FilePath, contents :: Text } deriving (Show, Eq)

instance Format File where
    format f = FormattedString.color_text Colors.file_path (Text.pack $ path f)

open_file :: FilePath -> IO File
open_file path' = File path' <$> Text.IO.readFile path'

write_file :: File -> IO ()
write_file f = Text.IO.writeFile (path f) (contents f)
