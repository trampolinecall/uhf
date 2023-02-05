module UHF.IO.File where

import UHF.Util.Prelude

import qualified UHF.FormattedString as FormattedString

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified System.Console.ANSI as ANSI

data File = File { path :: FilePath, contents :: Text } deriving (Show, Eq)

file_path_color :: [ANSI.SGR]
file_path_color = [ANSI.SetConsoleIntensity ANSI.BoldIntensity, ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Cyan]

instance Format File where
    format f = FormattedString.color_text file_path_color (Text.pack $ path f)

open_file :: FilePath -> IO File
open_file path' = File path' <$> Text.IO.readFile path'

write_file :: File -> IO ()
write_file f = Text.IO.writeFile (path f) (contents f)
