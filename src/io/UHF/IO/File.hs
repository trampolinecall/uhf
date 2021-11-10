module UHF.IO.File where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

data File = File { path :: String, contents :: Text.Text } deriving (Eq)

instance Show File where
    show (File p _) = "File { path = " ++ show p ++ ", contents = ... }"

open_file :: String -> IO File
open_file path' = File path' <$> Text.IO.readFile path'

write_file :: File -> IO ()
write_file f = Text.IO.writeFile (path f) (contents f)
