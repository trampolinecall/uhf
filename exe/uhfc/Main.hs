module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified UHF.Diagnostic as Diagnostic

import UHF.IO.Location (open_file) -- TODO: rename this module to something better (open_file in the Location module is a bit weird)

import qualified System.IO as IO

newtype Args = Args [FilePath]

argparser :: ParserInfo Args
argparser = info (args <**> helper)
    ( fullDesc
    <> progDesc "compile one or more uhf files"
    <> header "uhfc: uhf compiler"
    )
    where
        args = Args <$>
            some (
                argument str
                    ( metavar "FILES..."
                    <> help "files to compile"
                    )
            )

main :: IO ()
main =
    execParser argparser >>= \ (Args files) ->
    let total_files = length files
    in mapM_ (\ (num, f) -> compile num total_files f) (zip [1..] files)

compile :: Int -> Int -> FilePath -> IO ()
compile num total fname =
    open_file fname >>= \ f ->
    putStrLn ("[" <> show num <> "/" <> show total <> "]: compiling " <> format f) >>
    case Driver.compile f of
        Right res -> putTextLn (show res)
        Left diags -> mapM_ (Diagnostic.report IO.stderr) diags
