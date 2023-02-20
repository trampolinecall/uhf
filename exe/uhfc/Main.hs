module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified UHF.Compiler as Compiler
import qualified UHF.Compiler.DiagnosticSettings as DiagnosticSettings

import qualified UHF.IO.File as File

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
    File.open_file fname >>= \ f ->
    putStrLn ("[" <> show num <> "/" <> show total <> "]: compiling " <> format f) >>
    Compiler.run_compiler (Driver.compile f) DiagnosticSettings.AutoDetect >>= \case
        Just res -> putTextLn (show res)
        Nothing -> pure ()
