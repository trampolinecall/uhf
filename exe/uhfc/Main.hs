module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

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
    File.open_file fname >>= \ f ->
    putStrLn ("[" <> show num <> "/" <> show total <> "]: compiling " <> format f) >>
    let (res, diags) = runWriter $ Driver.compile f
    in Diagnostic.report_diagnostics IO.stderr diags >>
    putText (show res)
