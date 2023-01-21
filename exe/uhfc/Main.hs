module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified UHF.Driver as Driver
import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified System.IO as IO

newtype Args
    = Args
      { files :: [FilePath]
      }

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
    execParser argparser >>= \ (Args f) ->
    mapM_ compile f

compile :: FilePath -> IO ()
compile fname =
    File.open_file fname >>= \ f ->
    let (res, diags) = runWriter $ Driver.compile f
    in Diagnostic.report_diagnostics IO.stderr diags >>
    pure ()
    -- print res
