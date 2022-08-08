module Main where

import Options.Applicative

import qualified UHF.Compiler as Compiler
import qualified UHF.IO.File as File
import qualified UHF.Diagnostic as Diagnostic

import qualified System.IO as IO

data Args
    = Args
      { files :: [String]
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
    execParser argparser >>= \ (Args files) ->
    sequence_ (map compile files)

compile :: String -> IO ()
compile fname =
    File.open_file fname >>= \ f ->
    let (diags, res) = Compiler.compile f
    in Diagnostic.report_diagnostics IO.stderr diags >>
    return ()
    -- print res
