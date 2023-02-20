module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified UHF.Compiler as Compiler
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

import qualified UHF.IO.File as File

data Args = Args [FilePath] DiagnosticSettings.Settings

argparser :: ParserInfo Args
argparser =
    info
        (args <**> helper)
        ( fullDesc
            <> header "uhfc: uhf compiler"
        )
    where
        args = Args
            <$> some (
                    argument str
                        ( metavar "FILES..."
                        <> help "files to compile"
                        )
                )
            <*> (DiagnosticSettings.Settings
                    <$> option
                        (eitherReader $ \case
                            "always" -> Right DiagnosticSettings.Colors
                            "never" -> Right DiagnosticSettings.NoColors
                            "auto" -> Right DiagnosticSettings.AutoDetect
                            _ -> Left "invalid option: must be one of 'always', 'never', or 'auto'" -- TODO: figure out how to do this better
                        )
                        (long "colors"
                            <> metavar "COLORS"
                            <> value (DiagnosticSettings.AutoDetect)
                            <> help "when to print colors in diagnostics"
                        )
                )

main :: IO ()
main =
    execParser argparser >>= \ (Args files diagnostic_settings) ->
    let total_files = length files
    in mapM_ (\ (num, f) -> compile diagnostic_settings num total_files f) (zip [1..] files)

compile :: DiagnosticSettings.Settings -> Int -> Int -> FilePath -> IO ()
compile diagnostic_settings num total fname =
    File.open_file fname >>= \ f ->
    putStrLn ("[" <> show num <> "/" <> show total <> "]: compiling " <> format f) >>
    Compiler.run_compiler (Driver.compile f) diagnostic_settings >>= \case
        Just res -> putTextLn (show res)
        Nothing -> pure ()
