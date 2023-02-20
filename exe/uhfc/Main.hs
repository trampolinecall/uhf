{-# LANGUAGE FlexibleContexts #-}

module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Driver

import qualified UHF.IO.File as File

import qualified UHF.Compiler as Compiler

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

data Args = Args [FilePath] FormattedString.ColorsNeeded DiagnosticSettings.Settings

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
            <*> (option
                    (eitherReader $ \case
                        "always" -> Right FormattedString.Colors
                        "never" -> Right FormattedString.NoColors
                        "auto" -> Right FormattedString.AutoDetect
                        _ -> Left "invalid option: must be one of 'always', 'never', or 'auto'" -- TODO: figure out how to do this better
                    )
                    (long "colors"
                        <> metavar "COLORS"
                        <> value FormattedString.AutoDetect
                        <> help "when to print colors in diagnostics"
                    )
                )
            <*> pure DiagnosticSettings.Settings

main :: IO ()
main =
    execParser argparser >>= \ (Args files c_needed diagnostic_settings) ->
    let total_files = length files
    in mapM_ (\ (num, f) -> compile c_needed diagnostic_settings num total_files f) (zip [1..] files)

compile :: FormattedString.ColorsNeeded -> DiagnosticSettings.Settings -> Int -> Int -> FilePath -> IO ()
compile c_needed diagnostic_settings num total fname =
    File.open fname >>= \ f -> -- TODO: put this in driver
    Compiler.run_compiler (Driver.compile f) c_needed diagnostic_settings >>= \case
        Just (Just res) -> putTextLn res -- TODO: get rid of double Maybe
        Just Nothing -> exitFailure -- TODO: decide what should happen here
        Nothing -> exitFailure
