{-# LANGUAGE FlexibleContexts #-}

module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified UHF.Driver as Driver

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
            <*> pure (DiagnosticSettings.Settings DiagnosticSettings.Unicode) -- TODO: put this in an argument

main :: IO ()
main =
    execParser argparser >>= \ (Args files c_needed diagnostic_settings) ->
    mapM (Driver.compile c_needed diagnostic_settings) files >>= \ results ->
    if any isLeft results
       then exitFailure
       else pure ()
