{-# LANGUAGE FlexibleContexts #-}

module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified UHF.Driver as Driver

import qualified UHF.IO.FormattedString as FormattedString
import qualified UHF.Diagnostic.Settings as DiagnosticSettings

data Args = Args Driver.CompileOptions FormattedString.ColorsNeeded DiagnosticSettings.Settings

argparser :: ParserInfo Args
argparser =
    info
        (args <**> helper)
        ( fullDesc
            <> header "uhfc: uhf compiler"
        )
    where
        args = Args
            <$> (Driver.CompileOptions
                    <$> argument str
                            (metavar "FILE"
                                <> help "The module to compile"
                            )
                    <*> optional (strOption
                            (long "module-name"
                                <> metavar "MODULENAME"
                                <> help "The name of the current module"
                            )
                        )
                    <*> (fromMaybe [Driver.TS] <$> optional (some (option
                            (eitherReader $ \case
                                "ast" -> Right Driver.AST
                                "dot" -> Right Driver.Dot
                                "ts" -> Right Driver.TS
                                _ -> Left "invalid option: must one of 'ast', 'dot', or 'ts'")
                            (long "output-format"
                                <> metavar "FORMAT"
                                <> help "The types of output to emit")
                        )))
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
                        <> help "When to print colors in diagnostics"
                    )
                )
            <*> (DiagnosticSettings.Settings <$> option
                    (eitherReader $ \case
                        "original-ascii" -> Right DiagnosticSettings.ASCII
                        "original-unicode" -> Right DiagnosticSettings.Unicode
                        _ -> Left "invalid option: must be one of 'original-ascii' or 'original-unicode'"
                    )
                    (long "diagnostic-format"
                        <> metavar "FORMAT"
                        <> value DiagnosticSettings.Unicode
                        <> help "The format to output diagnostics")
                )

main :: IO ()
main =
    execParser argparser >>= \ (Args compile_opts c_needed diagnostic_settings) ->
    Driver.compile c_needed diagnostic_settings compile_opts >>= \case
        Right () -> pure ()
        Left () -> exitFailure
