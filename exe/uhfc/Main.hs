{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import UHF.Util.Prelude

import Options.Applicative

import qualified Data.Map as Map

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
                            (reader_from_map
                                [ ("ast", Driver.AST)
                                , ("ast-dump", Driver.ASTDump)
                                , ("sir", Driver.SIR)
                                , ("name-resolved-starts", Driver.NRStartsSIR)
                                , ("type-expressions-evaluated", Driver.TEESIR)
                                , ("name-resolved", Driver.NRSIR)
                                , ("infix-grouped", Driver.InfixGroupedSIR)
                                , ("typed-sir", Driver.TypedSIR)
                                , ("rir", Driver.RIR)
                                , ("anfir", Driver.ANFIR)
                                , ("anfir-optimized", Driver.OptimizedANFIR)
                                , ("backend-ir", Driver.BackendIR)
                                , ("ts", Driver.TS)
                                ])
                            (long "output"
                                <> metavar "FORMAT"
                                <> help "The type of output to emit")
                        )))
                )
            <*> option
                    (reader_from_map
                        [ ("always", FormattedString.Colors)
                        , ("never", FormattedString.NoColors)
                        , ("auto", FormattedString.AutoDetect)
                        ]
                    )
                    (long "colors"
                        <> metavar "COLORS"
                        <> value FormattedString.AutoDetect
                        <> help "When to print colors in diagnostics"
                    )
            <*> (DiagnosticSettings.Settings <$> option
                    (reader_from_map
                        [ ("original-ascii", DiagnosticSettings.ASCII)
                        , ("original-unicode", DiagnosticSettings.Unicode)
                        ])
                    (long "diagnostic-format"
                        <> metavar "FORMAT"
                        <> value DiagnosticSettings.Unicode
                        <> help "The format to output diagnostics"
                    )
                )

main :: IO ()
main =
    execParser argparser >>= \ (Args compile_opts c_needed diagnostic_settings) ->
    Driver.compile c_needed diagnostic_settings compile_opts >>= \case
        Right () -> pure ()
        Left () -> exitFailure

reader_from_map :: Map.Map [Char] a -> ReadM a
reader_from_map options = eitherReader $ \ choice ->
    case Map.lookup choice options of
        Just result -> Right result
        Nothing -> Left $ "invalid option: must be one of " <> intercalate ", " (map (\ o -> "'" <> o <> "'") (Map.keys options))
