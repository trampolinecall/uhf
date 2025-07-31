{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import UHF.Prelude

import Options.Applicative
import qualified Data.Map as Map

import qualified UHF.Diagnostic.Settings as Diagnostic.Settings
import qualified UHF.Driver as Driver
import qualified UHF.Source.FormattedString as FormattedString

data Args = Args Driver.CompileOptions FormattedString.ColorsNeeded Diagnostic.Settings.Settings

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
                                , ("sir", Driver.SIR)
                                , ("solved-sir", Driver.SolvedSIR)
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
            <*> (Diagnostic.Settings.Settings <$> option
                    (reader_from_map
                        [ ("ascii", Diagnostic.Settings.ASCII)
                        , ("unicode", Diagnostic.Settings.Unicode)
                        , ("json", Diagnostic.Settings.JSON)
                        ])
                    (long "diagnostic-format"
                        <> metavar "FORMAT"
                        <> value Diagnostic.Settings.Unicode
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
